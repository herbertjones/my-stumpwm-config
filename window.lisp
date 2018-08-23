(in-package :hfj)
(export '(get-x
          get-y
          get-width
          get-height
          with-new-window))

(defgeneric get-x (window)
  (:documentation "Generic get x position"))
(defgeneric get-y (window)
  (:documentation "Generic get y position"))
(defgeneric get-width (window)
  (:documentation "Generic get width"))
(defgeneric get-height (window)
  (:documentation "Generic get height position"))

(defmethod get-x ((window xlib:window))
  (xlib:drawable-x window))
(defmethod get-y ((window xlib:window))
  (xlib:drawable-y window))
(defmethod get-width ((window xlib:window))
  (xlib:drawable-width window))
(defmethod get-height ((window xlib:window))
  (xlib:drawable-height window))

(defmethod get-x ((frame stumpwm::frame))
  (frame-x frame))
(defmethod get-y ((frame stumpwm::frame))
  (frame-y frame))
(defmethod get-width ((frame stumpwm::frame))
  (frame-width frame))
(defmethod get-height ((frame stumpwm::frame))
  (frame-height frame))

(defmethod get-x ((window stumpwm::window))
  (window-x window))
(defmethod get-y ((window stumpwm::window))
  (window-y window))
(defmethod get-width ((window stumpwm::window))
  (window-width window))
(defmethod get-height ((window stumpwm::window))
  (window-height window))

(defcommand windowlist-all () ()
  (let* ((windows (sort (copy-list (stumpwm::all-windows)) #'string-lessp :key #'stumpwm::window-name))
         (window (stumpwm::select-window-from-menu windows "%12c: %50t")))
    (when window
      (stumpwm::focus-all window))))

(defcommand iresize-hfj () ()
  (ratwarp 0 0)
  (run-commands "iresize"))

(defun run-and-act-on-new-window (cmd props timeout function)
  "Run a command, setup a handler to apply a function to the new window once it's open."
  (let* (focus-window-handler
         timeout-handler
         (timer (run-with-timer timeout nil
                                #'(lambda ()
                                    ;; Remove hooks after period of time should something go wrong.
                                    (when focus-window-handler
                                      (remove-hook *focus-window-hook* focus-window-handler))
                                    (when new-window-handler
                                      (remove-hook *new-window-hook* new-window-handler))))))
    (setf new-window-handler
          #'(lambda (new-window)
              (when (apply 'stumpwm::window-matches-properties-p new-window props)
                (remove-hook *new-window-hook* new-window-handler)
                (setf new-window-handler nil)
                (setf focus-window-handler
                      #'(lambda (focused-window last-focused-window)
                          (declare (ignore last-focused-window))
                          (when (eq new-window focused-window)
                            (remove-hook *focus-window-hook* focus-window-handler)
                            (setf focus-window-handler nil)
                            (cancel-timer timer)
                            (funcall function new-window))))
                (add-hook *focus-window-hook* focus-window-handler))))
    (add-hook *new-window-hook* new-window-handler)
    (run-shell-command cmd)))

(defmacro with-new-window ((window cmd &key properties (timeout 30))
                           &body body)
  "Execute command, on next new window matching properties, run the body.  If no
properties given, next new window will be acted on."
  `(run-and-act-on-new-window ,cmd ,properties ,timeout
                              #'(lambda (,window)
                                  ,@body)))
