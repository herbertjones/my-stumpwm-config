(in-package :hfj)
(export '(get-x
          get-y
          get-width
          get-height))

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
