(in-package :cl-user)
(defpackage mouse-follow
  (:use :cl :stumpwm :hfj)
  (:export
   #:enable
   #:disable))
(in-package :mouse-follow)

(defun temporarilly-disable-sloppy-pointer ()
  "Disable the sloppy pointer for a brief period of time."
  (when (eq *mouse-focus-policy* :sloppy)
    (setf *mouse-focus-policy* :ignore)
    (run-with-timer 0.2 nil #'reenable-sloppy-pointer)))

(defun reenable-sloppy-pointer ()
  (setf *mouse-focus-policy* :sloppy))

(defun mouse-inside-frame-p (frame)
  "Determine if mouse already inside frame."
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position  *display*)
    (let* ((group (current-group))
           (min-x (get-x frame))
           (min-y (stumpwm::frame-display-y group frame))
           (max-x (+ min-x (get-width frame)))
           (max-y (+ min-y (stumpwm::frame-display-height group frame))))
      (and (<= min-x mouse-x max-x)
           (<= min-y mouse-y max-y)))))

(defun banish-frame (frame)
  "Banish mouse to corner of frame"
  (let* ((group (current-group))
         (x-offset 15)
         (y-offset 15)
         (win-x (get-x frame))
         (win-y (stumpwm::frame-display-y group frame))
         (w (get-width frame))
         (h (stumpwm::frame-display-height group frame))
         (x (- (+ win-x w)
               x-offset))
         (y (- (+ win-y h)
               y-offset)))
    (temporarilly-disable-sloppy-pointer)
    (ratwarp x y)))

(defun mouse-inside-window-p (window)
  "Determine if mouse already inside window."
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position *display*)
    (let* ((min-x (get-x window))
           (min-y (get-y window))
           (max-x (+ min-x (get-width window)))
           (max-y (+ min-y (get-height window))))
      (and (<= min-x mouse-x max-x)
           (<= min-y mouse-y max-y)))))

(defun banish-window (window)
  "Banish mouse to corner of a window"
  (let* ((x-offset 15)
         (y-offset 15)
         (win-x (get-x window))
         (win-y (get-y window))
         (w (get-width window))
         (h (get-height window))
         (x (- (+ win-x w)
               x-offset))
         (y (- (+ win-y h)
               y-offset)))
    (temporarilly-disable-sloppy-pointer)
    (ratwarp x y)))

(defun mouse-focus-frame-hook (current-frame last-frame)
  "Move mouse when moving frames."
  (let ((current-window (stumpwm::frame-window current-frame)))
    (cond (current-window
           (unless (mouse-inside-window-p current-window)
             (banish-window current-window)))
          (t
           (unless (eq current-frame last-frame)
             (unless (mouse-inside-frame-p current-frame)
               (banish-frame current-frame)))))))

(defun mouse-split-frame-hook (original-frame a-frame b-frame)
  "Reposition the mouse when a frame is created."
  (banish-frame a-frame))

(defun mouse-remove-split-hook (current-frame old-frame)
  "Reposition the mouse when a frame is removed."
  (banish-frame current-frame))

(defun mouse-focus-window-hook (current-window last-window)
  "Move mouse for floating windows."
  (unless (or (eq current-window last-window)
              (mouse-inside-window-p current-window))
    (banish-window current-window)))

(defun mouse-focus-group-hook (current-group last-group)
  "Disable sloppy pointer when switching groups to prevent floating windows from
getting stuck."
  ;; Without the sloppy pointer disable, floating windows cause rapid window
  ;; switching.  My banish functions aren't called when this happens, so it may
  ;; be a problem with floats and sloppy pointers.
  (temporarilly-disable-sloppy-pointer))

(defvar *original-focus-policy* :ignore)
(defun enable ()
  "Enable mouse follows window mode."
  (setq *original-focus-policy* *mouse-focus-policy*)

  ;; :click, :ignore, :sloppy
  (setf *mouse-focus-policy* :sloppy)

  (add-hook *focus-group-hook* #'mouse-focus-group-hook)
  (add-hook *focus-window-hook* #'mouse-focus-window-hook)
  (add-hook *focus-frame-hook* #'mouse-focus-frame-hook)
  (add-hook *split-frame-hook* #'mouse-split-frame-hook)
  (add-hook *remove-split-hook* #'mouse-remove-split-hook))

(defun disable ()
  "Disable mouse follows window mode."
  (setq *mouse-focus-policy* *original-focus-policy*)

  (remove-hook *focus-group-hook* #'mouse-focus-group-hook)
  (remove-hook *focus-window-hook* #'mouse-focus-window-hook)
  (remove-hook *focus-frame-hook* #'mouse-focus-frame-hook)
  (remove-hook *split-frame-hook* #'mouse-split-frame-hook)
  (remove-hook *remove-split-hook* #'mouse-remove-split-hook))
