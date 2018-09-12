(in-package :cl-user)
(defpackage mouse-follow
  (:use :cl :stumpwm :hfj :alexandria)
  (:export
   *mouse-follow-banish-x-offset*
   *mouse-follow-banish-y-offset*
   disable
   enable))
(in-package :mouse-follow)

(setf *mouse-focus-policy* :click)

(defvar *mouse-follow-banish-x-offset* -15
  "Negative values mean position from the right.")

(defvar *mouse-follow-banish-y-offset* -15
  "Negative values mean position from the bottom.")

(defstruct last-change mouse-x mouse-y window frame)

(defparameter *last-mouse-position* nil)

(defun wait-for-stable-change (window frame)
  "Set a timer that will determine if the current change isn't replaced by
another.  If it hasn't, then the pointer will be moved."
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position *display*)
    (let ((change (make-last-change :mouse-x mouse-x
                                    :mouse-y mouse-y
                                    :window window
                                    :frame frame)))
      (setq *last-mouse-position* change)
      (run-with-timer 0.15 nil #'wait-for-stable-change-timeout change))))

(defun wait-for-stable-change-timeout (change)
  "Test that the timeout's change is the last change made and that the mouse
hasn't been moved by other means since then."
  (when (eq change *last-mouse-position*)
    (multiple-value-bind (mouse-x mouse-y)
        (xlib:global-pointer-position *display*)
      (when (and (eql (last-change-mouse-x change) mouse-x)
                 (eql (last-change-mouse-y change) mouse-y))
        (cond ((and (last-change-window change)
                    (not (mouse-inside-window-p (last-change-window change))))
               (mouse-banish-window (last-change-window change)))
              ((and (last-change-frame change)
                    (not (mouse-inside-frame-p (last-change-frame change))))
               (mouse-banish-frame (last-change-frame change))))))))

(defun mouse-banish-frame (frame)
  "Banish mouse to corner of frame"
  (let* ((group (current-group))
         (min-x (frame-x frame))
         (max-x (+ min-x (frame-width frame)))
         (new-x (if (minusp *mouse-follow-banish-x-offset*)
                    (+ max-x *mouse-follow-banish-x-offset*)
                    (+ min-x *mouse-follow-banish-x-offset*)))
         (min-y (stumpwm::frame-display-y group frame))
         (max-y (+ min-y (stumpwm::frame-display-height group frame)))
         (new-y (if (minusp *mouse-follow-banish-y-offset*)
                    (+ max-y *mouse-follow-banish-y-offset*)
                    (+ min-y *mouse-follow-banish-y-offset*))))
    (ratwarp (clamp new-x min-x max-x)
             (clamp new-y min-y max-y))))

(defun mouse-banish-window (window)
  "Move mouse pointer to edge of a window."
  (let* ((min-x (xlib:drawable-x (window-parent window)))
         (max-x (+ min-x (xlib:drawable-width (window-parent window))))
         (new-x (if (minusp *mouse-follow-banish-x-offset*)
                    (+ max-x *mouse-follow-banish-x-offset*)
                    (+ min-x *mouse-follow-banish-x-offset*)))
         (min-y (xlib:drawable-y (window-parent window)))
         (max-y (+ min-y (xlib:drawable-height (window-parent window))))
         (new-y (if (minusp *mouse-follow-banish-y-offset*)
                    (+ max-y *mouse-follow-banish-y-offset*)
                    (+ min-y *mouse-follow-banish-y-offset*))))
    (ratwarp (clamp new-x min-x max-x)
             (clamp new-y min-y max-y))))

;; Check

(defun mouse-inside-frame-p (frame)
  "Determine if mouse already inside frame."
  (multiple-value-bind (mouse-x mouse-y)
      (xlib:global-pointer-position  *display*)
    (let* ((group (current-group))
           (min-x (frame-x frame))
           (min-y (stumpwm::frame-display-y group frame))
           (max-x (+ min-x (frame-width frame)))
           (max-y (+ min-y (stumpwm::frame-display-height group frame))))
      (and (<= min-x mouse-x max-x)
           (<= min-y mouse-y max-y)))))

(defgeneric mouse-inside-window-p (window)
  (:documentation "Determine if mouse already inside window.")
  (:method ((window stumpwm::float-window))
    (multiple-value-bind (mouse-x mouse-y)
        (xlib:global-pointer-position *display*)
      (let* ((leniency-offset 2)
             (x (xlib:drawable-x (window-parent window)))
             (w (xlib:drawable-width (window-parent window)))
             (min-x (- x stumpwm::*float-window-border* leniency-offset))
             (max-x (+ x w stumpwm::*float-window-border* leniency-offset))
             (y (xlib:drawable-y (window-parent window)))
             (h (xlib:drawable-height (window-parent window)))
             (min-y (- y stumpwm::*float-window-title-height* leniency-offset))
             (max-y (+ y h stumpwm::*float-window-border* leniency-offset)))
        (and (<= min-x mouse-x max-x)
             (<= min-y mouse-y max-y)))))
  (:method ((window stumpwm::tile-window))
    (let ((frame (stumpwm::window-frame window)))
      (mouse-inside-frame-p frame))))

;; Handlers

(defun mouse-handle-focus-frame (current-frame last-frame)
  "Move mouse when moving frames."
  (declare (ignore last-frame))
  (wait-for-stable-change nil current-frame))

(defun mouse-handle-split-frame (old-frame first-frame second-frame)
  "Reposition the mouse when a frame is created."
  (declare (ignore old-frame second-frame))
  (wait-for-stable-change (current-window) first-frame))

(defun mouse-handle-remove-split (current-frame old-frame)
  "Reposition the mouse when a frame is removed."
  (declare (ignore old-frame))
  (wait-for-stable-change nil current-frame))

(defun mouse-handle-focus-window (current-window last-window)
  "Move mouse for floating windows."
  (declare (ignore last-window))
  (wait-for-stable-change current-window nil))

(defun mouse-handle-focus-group (group old-group)
  "Disable sloppy pointer when switching groups to prevent floating windows from
getting stuck and banish to last window or frame."
  (declare (ignore old-group))
  (wait-for-stable-change (current-window) (stumpwm::tile-group-current-frame group)))

;; Configuration

(defun disable ()
  "Disable mouse follows window mode."
  (remove-hook *focus-frame-hook* #'mouse-handle-focus-frame)
  (remove-hook *split-frame-hook* #'mouse-handle-split-frame)
  (remove-hook *remove-split-hook* #'mouse-handle-remove-split)
  (remove-hook *focus-window-hook* #'mouse-handle-focus-window)
  (remove-hook *focus-group-hook* #'mouse-handle-focus-group))

(defun enable ()
  "Enable mouse follows window mode."
  (disable)
  (add-hook *focus-frame-hook* #'mouse-handle-focus-frame)
  (add-hook *split-frame-hook* #'mouse-handle-split-frame)
  (add-hook *remove-split-hook* #'mouse-handle-remove-split)
  (add-hook *focus-window-hook* #'mouse-handle-focus-window)
  (add-hook *focus-group-hook* #'mouse-handle-focus-group))

(enable)
