(in-package :cl-user)
(defpackage scratchpad
  (:use :cl :stumpwm)
  (:export #:*default-split-ratio*
           #:toggle-split-scratchpad
           #:toggle-floating-scratchpad))
(in-package :scratchpad)

(defvar *default-split-ratio* 1/2)

(defvar *default-float-ratio* 1/2)

(defvar *scratch-floats* '()
  "Alist of names to windows for float scratchpads.")

(defun scratchpad-handle-float-window-destroy (window)
  (setf *scratch-floats* (delete window *scratch-floats*
                                 :key #'cdr)))

(remove-hook *destroy-window-hook* #'scratchpad-handle-float-window-destroy)
(add-hook *destroy-window-hook* #'scratchpad-handle-float-window-destroy)

(defvar *scratch-splits* '()
  "Alist of names to windows for split scratchpads.")

(defun scratchpad-handle-split-window-destroy (window)
  (setf *scratch-splits* (delete window *scratch-splits*
                                 :key #'cdr)))

(remove-hook *destroy-window-hook* #'scratchpad-handle-split-window-destroy)
(add-hook *destroy-window-hook* #'scratchpad-handle-split-window-destroy)


(defun maybe-remove-old-split (moved-from-group moved-from-frame)
  "Remove old frame if empty."
  (let* ((head (stumpwm::frame-head moved-from-group moved-from-frame))
         (tree (stumpwm::tile-group-frame-head moved-from-group head)))
    (when (and (null (stumpwm::frame-window moved-from-frame))
               (not (atom tree)))
      (remove-split moved-from-group))))

(defun flatten-direction (current-frame direction)
  "Convert direction list into single direction."
  (cond ((listp direction)
         (let* ((w (frame-width current-frame))
                (h (frame-height current-frame))
                (allowed (if (< w h)
                             '(:above :below)
                             '(:left :right))))
           (or (first (intersection allowed direction)) :below)))
        (t
         direction)))

(defun scratchpad-split-frame (direction ratio group current-frame current-window
                           scratchpad-window moved-from-group moved-from-frame)
  "Create a new frame and place the scratchpad in it."
  (let* ((decided-direction (flatten-direction current-frame direction))
         (swapped (member decided-direction '(:above :left)))
         (dir (if (member decided-direction '(:below :above)) :row :column))
         (r (if swapped ratio (- 1 ratio)))
         (old-num (stumpwm::frame-number current-frame))
         (new-num (stumpwm::split-frame group dir r))
         (target-frame (stumpwm::frame-by-number group (if swapped old-num new-num)))
         (original-frame (stumpwm::frame-by-number group (if swapped new-num old-num))))
    (move-window-to-group scratchpad-window group)
    (maybe-remove-old-split moved-from-group moved-from-frame)
    (when swapped
      (stumpwm::migrate-frame-windows group target-frame original-frame))
    (stumpwm::pull-window scratchpad-window target-frame nil)
    (when current-window
      (stumpwm::pull-window current-window original-frame nil))
    (stumpwm::focus-frame group target-frame)
    (stumpwm::sync-all-frame-windows group)))

(defun split-toggle (props &key cmd
                         (ratio *default-split-ratio*)
                         (direction '(:below :right))
                         (all-groups *run-or-raise-all-groups*)
                         (all-screens *run-or-raise-all-screens*))
  "Display a window in the current group, splitting or focusing.

Direction can be one of: :above :below :left :right
 Or a list with the sides that may be chosen.  The shorter edge decide the split.

Can optionally create the process if needed, however it will initially display
in the current frame or by whatever rules have been set for new windows.
"
  (let* ((group (current-group))
         (matches (stumpwm::find-matching-windows props all-groups all-screens))
         (current-frame (stumpwm::tile-group-current-frame group))
         (current-window (group-current-window group)))
    (cond ((null matches)
           (when cmd
             (run-shell-command cmd)))
          (t (let* ((scratchpad-window (first matches))
                    (moved-from-frame (stumpwm::window-frame scratchpad-window))
                    (moved-from-group (window-group scratchpad-window)))
               (cond
                 ;; Currently focused on scratchpad; Hide it
                 ((and (eq current-frame moved-from-frame)
                       (eq scratchpad-window (stumpwm::frame-window current-frame)))
                  (remove-split))
                 ;; Scratchpad is visible, move to it
                 ((and (eq moved-from-group group) (window-visible-p scratchpad-window))
                  (stumpwm::focus-frame moved-from-group moved-from-frame))
                 ;; Current frame is empty, just display it
                 ((null (stumpwm::frame-window current-frame))
                  (move-window-to-group scratchpad-window group)
                  (maybe-remove-old-split moved-from-group moved-from-frame))
                 ;; Scratchpad needs a new frame
                 (t (scratchpad-split-frame direction
                                            ratio
                                            group
                                            current-frame
                                            current-window
                                            scratchpad-window
                                            moved-from-group
                                            moved-from-frame))))))))

(defun resize-by-gravity (window gravity ratio)
  (let* ((screen (current-screen))
         (screen-x (stumpwm::screen-x screen))
         (screen-y (stumpwm::screen-y screen))
         (screen-width (stumpwm::screen-width screen))
         (screen-height (stumpwm::screen-height screen))

         (x-min screen-x)
         (x-max (- (+ screen-x screen-width)
                   (* 2 stumpwm::*float-window-border*)))

         (y-min screen-y)
         (y-max (- (+ screen-y screen-height)
                   stumpwm::*float-window-title-height*
                   stumpwm::*float-window-border*))

         (new-x-min x-min)
         (new-x-max x-max)
         (new-y-min y-min)
         (new-y-max y-max)
         (x-width (- x-max x-min))
         (x-ratio-length (- x-width (floor (* x-width ratio))))
         (y-width (- y-max y-min))
         (y-ratio-length (- y-width (floor (* y-width ratio)))))
    (when (member gravity '(:top :top-right :top-left))
      (decf new-y-max y-ratio-length))

    (when (member gravity '(:bottom :bottom-right :bottom-left))
      (incf new-y-min y-ratio-length))

    (when (member gravity '(:left :top-left :bottom-left))
      (decf new-x-max x-ratio-length))

    (when (member gravity '(:right :top-right :bottom-right))
      (incf new-x-min x-ratio-length))

    (when (eq gravity :center)
      (decf new-y-max (floor (/ y-ratio-length 2)))
      (incf new-y-min (floor (/ y-ratio-length 2)))

      (incf new-x-min (floor (/ x-ratio-length 2)))
      (decf new-x-max (floor (/ x-ratio-length 2))))

    (stumpwm::float-window-move-resize window
                                       :x new-x-min :y new-y-min
                                       :width (- new-x-max new-x-min 1)
                                       :height (- new-y-max new-y-min 1))))

(defun toggle-floating-scratchpad (name cmd &key initial-gravity (ratio *default-float-ratio*))
  "Create a named scratchpad."
  (let ((found (member name *scratch-floats*
                       :key #'car
                       :test #'string=)))
    (cond (found
           (let ((window (cdr (car found))))
             (cond ((eq (current-window) window)
                    (stumpwm::hide-window window))
                   ((stumpwm::window-in-current-group-p window)
                    (focus-window window t))
                   (t
                    (move-window-to-group window (current-group))
                    (focus-window window t)))))
          (t
           (hfj:with-new-window (window cmd)
             :init
             (push (cons name window) *scratch-floats*)
             :config
             (stumpwm::float-window window (current-group))
             (cond (initial-gravity
                    (resize-by-gravity window initial-gravity ratio))))))))

(defcommand scratchpad-float (name cmd gravity) ((:string "Name: ")
                                                 (:string "Command: ")
                                                 (:gravity "Side: "))
  (toggle-floating-scratchpad name cmd
                              :initial-gravity gravity))
