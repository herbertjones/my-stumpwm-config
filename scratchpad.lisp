(in-package :cl-user)
(defpackage scratchpad
  (:use :cl :stumpwm)
  (:export #:scratchpad-toggle
           #:*default-ratio*))
(in-package :scratchpad)

(defvar *default-ratio* 1/2)

(defun maybe-remove-old-split (moved-from-group moved-from-frame)
  "Remove old frame if empty."
  (let* ((head (stumpwm::frame-head moved-from-group moved-from-frame))
         (tree (stumpwm::tile-group-frame-head moved-from-group head)))
    (when (and (null (stumpwm::frame-window moved-from-frame))
               (not (atom tree)))
      (remove-split moved-from-group))))

(defun decide-direction (current-frame direction)
  (cond ((listp direction)
         (let* ((w (frame-width current-frame))
                (h (frame-height current-frame))
                (allowed (if (< w h)
                             '(:above :below)
                             '(:left :right))))
           (or (car (intersection allowed direction)) :below)))
        (t
         direction)))

(defun show-scratchpad (direction ratio group current-frame current-window
                    scratchpad-window moved-from-group moved-from-frame)
  (let* ((decided-direction (decide-direction current-frame direction))
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

(defun scratchpad-toggle (cmd props &key (ratio *default-ratio*) (direction '(:below :right))
                                  (all-groups *run-or-raise-all-groups*)
                                  (all-screens *run-or-raise-all-screens*))
  "Display a window in the current group, splitting or focusing.

Direction can be one of: :above :below :left :right
 Or a list with the sides that may be chosen.  The shorter edge decide the split.
"
  (let* ((group (current-group))
         (matches (stumpwm::find-matching-windows props all-groups all-screens))
         (current-frame (stumpwm::tile-group-current-frame group))
         (current-window (group-current-window group)))
    (cond ((null matches)
           (run-shell-command cmd))
          (t (let* ((scratchpad-window (car matches))
                    (moved-from-frame (stumpwm::window-frame scratchpad-window))
                    (moved-from-group (window-group scratchpad-window)))
               (cond
                 ;; Currently focused on scratchpad
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
                 ;; Scratchpad must be displayed
                 (t
                  (show-scratchpad direction
                                   ratio
                                   group
                                   current-frame
                                   current-window
                                   scratchpad-window
                                   moved-from-group
                                   moved-from-frame))))))))

