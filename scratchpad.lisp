(in-package :cl-user)
(defpackage scratchpad
  (:use :cl :stumpwm)
  (:export
   scratchpad-toggle
   *default-ratio*))
(in-package :scratchpad)

(defvar *default-ratio* 1/2)

(defun scratchpad-toggle (cmd props &key (ratio *default-ratio*) (direction '(:below :right))
                                  (all-groups *run-or-raise-all-groups*)
                                  (all-screens *run-or-raise-all-screens*))
  "Display a window in the current group, splitting or focusing.

Direction can be one of: :above :below :left :right
 Or a list with the sides that may be chosen.  The shorter edge decide the split.
"
  (let* ((group (current-group))
         (matches (stumpwm::find-matching-windows props all-groups all-screens))
         (cframe (stumpwm::tile-group-current-frame group))
         (cwindow (group-current-window group)))
    (cond ((null matches)
           (run-shell-command cmd))
          (t (let* ((win (car matches))
                    (wframe (stumpwm::window-frame win))
                    (wgroup (window-group win)))
               (labels ((maybe-remove-old-split ()
                          ;; Remove old frame if empty
                          (let* ((head (stumpwm::frame-head wgroup wframe))
                                 (current (stumpwm::tile-group-current-frame wgroup))
                                 (tree (stumpwm::tile-group-frame-head wgroup head)))
                            (when (and (null (stumpwm::frame-window wframe))
                                       (not (atom tree)))
                              (remove-split wgroup))))
                        (decide-direction ()
                          (cond ((listp direction)
                                 (let* ((w (frame-width cframe))
                                        (h (frame-height cframe))
                                        (allowed (if (< w h)
                                                     '(:above :below)
                                                     '(:left :right))))
                                   (or (car (intersection allowed direction)) :below)))
                                (t
                                 direction))))
                 (cond
                   ;; Currently focused on scratchpad
                   ((and (eq cframe wframe)
                         (eq win (stumpwm::frame-window cframe)))
                    (remove-split))
                   ;; Scratchpad is visible, move to it
                   ((and (eq wgroup group) (window-visible-p win))
                    (stumpwm::focus-frame wgroup wframe))
                   ;; Current frame is empty, just display it
                   ((null (stumpwm::frame-window cframe))
                    (move-window-to-group win group)
                    (maybe-remove-old-split))
                   ;; Scratchpad must be displayed
                   (t (let* ((decided-direction (decide-direction))
                             (swapped (member decided-direction '(:above :left)))
                             (dir (if (member decided-direction '(:below :above)) :row :column))
                             (r (if swapped ratio (- 1 ratio)))
                             (old-num (stumpwm::frame-number cframe))
                             (new-num (stumpwm::split-frame group dir r))
                             (target-frame (stumpwm::frame-by-number group (if swapped old-num new-num)))
                             (original-frame (stumpwm::frame-by-number group (if swapped new-num old-num))))
                        (move-window-to-group win group)
                        (maybe-remove-old-split)
                        (when swapped
                          (stumpwm::migrate-frame-windows group target-frame original-frame))
                        (stumpwm::pull-window win target-frame nil)
                        (when cwindow
                          (stumpwm::pull-window cwindow original-frame nil))
                        (stumpwm::focus-frame group target-frame)
                        (stumpwm::sync-all-frame-windows group))))))))))

