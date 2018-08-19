(in-package :cl-user)
(defpackage scratchpad
  (:use :cl :stumpwm)
  (:export
   scratchpad-toggle))
(in-package :scratchpad)

(defun swap-frame-windows (group frame-a frame-b &optional ignored)
  "Swap the contents of two frames."
  (let* ((windows-a (stumpwm::frame-windows group frame-a))
         (windows-b (stumpwm::frame-windows group frame-b)))
    (loop for win in (reverse windows-a)
          when (not (member win ignored))
            do (stumpwm::pull-window win frame-b))
    (loop for win in (reverse windows-b)
          when (not (member win ignored))
            do (stumpwm::pull-window win frame-a))))

(defun scratchpad-toggle (cmd props &key (ratio 1/2) (direction '(:bottom :right))
                                  (all-groups *run-or-raise-all-groups*)
                                  (all-screens *run-or-raise-all-screens*))
  "Display a window in the current group, splitting or focusing.

Direction can be one of: :top :bottom :left :right
 Or a list with the sides that may be chosen.  The shorter edge decide the split.
"
  (let* ((group (current-group))
         (matches (stumpwm::find-matching-windows props all-groups all-screens))
         (cframe (stumpwm::tile-group-current-frame group)))
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
                                                     '(:top :bottom)
                                                     '(:left :right))))
                                   (or (car (intersection allowed direction)) :bottom)))
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
                             (swapped (member decided-direction '(:top :left)))
                             (dir (if (member decided-direction '(:bottom :top)) :row :column))
                             (r (if swapped ratio (- 1 ratio)))
                             (old-num (stumpwm::frame-number cframe))
                             (new-num (stumpwm::split-frame group dir r))
                             (target-frame (stumpwm::frame-by-number group (if swapped old-num new-num))))
                        (when swapped
                          (swap-frame-windows group (stumpwm::frame-by-number group new-num) target-frame (list win)))
                        (move-window-to-group win group)
                        (stumpwm::pull-window win target-frame)
                        (stumpwm::focus-frame group target-frame)
                        (maybe-remove-old-split))))))))))

