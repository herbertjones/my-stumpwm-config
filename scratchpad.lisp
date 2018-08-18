(in-package :stumpwm)

(defun swap-frame-windows (group frame-a frame-b &optional ignored)
  "Swap the contents of two frames."
  (let* ((windows-a (frame-windows group frame-a))
         (windows-b (frame-windows group frame-b)))
    (loop for win in (reverse windows-a)
          when (not (member win ignored))
            do (pull-window win frame-b))
    (loop for win in (reverse windows-b)
          when (not (member win ignored))
            do (pull-window win frame-a))))

(defun scratchpad-toggle (cmd props &optional (ratio 1/2) (direction :right)
                                  (all-groups *run-or-raise-all-groups*)
                                  (all-screens *run-or-raise-all-screens*))
  "Display a window in the current group, splitting or focusing.

Direction can be one of: :top :bottom :left :right"
  (let* ((group (current-group))
         (matches (find-matching-windows props all-groups all-screens))
         (cframe (tile-group-current-frame group)))
    (cond ((null matches)
           (run-shell-command cmd))
          (t (let* ((win (car matches))
                    (wframe (window-frame win))
                    (wgroup (window-group win)))
               (labels ((maybe-remove-old-split ()
                          ;; Remove old frame if empty
                          (let* ((head (frame-head wgroup wframe))
                                 (current (tile-group-current-frame wgroup))
                                 (tree (tile-group-frame-head wgroup head)))
                            (when (and (null (frame-window wframe))
                                       (not (atom tree)))
                              (remove-split wgroup)))))
                 (cond
                   ;; Currently focused on scratchpad
                   ((and (eq cframe wframe)
                         (eq win (frame-window cframe)))
                    (remove-split))
                   ;; Scratchpad is visible, move to it
                   ((and (eq wgroup group) (window-visible-p win))
                    (focus-frame wgroup wframe))
                   ;; Current frame is empty, just display it
                   ((null (frame-window cframe))
                    (move-window-to-group win group)
                    (maybe-remove-old-split))
                   ;; Scratchpad must be displayed
                   (t (let* ((swapped (member direction '(:top :left)))
                             (dir (if (member direction '(:bottom :top)) :row :column))
                             (r (if swapped ratio (- 1 ratio)))
                             (old-num (frame-number cframe))
                             (new-num (split-frame group dir r))
                             (target-frame (frame-by-number group (if swapped old-num new-num))))
                        (when swapped
                          (swap-frame-windows group (frame-by-number group new-num) target-frame (list win)))
                        (move-window-to-group win group)
                        (pull-window win target-frame)
                        (focus-frame group target-frame)
                        (maybe-remove-old-split))))))))))

