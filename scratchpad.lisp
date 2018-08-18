(in-package :stumpwm)

;;; TODO: Add :top :bottom :left :right directions
;;;       See: migrate-frame-windows, neighbour
(defun scratchpad-toggle (cmd props &optional (ratio 1/2) (direction :row)
                                  (all-groups *run-or-raise-all-groups*)
                                  (all-screens *run-or-raise-all-screens*))
  "Display a window in the current group, splitting or focusing."
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
                          (when (null (frame-window wframe)) ;; TODO and group has more than one frame
                            (remove-split wgroup))))
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
                   (t
                    (let ((new-num (split-frame group direction ratio)))
                      (move-window-to-group win group)
                      (pull-window win (frame-by-number group new-num))
                      (focus-frame group (frame-by-number group new-num))
                      (maybe-remove-old-split))))))))))

