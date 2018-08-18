(in-package :stumpwm)

(defcommand windowlist-all () ()
  (let* ((windows (sort (copy-list (all-windows)) #'string-lessp :key #'window-name))
         (window (select-window-from-menu windows "%50t" "Select window:")))
    (when window
      (focus-all window))))
