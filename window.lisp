(in-package :hfj)

(defcommand windowlist-all () ()
  (let* ((windows (sort (copy-list (stumpwm::all-windows)) #'string-lessp :key #'stumpwm::window-name))
         (window (stumpwm::select-window-from-menu windows "%12c: %50t")))
    (when window
      (stumpwm::focus-all window))))
