(in-package :hfj)

(clear-window-placement-rules)

;; Clear hooks I use on restart
(remove-all-hooks *focus-group-hook*)
(remove-all-hooks *focus-window-hook*)
(remove-all-hooks *focus-frame-hook*)
(remove-all-hooks *split-frame-hook*)
(remove-all-hooks *remove-split-hook*)
