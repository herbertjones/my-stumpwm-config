(in-package :hfj)

(when (consp *group-names*)
  (grename (first *group-names*))
  (loop for name in (rest *group-names*)
        do (add-group (current-screen) name)))

(when (consp *frame-preferences*)
  (loop for (name . prefs) in *frame-preferences*
        do (eval `(define-frame-preference ,name ,@prefs))))
