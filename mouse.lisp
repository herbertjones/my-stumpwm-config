(in-package :cl-user)
(defpackage mouse-follow
  (:use :cl :stumpwm :hfj)
  (:export
   #:enable
   #:disable))
(in-package :mouse-follow)

(setf *mouse-follow-policy* :follow)
(setf *mouse-focus-policy* :sloppy)
