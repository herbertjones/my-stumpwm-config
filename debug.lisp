(in-package :hfj)
(export 'my-debug)

(defun my-debug (&rest data)
  (with-open-file (stream (uiop:subpathname* (user-homedir-pathname) "tmp/stumpwm.txt")
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~&~A" (car data))
    (loop for item in (cdr data)
          do (format stream " ~A" item))
    (terpri stream)))
