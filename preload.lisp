(in-package :hfj)
(export '(after-load-conf))

(ql:quickload "xembed")

(defparameter *load-hooks* '())

(defun add-local-hook (name f)
  "Add a hook for running at the end of a local file."
  (push (list name f) *load-hooks*))

(defun run-local-hooks (name)
  (dolist (l (reverse *load-hooks*))
    (let ((local-name (first l))
          (f (second l)))
      (when (string= name local-name)
        (funcall f)))))

(defmacro after-load-conf ((name) &body body)
  `(add-local-hook ,name
                   #'(lambda ()
                       (eval ',@body))))
(defun load-conf (name)
  "Load a config file in the *config-path*."
  (load (uiop:subpathname* *config-path* name))
  (run-local-hooks name))
