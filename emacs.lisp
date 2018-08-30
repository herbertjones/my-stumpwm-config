(in-package :hfj)

(ql:quickload "swank")
(swank-loader:init)

(defvar *swank-port* nil)

(def-menu-command start-swank (port) ((:number "Port: ")) (*default-stumpwm-menu* "Start Swank")
  (when port
    (when *swank-port*
      (ignore-errors (swank:stop-server *swank-port*)))
    (setf *swank-port* port)
    (swank:create-server :port port
                         :style swank:*communication-style*
                         :dont-close nil)))

(def-menu-command stop-swank () () (*default-stumpwm-menu* "Stop Swank")
  (when *swank-port*
    (ignore-errors (swank:stop-server *swank-port*))))
