(in-package :hfj)

(ql:quickload "swank")
(swank-loader:init)

(def-menu-command start-swank (port) ((:number "Port: ")) (*default-stumpwm-menu* "Start Swank")
  (when port
    (swank:create-server :port port
                         :style swank:*communication-style*
                         :dont-close nil)))

(def-menu-command stop-swank () () (*default-stumpwm-menu* "Stop Swank")
  (swank:stop-server 4004))
