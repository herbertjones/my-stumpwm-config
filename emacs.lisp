(in-package :hfj)

(ql:quickload "swank")
(swank-loader:init)

(def-menu-command start-swank () () (*default-stumpwm-menu* "Start Swank")
  (swank:create-server :port 4004
                       :style swank:*communication-style*
                       :dont-close t))

(def-menu-command stop-swank () () (*default-stumpwm-menu* "Stop Swank")
  (swank:stop-server 4004))
