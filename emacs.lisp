(in-package :hfj)

(ql:quickload "swank")
(swank-loader:init)

(defcommand start-swank () ()
  (swank:create-server :port 4004
                       :style swank:*communication-style*
                       :dont-close t))

(defcommand stop-swank () ()
  (swank:stop-server 4004))
