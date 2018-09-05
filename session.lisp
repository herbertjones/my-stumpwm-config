(in-package :hfj)

(defmacro if-sure (&body body)
  `(when (equal :yes (second (select-from-menu (current-screen)
                                               '(("no" :no) ("yes" :yes))
                                               "Are you sure?")))
     ,@body))

(flet ((finalize-session ()
         "mktemp and set env var SHUTDOWN_FILE to catch fatal errors in stumpwm and
restart instead of logging out, except when we really want to.

Example:
  export SHUTDOWN_FILE=$(mktemp)
  while [[ -e $SHUTDOWN_FILE ]]; do
      echo \"Starting: $(date)\" >> ~/tmp/stumpwm.log
      /home/herbert/opt/stumpwm/bin/stumpwm >> ~/tmp/stumpwm.log 2>&1
      status=$?
      echo \"Shutdown with: ${status} on $(date)\" >> ~/tmp/stumpwm.log
  done
"
         (when-let* ((shutdown-file (uiop:getenv "SHUTDOWN_FILE"))
                     (file (probe-file shutdown-file)))
           (delete-file file))))
  (defcommand session-menu () ()
    (let* ((menu '(("0. cancel" "echo cancelled")
                   ("1. log out" :quit)
                   ("2. switch user" :switch-user)
                   ("3. reboot" :reboot)
                   ("4. poweroff" :poweroff)))
           (selection (select-from-menu (current-screen) menu "Choose action:")))
      (cond ((null selection)
             nil)
            ((stringp (second selection))
             (run-commands (second selection)))
            (t (case (second selection)
                 (:quit
                  (if-sure
                   (finalize-session)
                   (run-commands "quit")))
                 (:switch-user
                  (if-sure (run-shell-command "dm-tool switch-to-greeter")))
                 (:reboot
                  (if-sure
                   (finalize-session)
                   (run-shell-command "systemctl reboot")))
                 (:poweroff
                  (if-sure
                   (finalize-session)
                   (run-shell-command "systemctl poweroff")))
                 (otherwise
                  (run-commands (format nil "echo Unknown selection: ~S" (second selection))))))))))
