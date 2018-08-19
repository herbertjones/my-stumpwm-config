(in-package :hfj)

(defmacro if-sure (&body body)
  `(when (equal :yes (second (select-from-menu (current-screen)
                                               '(("no" :no) ("yes" :yes))
                                               "Are you sure?")))
     ,@body))

(defcommand session-menu () ()
  (let* ((menu '(("0. cancel" "echo cancelled")
                 ("1. log out" :quit)
                 ("2. switch user" :switch-user)
                 ("3. reboot" :reboot)
                 ("4. reload" "loadrc")
                 ("5. poweroff" :poweroff)))
         (selection (select-from-menu (current-screen) menu "Choose action:")))
    (cond ((null selection)
           nil)
          ((stringp (second selection))
           (run-commands (second selection)))
          (t (case (second selection)
               (:quit
                (if-sure (run-commands "quit")))
               (:switch-user
                (if-sure (run-shell-command "dm-tool switch-to-greeter")))
               (:reboot
                (if-sure (run-shell-command "systemctl reboot")))
               (:poweroff
                (if-sure (run-shell-command "systemctl poweroff")))
               (otherwise
                (run-commands (format nil "echo Unknown selection: ~S" (second selection)))))))))
