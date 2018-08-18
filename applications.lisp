(in-package :stumpwm)
(ql:quickload "str")

(defun string-escape (str)
  "Escape a string"
  (format nil "~S" str))

(defun named-terminal-title (name)
  "Title for named terminal"
  (format nil "tmux - ~A" name))

(defcommand xbacklight (args) ((:shell "Arguments: "))
  "Run xbacklight"
  (run-shell-command (format nil "xbacklight ~S" args)))

(defcommand amixer (args) ((:shell "Arguments: "))
  "Run amixer"
  (run-shell-command (format nil "amixer ~A" args)))

(defcommand lock () ()
  "Lock session"
  (run-shell-command "dm-tool lock"))

(defcommand run-firefox () ()
  "Run Firefox"
  (run-or-raise "firefox-bin" '(:class "Firefox")))

(defcommand run-named-terminal (name) ((:string "Name: "))
  "Run terminal"
  (let* ((title (named-terminal-title name))
         (args (list
                "st"
                "-t" title ;; Title
                "-f" "Source Code Pro"
                "-e" "/usr/bin/tmux" "new-session" "-AD" "-s" name))
         (cmd (str:join " " (map 'list #'string-escape args))))
    (run-or-raise cmd `(:title ,title))))

(defcommand run-chrome () ()
  "Run Chrome"
  (run-or-raise "firejail google-chrome-stable" '(:class "Google-chrome")))

(defcommand run-thunderbird () ()
  "Run Thunderbird"
  (run-or-raise "thunderbird-bin" '(:class "Thunderbird")))

(defcommand run-keepassxc () ()
  "Run KeepassXC"
  (run-or-raise "keepassxc" '(:class "keepassxc")))

(defcommand toggle-touchpad () ()
  "Enable/Disable touchpad"
  (run-shell-command "toggle-touchpad"))

(defcommand display-named-emacs (name) ((:string "Name: "))
  "Raise emacs frame with given name"
  (let* ((title (format nil "Emacs - ~A" name))
         (name-str (format nil "(name . ~S)" title))
         (title-str (format nil "(title . ~S)" title))
         (form (format nil "(~A ~A)" name-str title-str))
         (args (list "/usr/bin/emacsclient" "-c" "-F" (string-escape form)))
         (cmd (str:join " " args)))
    (run-or-raise cmd `(:title ,title))))

(defcommand show-yakyak () ()
  "Show Yakyak"
  (scratchpad-toggle "yakyak" '(:class "yakyak")))

(defcommand run-yakyak () ()
  "Run Yakyak"
  (run-or-raise "yakyak" '(:class "yakyak")))