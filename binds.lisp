(in-package :hfj)

(defun alist-define-keys (map alist)
  "define key using alist."
  (loop for (key . command) in alist
        do (define-key map (kbd key) command)))

(defmacro create-map (var key &key (on *top-map*))
  `(progn
     (defparameter ,var (make-sparse-keymap))
     (define-key ,on (kbd ,key) ',var)
     ,var))

(alist-define-keys *top-map*
                   '(("s-h" . "move-focus left")
                     ("s-j" . "move-focus down")
                     ("s-k" . "move-focus up")
                     ("s-l" . "move-focus right")

                     ("s-H" . "move-window left")
                     ("s-J" . "move-window down")
                     ("s-K" . "move-window up")
                     ("s-L" . "move-window right")

                     ("s-M-H" . "exchange-direction left")
                     ("s-M-J" . "exchange-direction down")
                     ("s-M-K" . "exchange-direction up")
                     ("s-M-L" . "exchange-direction right")

                     ("s-;" . "colon")

                     ("s-b" . "fullscreen")

                     ("s-TAB" . "grouplist")

                     ("s-m" . "banish window")

                     ("s-n" . "next-in-frame")
                     ("s-p" . "prev-in-frame")

                     ("s-q" . "session-menu")

                     ("s-o" . "show-menu")

                     ("s-x" . "run-shell-command")

                     ("s-z" . "mark")
                     ("s-Z" . "gmove-marked")

                     ("s-`" . "scratchpad-float yakyak yakyak left")

                     ("XF86ScreenSaver" . "lock")

                     ("S-XF86MonBrightnessUp" . "xbacklight =100%")
                     ("S-XF86MonBrightnessDown" . "xbacklight =3%")
                     ("XF86MonBrightnessUp" . "xbacklight +5%")
                     ("XF86MonBrightnessDown" . "xbacklight -5%")

                     ("XF86AudioRaiseVolume" . "amixer -c 0 sset Master 1+")
                     ("XF86AudioLowerVolume" . "amixer -c 0 sset Master 1-")

                     ("XF86AudioMute" . "amixer sset Master,0 toggle")

                     ("S-XF86AudioRaiseVolume" . "amixer -c 0 sset Capture 1+")
                     ("S-XF86AudioLowerVolume" . "amixer -c 0 sset Capture 1-")

                     ("XF86AudioMicMute" . "amixer sset Capture,0 toggle")))
(loop for i from 1 to 9
      do (let ((key (kbd (format nil "s-~A" i)))
               (action (format nil "fselect ~A" (1- i))))
           (define-key *top-map* key action)))

(loop for ch in '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\()
      and i from 0 to 9
      do (let ((key (kbd (format nil "s-~A" ch)))
               (action (format nil "gmove ~A" i)))
           (define-key *top-map* key action)))

(alist-define-keys (create-map *frame-map* "s-f")
                   '(("f" . "frame-windowlist")
                     ("s-f" . "fother")
                     ("n" . "next-in-frame")
                     ("p" . "prev-in-frame")
                     ("e" . "fclear")
                     ("m" . "only")
                     ("=" . "balance-frames")))
(loop for i from 0 to 9
      do (let ((key (kbd (format nil "~A" i)))
               (action (format nil "fselect ~A" i)))
           (define-key *frame-map* key action)))

(alist-define-keys (create-map *window-map* "s-w")
                   '(("h" . "move-focus left")
                     ("j" . "move-focus down")
                     ("k" . "move-focus up")
                     ("l" . "move-focus right")

                     ("q" . "delete")
                     ("Q" . "kill")

                     ("n" . "pull-hidden-next")
                     ("p" . "pull-hidden-previous")

                     ("w" . "windowlist-all")
                     ("s-w" . "switch-to-previous-window")

                     ("g" . "gmove")
                     ("m" . "only")

                     ("t" . "mark")

                     ("s" . "vsplit")
                     ("v" . "hsplit")
                     ("d" . "remove")
                     ("r" . "iresize-hfj")))
(loop for i from 0 to 9
      do (let ((key (kbd (write-to-string i)))
               (action (format nil "select-window-by-number ~A" i)))
           (define-key *window-map* key action)))

(alist-define-keys (create-map *window-move-map* "m" :on *window-map*)
                   '(("h" . "move-window left")
                     ("j" . "move-window down")
                     ("k" . "move-window up")
                     ("l" . "move-window right")))

(alist-define-keys (create-map *window-transpose-map* "x" :on *window-map*)
                   '(("h" . "exchange-direction left")
                     ("j" . "exchange-direction down")
                     ("k" . "exchange-direction up")
                     ("l" . "exchange-direction right")))

(alist-define-keys (create-map *group-map* "s-g")
                   '(("g" . "grouplist")
                     ("s-g" . "gother")

                     ("n" . "gnext")
                     ("N" . "gnext-with-window")

                     ("p" . "gprev")
                     ("P" . "gprev-with-window")

                     ("t" . "gmove-marked")

                     ("c" . "gnew")
                     ("q" . "gkill")
                     ("r" . "grename")))
(loop for i from 0 to 9
      do (let ((key (kbd (format nil "~A" i)))
               (action (format nil "gselect ~A" i)))
           (define-key *group-map* key action)))

(alist-define-keys (create-map *systray-map* "s-s")
                   '(("j" . "stumptray-toggle-hidden-icons-visibility")
                     ("k" . "systray-toggle-icon-hiding")

                     ("h" . "systray-selection-left")
                     ("l" . "systray-selection-right")

                     ("H" . "systray-move-icon-left")
                     ("L" . "systray-move-icon-right")

                     ("s" . "stumptray")))

(alist-define-keys (create-map *applications-map* "s-a")
                   '(("f" . "run-firefox")
                     ("c" . "run-chrome")
                     ("k" . "run-keepassxc")
                     ("t" . "run-named-terminal main")
                     ("m" . "run-thunderbird")
                     ("y" . "run-yakyak")))

(alist-define-keys (create-map *applications-emacs* "e" :on *applications-map*)
                   '(("e" . "display-named-emacs main n")
                     ("E" . "display-named-emacs main y")))

(loop for c across "abcdfghijklmnopqrstuvwxyz0123456789"
      do (let ((key (kbd (string c)))
               (action (format nil "display-named-emacs ~C n" c)))
           (define-key *applications-emacs* key action)))
(loop for c across "ABCDFGHIJKLMNOPQRSTUVWXYZ"
      do (let ((key (kbd (string c)))
               (action (format nil "display-named-emacs ~C y" (char-downcase c))))
           (define-key *applications-emacs* key action)))

(define-key stumpwm:*menu-map* (kbd "TAB") 'menu-down)
(define-key stumpwm:*menu-map* (kbd "M-TAB") 'menu-down)
(define-key stumpwm:*menu-map* (kbd "s-TAB") 'menu-up)
