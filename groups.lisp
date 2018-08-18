(in-package :stumpwm)

(run-commands "grename Emacs"
              "gnewbg Browser"
              "gnewbg Email"
              "gnewbg Terminal")

(define-frame-preference "Browser"
    (0 t   t :class "Firefox")
  (1 t   t :class "Google-chrome")
  (2 t t :class "keepassxc"))

(define-frame-preference "Email"
    (0 t   t :class "Thunderbird"))

(define-frame-preference "Terminal"
    (0 t t :title "tmux - main"))
