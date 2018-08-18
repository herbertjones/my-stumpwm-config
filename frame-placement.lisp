(in-package :stumpwm)

(run-commands "grename Emacs"
              "gnewbg Browser"
              "gnewbg Email"
              "gnewbg Terminal"
              "gnewbg Security")

(clear-window-placement-rules)

(define-frame-preference "Browser"
    (0 t   t :class "Firefox")
  (1 t   t :class "Google-chrome"))

(define-frame-preference "Email"
    (0 t   t :class "Thunderbird"))

(define-frame-preference "Terminal"
    (0 t t :title "tmux - main"))

(define-frame-preference "Security"
    (0 t t :class "keepassxc"))
