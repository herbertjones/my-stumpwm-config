(in-package :hfj)

(defvar *group-setup* nil)
(unless *group-setup*
  (run-commands "grename Emacs"
                "gnewbg Browser"
                "gnewbg Terminal"
                "gnewbg Email")
  (setf *group-setup* t))

(define-frame-preference "Browser"
    (0 t   t :class "Firefox")
  (1 t   t :class "Google-chrome")
  (2 t t :class "keepassxc"))

(define-frame-preference "Email"
    (0 t   t :class "Thunderbird"))

(define-frame-preference "Terminal"
    (0 t t :title "tmux - main"))
