(in-package :hfj)

(defvar *group-names* '("Emacs"
                        "Browser"
                        "Terminal"
                        "Email")
  "List of group names to be created.")

(defvar *frame-preferences* '(("Emacs"
                               (0 t t :title "Emacs - main"))
                              ("Browser"
                               (0 t t :class "Firefox")
                               (1 t t :class "Google-chrome")
                               (2 t t :class "keepassxc"))
                              ("Email"
                               (0 t t :class "Thunderbird"))
                              ("Terminal"
                               (0 t t :title "tmux - main")))
  "List of preferences to pass to define-frame-preference.")
