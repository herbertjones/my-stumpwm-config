(in-package :hfj)
(ql:quickload "clx-truetype")

(setf *mode-line-position* :bottom)

;; (load-module "app-menu")
;; (load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "wifi")
(load-module "stumptray")
(load-module "ttf-fonts")

(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "Iosevka Light" :subfamily "Regular" :size 10))

(setf *window-format*
      ;; "%m%n%s%50t" ;; Default
      "%m%s%20c")

(setf *screen-mode-line-format*
      ;; (list "%g | " '(:eval (run-shell-command "date" t)))
      ;; "[^B%n^b] %W"  ;; Original
      "%g | %W^> | %c%M%I %d       ")

(setf *mode-line-timeout* 1)

(unless (stumpwm::head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head))
  (run-commands "stumptray"))
