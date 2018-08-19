(in-package :cl-user)
(defpackage hfj.mode-line
  (:use :cl :hfj :stumpwm))
(in-package :hfj.mode-line)

(ql:quickload "clx-truetype")

(setf *mode-line-position* :bottom)

;; (load-module "app-menu")
(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "wifi")
(load-module "stumptray")
(load-module "ttf-fonts")

(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "Iosevka Light" :subfamily "Regular" :size 10))

(setf *window-format* "%m%s%n.%20c")

(let ((battery "BAT: %B")
      (groups "%g")
      (sep " | ")
      (align-right "^>")
      (windows "%W")
      (cpu "%c")
      (cpu-bar "%C")
      (cpu-temp "%t")
      (cpu-freq "%f")
      (mem "%M")
      (wifi "%I")
      (date "%d")
      (_ " ")
      (right-padding (make-string 7 :initial-element #\Space)))
  (let ((left (list groups sep windows))
        (right (list battery _ cpu mem wifi _ date right-padding)))
    (setf *screen-mode-line-format*
          (list left align-right sep right))))

(setf *mode-line-timeout* 1)

(unless (stumpwm::head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head))
  (run-commands "stumptray"))
