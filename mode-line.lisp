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


(defun only-float-windows (windows)
  "Mirror of only-tile-windows."
  (remove-if-not (lambda (w)
                   (typep w 'stumpwm::float-window))
                 windows))

(defun group-float-windows (group)
  "Mirror of group-tile-windows."
  (only-float-windows (stumpwm::group-windows group)))

(defun mode-line-hidden-windows (ml)
  (let* ((group (stumpwm::mode-line-current-group ml))
         (current-frame (stumpwm::tile-group-current-frame group))
         (all (concatenate 'list
                           (group-float-windows group)
                           (stumpwm::frame-windows group current-frame))))
    (format nil "~{~a~^ | ~}"
            (mapcar (lambda (w)
                      (let ((str (stumpwm::format-expand *window-formatters*
                                                         *window-format*
                                                         w)))
                        (cond ((eq w (stumpwm::current-window))
                               (stumpwm::fmt-highlight str))
                              (t str))))
                    (stumpwm::sort1 all #'< :key #'window-number)))))

(add-screen-mode-line-formatter #\V #'mode-line-hidden-windows)

(setf *window-format* "%m%s%n.%8c - %20t")

(defun has-battery-p ()
  (or (probe-file "/sys/class/power_supply/BAT0")
      (probe-file "/sys/class/power_supply/BAT1")))

(defun has-wireless-p ()
  (loop
    for path in (directory #P"/sys/class/net/*")
      thereis (probe-file (merge-pathnames (make-pathname :directory '(:relative "wireless"))
                                           path))))

(let ((battery (and (has-battery-p) "BAT: %B"))
      (groups "%g")
      (sep " | ")
      (align-right "^>")
      (hidden-windows "%V")
      (cpu "%c")
      (cpu-bar "%C")
      (cpu-temp "%t")
      (cpu-freq "%f")
      (mem "%M")
      (wifi (and (has-wireless-p) "%I"))
      (date "%d")
      (_ " ")
      (right-padding (make-string 7 :initial-element #\Space)))
  (let ((left (list groups sep hidden-windows))
        (right (list battery (and battery _) cpu mem wifi _ date right-padding)))
    (setf *screen-mode-line-format*
          (list left align-right sep right))))

(setf *mode-line-timeout* 1)

(unless (stumpwm::head-mode-line (current-head))
  (toggle-mode-line (current-screen) (current-head))
  (run-commands "stumptray"))
