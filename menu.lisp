(in-package :hfj)
(export '(add-menu-item))

(defparameter *default-menu-name* "Menu")

(defparameter *default-menu*
  '(("Apps" (:submenu *default-apps-menu*))
    ("Util" (:submenu *default-util-menu*))
    ("StumpWM" (:submenu *default-stumpwm-menu*))))

(defparameter *default-apps-menu* nil)

(defparameter *default-util-menu* nil)

(defparameter *default-stumpwm-menu*
  '(("Restart hard" restart-hard)
    ("Restart soft" restart-soft)
    ("Reload RC" loadrc)))

(defun data-is-submenu-p (data)
  (consp data))

(defun menu-item-name (row)
  (first row))

(defun menu-item-data (row)
  (second row))

(defun append-menu-names (title menu)
  "Convert menu of two items per row two three, where first is a pretty title."
  (let* ((longest-size (reduce #'(lambda (val row)
                                   (max val (length (menu-item-name row))))
                               menu
                               :initial-value 0))
         (has-submenu (loop for row in menu
                              thereis (data-is-submenu-p (menu-item-data row))))
         ;; When apps have " ->" postfix, commands need "   "
         (submenu-postfix " ->")
         (command-postfix (if has-submenu
                              (make-string (length submenu-postfix)
                                           :initial-element #\Space)
                              ""))
         (longest-with-postfix (+ (length command-postfix)
                                  longest-size))
         (title-size (length title))
         (prefix (if (< longest-with-postfix title-size)
                     (make-string (- title-size longest-with-postfix)
                                  :initial-element #\Space)
                     "")))
    (labels ((cons-nice-name (row)
               (let ((title (menu-item-name row))
                     (content (menu-item-data row)))
                 (cond
                   ;; Submenu
                   ((data-is-submenu-p content)
                    (cons (concat prefix
                                  (make-string (- longest-size (length title))
                                               :initial-element #\Space)
                                  title
                                  submenu-postfix)
                          row))
                   ;; Command
                   (t (cons (concat prefix
                                    (make-string (- longest-size (length title))
                                                 :initial-element #\Space)
                                    title
                                    command-postfix)
                            row))))))
      (map 'list #'cons-nice-name menu))))

(defun run-submenu (menu &optional path previous-menus selected-entry)
  "Present menu to user."
  (let ((selection
          (let* ((title (str:join "/" (reverse path)))
                 (smenu (sort (append-menu-names title menu)
                              #'(lambda (a b)
                                  (string-lessp (second a)
                                                (second b)))))
                 (pos (or (and selected-entry
                               (position-if #'(lambda (row)
                                                (string-equal selected-entry
                                                              (second row)))
                                            smenu))
                          0)))
            (select-from-menu (current-screen)
                              smenu
                              title
                              pos))))
    (cond
      ;; Ascend to previous
      ((and (null selection)
            previous-menus)
       (run-submenu (first previous-menus)
                    (rest path)
                    (rest previous-menus)
                    (first path)))
      ;; End
      ((null selection)
       nil)
      (t (let ((name (second selection))
               (data (descend-data (third selection))))
           (cond
             ;; Submenu
             ((data-is-submenu-p data)
              (run-submenu data
                           (cons name path)
                           (cons menu previous-menus)))
             ;; CLI command to run
             ((stringp data)
              (run-shell-command data))
             ;; Lisp function to run
             ((symbolp data)
              (run-commands (string data)))
             ;; Unknown
             (t
              (error "Unknown menu data: ~S" selection))))))))

(defun show-menu (&optional (menu *default-menu*) (name *default-menu-name*))
  (run-submenu menu (list name)))

(defun descend-data (data)
  (cond ((and (consp data)
              (eq :submenu (first data)))
         (symbol-value (second data)))
        (t
         data)))

(defun find-menu-row (name menu &key (offset 0))
  "Find menu item from name and return submenu or item or nil."
  (find-if #'(lambda (row)
               (string-equal (elt row offset) name))
           menu))

(defun descend-menu (name menu &key (offset 0))
  "Find menu item from name and return submenu or item or nil."
  (let ((row (find-menu-row name menu offset)))
    (when row
      (descend-data (elt row (+ 1 offset))))))

(defun append-menu-f (menu name data)
  "Find menu item from name and return submenu or item or nil."
  (push (list name data) menu)
  menu)

(defmacro replace-menu-f (menu name data)
  "Find menu item from name and return submenu or item or nil."
  `(progn
     (check-type ,menu cons)
     (check-type ,name string)
     (setf ,menu (delete-if #'(lambda (row)
                                (string-equal ,name
                                              (menu-item-name row)))
                            ,menu))
     (append-menu-f ,menu ,name ,data)))

(defun new-menus (path data)
  (list (if (= 1 (length path))
            (list (first path) data)
            (list (first path) (list (new-menus (rest path) data))))))

(defmacro add-menu-item (menu path f)
  `(cond ((null ,menu)
          (setf ,menu (new-menus ,path ,f)))
         (t
          (setf ,menu (add-menu-item-helper ,menu ,path ,f)))))

(defun add-menu-item-helper (menu path f)
  (check-type menu cons)
  (labels ((add (title)
             (replace-menu-f menu title f)))
    (cond
      ;; Insert here if string
      ((stringp path)
       (add path))
      ;; Insert here if last path
      ((= 1 (length path))
       (add (first path)))
      (t (let ((name (first path))
               (remaining (rest path)))
           (acond
             ;; Descend: Path exists already
             ((find-menu-row name menu)
              (let* ((row it)
                     (raw-data (menu-item-data row))
                     (data (descend-data raw-data)))
                (cond
                  ((and (consp raw-data)
                        (eq :submenu (first raw-data)))
                   ;; Don't change menu, change submenu referenced by symbol
                   (let ((sym-name (second raw-data)))
                     (add-menu-item (symbol-value sym-name) remaining f)
                     menu))
                  ((data-is-submenu-p data)
                   (replace-menu-f menu name
                                   (add-menu-item data remaining f)))
                  (t (replace-menu-f menu name (new-menus remaining f))))))
             ;; Descend: Path is new
             (t (replace-menu-f menu name (new-menus remaining f)))))))))
