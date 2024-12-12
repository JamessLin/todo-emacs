
(require 'cl-lib)
(require 'ansi-color)

(defface todo-directory-face
  '((t (:foreground "#5F87AF" :weight bold :height 1.1)))
  "Face for displaying directory names in TODO list.")

(defface todo-filename-face
  '((t (:foreground "#87AF5F" :weight semi-bold)))
  "Face for displaying file names in TODO list.")

(defface todo-line-number-face
  '((t (:foreground "#D75F5F")))
  "Face for displaying line numbers in TODO list.")

(defface todo-text-face
  '((t (:foreground "#FFFFFF")))
  "Face for displaying TODO text.")

(defun todo-search-in-file (file)
  "Search for TODO comments in a single file."
  (when (and (file-readable-p file)
             (not (file-directory-p file)))
    (with-temp-buffer
      (condition-case nil
          (progn
            (insert-file-contents file)
            (let (todos)
              (goto-char (point-min))
              (while (re-search-forward "// TODO: \\(.*\\)" nil t)
                (let ((line (line-number-at-pos))
                      (todo-text (match-string 1)))
                  (push (list file line todo-text) todos)))
              (nreverse todos)))
        (error
         (message "Error processing file: %s" file)
         nil)))))

(defun todo-search-in-directory (dir)
  "Search for TODO comments in all files under a directory."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively
                dir
                "\\.\\(cpp\\|h\\|py\\|el\\|js\\|c\\|rs\\)"
                t
                t
                )))
    (cl-remove-if
     #'null
     (cl-mapcan #'todo-search-in-file files))))

(defun todo-show-todos ()
  "Display all TODOs found in the current directory."
  (interactive)
  (let ((todos (todo-search-in-directory default-directory)))
    (if todos
        (with-current-buffer (get-buffer-create "*TODOs*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (todo-mode)


            (let ((grouped-todos (todo-group-by-directory todos)))
              (dolist (dir-group grouped-todos)
                (let ((directory (car dir-group))
                      (dir-todos (cdr dir-group)))


                  (insert (propertize
                           (format "\n📁 Directory: %s\n"
                                   (abbreviate-file-name directory))
                           'face 'todo-directory-face))
                  (insert (propertize
                           (make-string 40 ?─)
                           'face 'todo-directory-face)
                          "\n")


                  (dolist (todo dir-todos)
                    (let* ((file (car todo))
                           (line (cadr todo))
                           (todo-text (caddr todo))
                           (filename (file-name-nondirectory file))
                           (start (point)))
                      (insert
                       (propertize (format "  • %s" filename)
                                   'face 'todo-filename-face)
                       (propertize (format " [Line %d]\n" line)
                                   'face 'todo-line-number-face)
                       (propertize (format "    %s\n" todo-text)
                                   'face 'todo-text-face))


                      (add-text-properties
                       start
                       (point)
                       `(todo-file ,file
                         todo-line ,line
                         mouse-face highlight
                         help-echo ,(format "Open %s at line %d" file line)))))))

              (goto-char (point-min))
              (display-buffer (current-buffer)))))
      (message "No TODOs found!"))))

(defun todo-group-by-directory (todos)
  "Group TODOS by their directory."
  (let ((directory-groups (make-hash-table :test 'equal)))
    (dolist (todo todos)
      (let* ((file (car todo))
             (directory (file-name-directory file)))
        (push todo (gethash directory directory-groups))))
    (let (result)
      (maphash (lambda (dir todos)
                 (push (cons dir (nreverse todos)) result))
               directory-groups)
      (sort result (lambda (a b)
                     (string< (car a) (car b)))))))

(defun todo-visit-todo-at-point ()
  "Visit the file and line of the TODO at point."
  (interactive)
  (let ((file (get-text-property (point) 'todo-file))
        (line (get-text-property (point) 'todo-line)))
    (when (and file line)
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter-top-bottom))))

(define-derived-mode todo-mode special-mode "TODOs"
  "Major mode for displaying TODO list."
  (setq buffer-read-only t)
  (use-local-map todo-mode-map)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (_ignore-auto _noconfirm) (todo-show-todos))))

(define-key todo-mode-map (kbd "RET") 'todo-visit-todo-at-point)

(global-set-key (kbd "C-c t") 'todo-show-todos)

(provide 'todo)
