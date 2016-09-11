;; Load package-config.el
(load (expand-file-name "package-config.el" user-emacs-directory))

;; Load all elisp files inside config/
(defun load-directory (directory)
  "Recursively load all '.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
	   (fullpath (concat directory "/" path))
	   (isdir (car (cdr element)))
	   (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t)
	     (not ignore-dir))
	(load-directory fullpath))
       ((and (eq isdir nil)
	     (string= (substring path -3) ".el"))
	(load (file-name-sans-extension fullpath)))))))
(load-directory "~/.emacs.d/config")
