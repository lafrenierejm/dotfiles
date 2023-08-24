(setq package-archives nil)
(setq package-enable-at-startup nil)
(setq straight-repository-branch "master")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (format
          "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
          straight-repository-branch)
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
