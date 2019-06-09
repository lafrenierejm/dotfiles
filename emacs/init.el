;;; init.el --- Setup package management and load lisp/

;;; Commentary:
;; All non-comment code in this file is copyright Joseph M LaFreniere and licensed under [[https://gitlab.com/lafrenierejm/dotfiles/blob/master/LICENSE][an ISC license]] except when otherwise noted.
;; All prose in this file is is copyright Joseph M LaFreniere and licensed under [[https://creativecommons.org/licenses/by/4.0/][CC BY 4.0]] except when otherwise noted.

;;; Code:
;; Bootstrap straight.el.
(defvar bootstrap-version)
(setq straight-repository-branch "develop")

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

;; Install and configure use-package.
(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t) ; Download packages with straight by default.
(setq use-package-always-defer t)       ; Lazy-load packages by default.

;; Load general, which provides convenience wrappers for setting keybindings.
(use-package general
  ;; Load this package eagerly.
  :demand)

;;; Load validate.el
;; [[https://github.com/Malabarba/validate.el][validate.el]] provides functions to perform schema validation.

(use-package validate
  ;; Do not load lazily.
  :demand)

(require 'org)
(defun init/load-directory-recursively (directory)
  "Tangle, compile, and load the Org files in DIRECTORY."
  (let ((org-regexp (rx ".org" string-end))
        (el-extension ".el"))
    (dolist (org-file
             (directory-files-recursively directory org-regexp))
      (let ((el-file (replace-regexp-in-string
                      org-regexp el-extension org-file)))
        (if (not (file-exists-p el-file))
            ;; If the Emacs Lisp file does not exist, tangle the Org file.
            (org-babel-tangle-file org-file)
          ;; If the Emacs Lisp file does exist, compare the modification times.
          (let* ((modify-time (lambda (file)
                                (float-time
                                 (file-attribute-modification-time
                                  (file-attributes file)))))
                 (org-modify (funcall modify-time org-file))
                 (el-modify (funcall modify-time el-file)))
            ;; If the Org file was modified more recently, tangle it.
            (when (< el-modify org-modify)
              (org-babel-tangle-file org-file))))
        ;; Compile and load the tangled file.
        (byte-compile-file el-file :load)))))

(init/load-directory-recursively "~/.emacs.d/config/")

(provide 'init)
;;; init.el ends here
