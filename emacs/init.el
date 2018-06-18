;;; init.el --- Setup package management and load lisp/

;;; Commentary:
;; All non-comment code in this file is copyright Joseph M LaFreniere and licensed under [[https://gitlab.com/lafrenierejm/dotfiles/blob/master/LICENSE][an ISC license]] except when otherwise noted.
;; All prose in this file is is copyright Joseph M LaFreniere and licensed under [[https://creativecommons.org/licenses/by/4.0/][CC BY 4.0]] except when otherwise noted.

;;; Code:

;;; Package Management with straight.el
;; [[https://github.com/raxod502/straight.el][straight.el]] is a "next-generation, purely functional package manager".

;; Disable package.el.
(setq package-enable-at-startup nil)

;; Bootstrap straight.el, a purely functional package manager.
(setq straight-recipe-overrides
      '((nil . ((straight :type git :host github
                             :repo "raxod502/straight.el"
                             :branch "develop"
                             :files ("straight.el"))))))

(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Integration with use-package
;; straight.el provides native support for jwiegley's [[https://github.com/jwiegley/use-package][use-package]].
;; To begin, use-package.el is installed with straight.el.

;; Enable straight.el's integration with use-package.
(straight-use-package 'use-package)
(straight-use-package 'diminish)

;; Automatically install missing packages.
(setq straight-use-package-by-default t)

;; Default to loading lazily.
;; This behavior makes it more explicit when a package will be loaded.
;; It can be overridden with the `:demand' macro.
(setq use-package-always-defer t)

;;; Load validate.el
;; [[https://github.com/Malabarba/validate.el][validate.el]] provides functions to perform schema validation.

(use-package validate
  :demand                               ; do not defer loading

  :straight                             ; recipe for straight.el
  (validate :type git :host github :repo "Malabarba/validate.el"))

;;; Load Org-mode
;; `org-babel-tangle' is provided by the org package.
;; So in order to tangle the rest of the configuration, Org-mode must be loaded.
;; This has a startup time cost, but daemonization can be used to incur that cost only once per login.

(use-package org
  :demand                               ; do not defer loading

  :straight nil                    ; do not download using straight.el

  :after        ; load the parent package after the following packages
  (validate)

  :defines   ; variables provided by the parent package and used below
  (org-highlight-latex-and-related
   org-mode-hook)

  :mode          ; establish deferred binding within `auto-mode-alist'
  (("\\.org\\'" . org-mode))

  :config   ; code to execute after the parent package has been loaded
  (validate-setq org-highlight-latex-and-related '(latex)
                 org-return-follows-link t
                 org-src-fontify-natively t
                 org-confirm-babel-evaluate nil)
  ;; Set the workflow states.
  ;; https://github.com/Malabarba/validate.el/issues/5 prevents using
  ;; `validate-setq' to set the value.
  (setq org-todo-keywords
                 '((sequence
                    "TODO" "FEEDBACK" "VERIFY" "|"
                    "DELEGATED" "DONE" "|"
                    "CANCELED")))
  ;; Add minor modes to `org-mode-hook'
  (dolist (minor-mode (list
                       'flyspell-mode
                       'visual-line-mode))
    (add-hook 'org-mode-hook minor-mode)))

(defun init/load-directory-recursively (directory)
  "Tangle, compile, and load the Org-mode files in DIRECTORY."
  (dolist (org-file (directory-files-recursively directory
                                                 (rx ".org" string-end)))
    (org-babel-load-file org-file 'compile-before-loading)))

(init/load-directory-recursively "~/.emacs.d/config/")

(provide 'init)
;;; init.el ends here
