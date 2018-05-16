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
  (validate :type git :host github :repo "Malabarba/validate.el")

  :commands   ; commands provided by the parent package and used below
  (validate-setq))

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

;;; Build the List of Files to Load
;; 1. The given directory (passed as an argument) is searched recursively for Org mode files ("org" filename extension).
;;    Each Org file found is added to a list.
;; 2. For each Org file in the list, look for a corresponding Emacs Lisp file ("el" filename extension).
;;    "Correspondence" is determined by the Org and Lisp filenames matching until the extension.
;; 3. If the Emacs Lisp file has been modified more recently than its corresponding Org file, remove the Org file from the list.

(defun init/filter (condp lst)
  "Remove items that do not meet CONDP from LST."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x ) x)) lst)))

(defun init/replace-file-name-extension (filename extension)
  "Replace a FILENAME's extension with EXTENSION."
  (if (string= "." (substring extension  0 1)) ; If user included "." in extension
      (concat (file-name-sans-extension filename) extension)
    (concat (file-name-sans-extension filename) "." extension)))

(defun init/org/find-files-to-tangle (directory tangle-extension)
  "Return the Org files in DIRECTORY older than corresponding files with TANGLE-EXTENSION."
  (let ((org-files (directory-files-recursively directory "\.org$")))
    (init/filter (lambda (org-file)
                   (let ((tangle-file
                          (init/replace-file-name-extension org-file tangle-extension)))
                     (file-newer-than-file-p org-file tangle-file)))
                 org-files)))

;;; Tangle Code from config/ Recursively
;; The bulk of my configuration is in Org files in the config/ subdirectory.
;; Now the above functions are be employed to tangle the Emacs Lisp from the Org files.

(mapcar (lambda (org-file)
          (message "Tangling and compiling " org-file ".")
          ;; (byte-compile-file (car (org-babel-tangle-file org-file)))
          (org-babel-tangle-file org-file))
        (init/org/find-files-to-tangle
         (concat (file-name-as-directory user-emacs-directory) "config")
         ".el"))

;;; Load Compiled Emacs Lisp
;; The previous tangling extracted all Emacs Lisp code from Org-mode files in config/.
;; Now config/ is searched for Emacs Lisp files which have the filename extension "el".

(defun init/load-directory-recursively (directory)
  "Recurse through DIRECTORY and load all compiled Emacs Lisp files found."
  (dolist (elisp-file (directory-files-recursively directory "^[^.]+\.el$"))
    (load-file elisp-file)))

(init/load-directory-recursively
 (concat (file-name-as-directory user-emacs-directory) "config"))

(provide 'init)
;;; init.el ends here
