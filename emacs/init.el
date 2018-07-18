;;; init.el --- Setup package management and load lisp/

;;; Commentary:
;; All non-comment code in this file is copyright Joseph M LaFreniere and licensed under [[https://gitlab.com/lafrenierejm/dotfiles/blob/master/LICENSE][an ISC license]] except when otherwise noted.
;; All prose in this file is is copyright Joseph M LaFreniere and licensed under [[https://creativecommons.org/licenses/by/4.0/][CC BY 4.0]] except when otherwise noted.

;;; Code:

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)
(push '("org" . "https://orgmode.org/elpa") package-archives)
(package-refresh-contents)
(package-initialize)
(package-install 'use-package)

(require 'use-package)
(setq use-package-always-ensure t)


;; Default to loading lazily.
;; This behavior makes it more explicit when a package will be loaded.
;; It can be overridden with the `:demand' macro.
(setq use-package-always-defer t)

;;; Load validate.el
;; [[https://github.com/Malabarba/validate.el][validate.el]] provides functions to perform schema validation.

(use-package validate
  ;; Do not load lazily.
  :demand)

(require 'org)
(defun init/load-directory-recursively (directory)
  "Tangle, compile, and load the Org-mode files in DIRECTORY."
  (dolist (org-file (directory-files-recursively directory
                                                 (rx ".org" string-end)))
    (org-babel-load-file org-file 'compile-before-loading)))

(init/load-directory-recursively "~/.emacs.d/config/")

(provide 'init)
;;; init.el ends here
