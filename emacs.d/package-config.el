;;; Configure package.el
(eval-when-compile (require 'package))
;; Disable automatic package loading
(setq package-enable-at-startup nil)
;; Build list of archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives ("gnu" . "https://elpa.gnu.org/packages/")))
;; Make sure package archives have been downloaded
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
;; Activate installed packages
(package-initialize)

;;; Install use-package
;; A use-package declaration for simplifying Emacs config
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'bind-key); Allow binding from during module load
(require 'diminish); Remove clutter from mode-line
