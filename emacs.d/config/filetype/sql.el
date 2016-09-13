;; Automatically capitalize SQL keywords
(use-package sqlup-mode
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  (add-hook 'redis-mode-hook 'sqlup-mode))
