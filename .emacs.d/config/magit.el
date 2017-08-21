;;; magit-config.el --- magit configuration
;;; Code:
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  ;; Use evil-mode in magit buffers
  (use-package evil-magit
    :ensure t
    :config
    (add-hook 'magit-mode-hook 'evil-local-mode))
  ;; Close commit message buffer after commit
  (defadvice git-commit-commit
      (after delete-window activate) (delete-window))
  ;; Close commit message buffer after abort
  (defadvice git-commit-abort
      (after delete-window activate) (delete-window))
  )
