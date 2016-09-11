(use-package evil
  :ensure t
  :init
  (setq evil-default-cursor t) ; Do not overwrite cursor color
  :config
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))
  (evil-mode 1) ; Enable evil mode by default
  )
