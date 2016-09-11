(use-package auctex
  :ensure t
  :commands
  (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  ;; Use RefTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))
