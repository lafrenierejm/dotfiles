(add-to-list 'auto-mode-alist
	     '("\.z[^.]*\\'" . shell-script-mode))

(add-hook 'shell-script-mode
	  (lambda ()
	    (if (string-match "\\.z[^.]*\\'" buffer-file-name)
		(sh-set-shell "zsh"))))
