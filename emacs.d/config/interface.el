;; Disable defaults
(setf inhibit-splash-screen t               ; Do not show splash screen
      inhibit-startup-message t             ; Do not show status message
      initial-major-mode #'fundamental-mode ; Start in most basic of modes
      initial-scratch-message nil)          ; Do not prepopulate scratch
(menu-bar-mode -1) ; Disable menu bar

(global-linum-mode 1) ; Always display line numbers

(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'linum)))

(unless window-system
  (setq linum-format 'linum-format-func))

(show-paren-mode t) ; Always highlight matching character pairs

;;; Interactive
(defalias 'yes-or-no-p 'y-or-n-p); Let 'y' and 'n' suffice for yes/no
