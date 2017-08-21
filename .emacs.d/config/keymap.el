;;; config-keymap.el -- Custom keybindings

;;; Commentary:
;; Create keybindings with the goal of portability (e.g. work without
;; external packages)

;;; Code:
;;; escape will quit all states
;; http://stackoverflow.com/a/10166400/61435
(defun minibuffer-cursor-quit ()
  "Abort recursive edit.
In Delete-Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer.
Windows opened for completions are closed automatically."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(add-hook 'evil-local-mode-hook
	  (lambda()
	    (define-key evil-normal-state-map [escape] 'keyboard-quit)
	    (define-key evil-visual-state-map [escape] 'keyboard-quit)
	    (define-key minibuffer-local-map [escape] 'minibuffer-cursor-quit)
	    (define-key minibuffer-local-ns-map [escape] 'minibuffer-cursor-quit)
	    (define-key minibuffer-local-completion-map [escape] 'minibuffer-cursor-quit)
	    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-cursor-quit)
	    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-cursor-quit)))

;;; Move to the beginning of the line intelligently
;; http://stackoverflow.com/a/145359
(defun smart-beginning-of-line ()
  "Move point to first nont-whitespace character or beginning of line.
Move point to the first non-whitespace on the line.
If the point was already at that position, move point to beginning of line."
  (interactive "^") ; First significant character
  (let ((oldpos (point)))
    (back-to-indentation) ; First column
    (and (= oldpos (point))
	 (beginning-of-line))))

;; Map "H" to smart-beginning-of-line in normal and visual modes
(add-hook 'evil-local-mode-hook
	  (lambda()
	    (define-key evil-normal-state-map "H" 'smart-beginning-of-line)
	    (define-key evil-visual-state-map "H" 'smart-beginning-of-line)))

;;; Move to the end of the line intelligently
;; http://stackoverflow.com/a/14245964
(defun end-of-significant ()
  "Move point to the end of the significant text on a line.
If the line contains a comment or trailing whitespace, the point is
moved before it."
  (interactive "^")
  (save-match-data
    (let* ((bolpos (progn (beginning-of-line) (point)))
	   (eolpos (progn (end-of-line) (point))))
      (if (comment-search-backward bolpos t)
	  (search-backward-regexp comment-start-skip bolpos 'noerror)))))

(defun smart-end-of-line ()
  "Move point to the end of significant text or the end of line.
Move point to the end of significant (e.g. non-whitespace,
non-comment) text.  If the point was already at that position, move
point to end of line."
  (interactive "^")
  (let ((oldpos (point))) ; Get the current point as oldpos
    (end-of-significant) ; Go to last significant point
    (if (or (>= oldpos (point))
	    (bolp))
	(end-of-line))))

;; Map "L" to end-of-significant-or-line in normal and visual modes
(add-hook 'evil-local-mode-hook
	  (lambda()
	    (define-key evil-normal-state-map "L" 'smart-end-of-line)
	    (define-key evil-visual-state-map "L" 'smart-end-of-line)))
