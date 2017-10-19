;;; delete-file-and-buffer.el --- Define function to deleting current buffer and its file
;;
;; Copyright (c) 2013 Bozhidar Batsov
;; License: GPLv3
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file provides the delete-file-and-buffer function which
;; deletes the current buffer and its corresponding file.

;;; Code:

(defun delete-file-and-buffer ()
  "Delete the current buffer and the corresponding file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
	  (vc-delete-file filename)
	(progn
	  (delete-file filename)
	  (message "Deleted file %s" filename)
	  (kill-buffer))))))

(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

(provide 'delete-file-and-buffer)
;;; delete-file-and-buffer.el ends here
