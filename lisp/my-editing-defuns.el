;;; my-editing-defuns.el --- Utility functions related to editing. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my-copy-line-as-kill (arg)
  "Copy ARG lines as kill."
  (interactive "p")
  (save-excursion
    (let ((beg (progn
		 (beginning-of-line)
		 (point)))
	  (end (progn
		 (forward-line arg)
		 (point))))
      (copy-region-as-kill beg end))))

(provide 'my-editing-defuns)
;;; my-editing-defuns.el ends here
