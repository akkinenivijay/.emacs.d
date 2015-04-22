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

(defun my-kill-sexp-backwards ()
  "Kill the sexp leading point."
  (interactive)
  (kill-sexp -1))

(defun my-comment-or-uncomment-line (&optional arg)
  "Comment current line or, if at EOL, call `comment-dwim' with ARG."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

(defun my-isearch-forward-regexp-other-window ()
  "`isarch-forward-regexp' in `other-window'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(defun my-isearch-backward-regexp-other-window ()
  "`isarch-backward-regexp' in `other-window'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-backward-regexp)))

(provide 'my-editing-defuns)
;;; my-editing-defuns.el ends here
