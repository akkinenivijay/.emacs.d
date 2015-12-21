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
  (when (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))))

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

(defun my-eval-last-sexp (arg)
  "Extension over eval-last-sexp that replaces the last sexp with the
result if called with the universal argument twice."
  (interactive "P")
  (if (= 16 (prefix-numeric-value arg))
      (my-replace-last-sexp)
    (eval-last-sexp arg)))

(defun my-replace-last-sexp ()
  "Eval last sexp and replaces it in the buffer with its result."
  (interactive)
  (let ((result (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" result))))

(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      "xdg-open")
                    " "
                    buffer-file-name))))

(provide 'my-editing-defuns)
;;; my-editing-defuns.el ends here
