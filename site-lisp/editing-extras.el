(require 'cl)

(defun my/kill-sexp-backwards ()
  "Kill the sexp leading point."
  (interactive)
  (kill-sexp -1))

(defun my/find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(defun my/isearch-forward-regexp-other-window ()
  "`isarch-forward-regexp' in `other-window'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(defun my/isearch-backward-regexp-other-window ()
  "`isarch-backward-regexp' in `other-window'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-backward-regexp)))

(defun my/eval-last-sexp (arg)
  "Extension over eval-last-sexp that replaces the last sexp with the
    result if called with the universal argument twice."
  (interactive "P")
  (if (= 16 (prefix-numeric-value arg))
      (my-replace-last-sexp)
    (eval-last-sexp arg)))

(provide 'editing-extras)
