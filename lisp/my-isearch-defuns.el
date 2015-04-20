;;; my-isearch-defuns.el --- Utility functions related to editing. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my-isearch-done-opposite (&optional nopush edit)
  "End current search in the opposite side of the match.

The arguments NOPUSH and EDIT are passed to the wrapped function `isearch-done'."
  (interactive)
  (let ((my-isearch-done-opposite t))
    (funcall #'isearch-done nopush edit)))

(provide 'my-isearch-defuns)
;;; my-isearch-defuns.el ends here
