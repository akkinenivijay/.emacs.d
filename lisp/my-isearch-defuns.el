;;; my-isearch-defuns.el --- Utility functions related to editing.

;;; Commentary:

;;; Code:

(defvar my-isearch-done-opposite
    nil
    "Wether or not isearch should end at the opposite side of the match.")

(defun my-isearch-done-opposite (&optional nopush edit)
  "End current search in the opposite side of the match.

The arguments NOPUSH and EDIT are passed to the wrapped function `isearch-done'."
  (interactive)
  (let ((my-isearch-done-opposite t))
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end
      (goto-char isearch-other-end))))

(provide 'my-isearch-defuns)
;;; my-isearch-defuns.el ends here
