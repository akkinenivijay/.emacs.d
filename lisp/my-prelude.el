;;; my-prelude.el --- Utility functions. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)

(defun my-cycle (list)
  "Return copy of LIST turned into an infinite list."
  (let ((newlist (copy-list list)))
    (setf (cdr (last newlist))
          newlist)
    newlist))

(provide 'my-prelude)
;;; my-prelude.el ends here
