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

(defun my-inside-project-p ()
  "Check if we are inside a project."
  (and (fboundp 'projectile-project-p)
       (projectile-project-p)))

(defun my-kill-buffer-and-file (&optional buffer-or-name)
  "Kill BUFFER-OR-NAME and its associated file.
If BUFFER-OR-NAME is not specified the current buffer is used."
  (interactive
   (list (read-buffer (format "Kill buffer and its file (default %s): "
                              (buffer-name (current-buffer))))))
  (let* ((buffer (get-buffer buffer-or-name))
         (filename (buffer-file-name buffer)))
    (kill-buffer buffer)
    (delete-file filename)))

(defun my-rename-buffer-and-file (newname newfilename)
  "Rename current buffer to NEWNAME and its file to NEWFILENAME."
  (interactive
   (let* ((newname (read-string "Rename buffer (to new name): "))
          (newfilename (read-string "Rename file (to new name): " newname)))
     (list newname newfilename)))
  (let ((ask-if-exists 1))
    (rename-file (buffer-file-name) newfilename ask-if-exists)
    (set-visited-file-name newfilename :no-query :along-with-file)))

(provide 'my-prelude)
;;; my-prelude.el ends here
