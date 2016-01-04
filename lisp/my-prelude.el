;;; my-prelude.el --- Utility functions. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)

(defcustom my-font-family
  "Hack"
  "My preferred font family."
  :group 'personal)

(defcustom my-font-size
  13
  "My preferred font size."
  :group 'personal)

(defun my-enable-mode (mode)
  "Enable MODE."
  (my-disable-mode mode)
  (when (fboundp mode)
    (funcall mode +1)))

(defun my-disable-mode (mode)
  "Disable MODE."
  (when (boundp mode)
    (funcall mode -1)))

(defalias 'my-enable-modes (apply-partially 'mapc #'my-enable-mode)
  "Enable all modes in argument.")

(defalias 'my-disable-modes (apply-partially 'mapc #'my-disable-mode)
  "Disable all modes in argument.")

(defmacro command (&rest body)
  "Wrap BODY inside an interactive lambda."
  `(lambda ()
     (interactive)
     ,@body))

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

(defun my-switch-to-next-buffer ()
  "Switch to the next buffer associated to a file."
  (interactive)
  (cl-labels ((next-non-special-buffer
               ()
               (let ((buffer (next-buffer)))
                 (if (buffer-file-name buffer)
                     buffer
                   (next-non-special-buffer)))))
    (next-non-special-buffer)))

(defun my-switch-to-previous-buffer ()
  "Switch to the previous buffer associated to a file."
  (interactive)
  (cl-labels ((previous-non-special-buffer
               ()
               (let ((buffer (previous-buffer)))
                 (if (buffer-file-name buffer)
                     buffer
                   (previous-non-special-buffer)))))
    (previous-non-special-buffer)))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(provide 'my-prelude)
;;; my-prelude.el ends here
