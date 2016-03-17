;;; my-themes.el --- Utility functions related to theming. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)
(require 'my-prelude)

(defvar my-load-theme-hook
  nil
  "Hooks to run after loading a theme.")

(defvar my-saved-theme-filename "~/.emacs-theme")

(advice-add 'load-theme :after #'my-save-theme)
(advice-add 'disable-theme :after #'my-save-default-theme)
(advice-add 'load-theme :after #'my-run-theme-hooks)

(defun my-run-theme-hooks (theme &optional no-confirm no-enable)
  (run-hooks 'my-load-theme-hook))

(defun my-save-default-theme (disabled-theme)
  (my-save-theme 'default))

(defun my-save-theme (theme &optional no-confirm no-enable)
  (with-temp-buffer
    (insert (symbol-name theme))
    (when (file-writable-p my-saved-theme-filename)
      (write-region (point-min)
                    (point-max)
                    my-saved-theme-filename))))

(defun my-load-saved-theme ()
  (interactive)
  (let ((theme (intern (with-temp-buffer
                         (insert-file-contents my-saved-theme-filename)
                         (buffer-string)))))
    (unless (eq theme 'default)
      (load-theme theme :no-confirm))))

(provide 'my-themes)
;;; my-themes.el ends here
