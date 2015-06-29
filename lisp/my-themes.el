;;; my-themes.el --- Utility functions related to theming. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)
(require 'my-prelude)

(defvar my-themes
  '(default)
  "Use next theme.")

(defun my-set-themes (themes)
  "Set THEMES as the themes to use."
  (setq my-themes (my-cycle themes)))

(defun my-use-next-theme ()
  "Use next theme in `my-themes'."
  (interactive)
  (my/disable-themes)
  (my/load-theme (pop my-themes)))

(defun my-load-theme (theme)
  "Use THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name (custom-available-themes))))))
  (my/disable-themes)
  (my/load-theme theme))

(defun my/disable-themes ()
  "Disable all enabled themes."
  (mapc #'disable-theme custom-enabled-themes))

(defun my/load-theme (theme)
  "Enable THEME."
  (unless (eq theme 'default)
    (load-theme theme :noconfirm)))

(provide 'my-themes)
;;; my-themes.el ends here
