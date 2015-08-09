;;; my-themes.el --- Utility functions related to theming. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)
(require 'my-prelude)

(defvar my-load-theme-hook
  nil
  "Hooks to run after loading a theme.")

(defvar my-themes
  '(default)
  "Use next theme.")

(defvar my-themes-delta
  1
  "Number of themes.")

(defun my-set-themes (themes)
  "Set THEMES as the themes to use."
  (setq my-themes (my-cycle themes)
        my-themes-delta (length themes)))

(defun my-use-next-theme ()
  "Use next theme in `my-themes'."
  (interactive)
  (my/disable-themes)
  (let ((theme (pop my-themes)))
    (my/load-theme theme)
    (run-hooks 'my-load-theme-hook)
    (message "Theme '%s' loaded" theme)))

(defun my-use-prev-theme ()
  "Use previous theme in `my-themes'."
  (interactive)
  (my/disable-themes)
  (let ((n (1+ my-themes-delta)))
    (while (> n 0)
      (pop my-themes)
      (setq n (1- n)))
    (my-use-next-theme)))

(defun popn (n list)
  "Pop the N-element of LIST."
  (cond ((> n 0) (let ((_ (pop list)))
                   (popn (1- n) list)))
        (t (pop list))))

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
