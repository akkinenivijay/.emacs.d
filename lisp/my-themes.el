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

(defvar my-current-theme
  'default
  "Current active theme.")

(defun -my-get-next-theme ()
  (let ((theme (cadr (-drop-while (lambda (item) (not (eq item my-current-theme)))
                                  my-themes))))
    (or theme (car my-themes) 'default)))

(defun -my-get-prev-theme ()
  (let ((theme (-last-item (-take-while (lambda (item)
                                          (not (eq item my-current-theme)))
                                        my-themes))))
    (or theme (-last-item my-themes) 'default)))

(defun -my-get-theme-pkg-name (theme)
  (format "%s-theme" (symbol-name theme)))

(defun -my-theme-available? (theme)
  (or
   (eq theme 'default)
   (member theme (custom-available-themes))
   (let ((theme-pkg (-my-get-theme-pkg-name theme)))
     (package-install (intern theme-pkg)))))



(defun my-use-next-theme ()
  "Use next theme in `my-themes'."
  (interactive)
  (let ((theme (-my-get-next-theme)))
    (message "Next theme is %s" theme)
    (when (-my-theme-available? theme)
      (my-load-theme theme)
      (run-hooks 'my-load-theme-hook)
      (message "Theme '%s' loaded." theme))
    ))

(defun my-use-prev-theme ()
  "Use previous theme in `my-themes'."
  (interactive)
  (let ((theme (-my-get-prev-theme)))
    (message "Prev theme is %s" theme)
    (when (-my-theme-available? theme)
      (my-load-theme theme)
      (run-hooks 'my-load-theme-hook)
      (message "Theme '%s' loaded." theme))
    ))

(defun my-load-theme (theme)
  "Use THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (unless (eq theme 'default)
    (load-theme theme :noconfirm))
  (setq my-current-theme theme))

(provide 'my-themes)
;;; my-themes.el ends here
