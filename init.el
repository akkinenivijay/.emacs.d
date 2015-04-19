;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-

;;; Commentary:

;; Personal emacs configuration.

;;; Code:

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(when (version< emacs-version "24.1")
  (error "Emacs >= 24.1 needed and you're using %s" emacs-version))

(defcustom my-paths
  '("lisp"
    "themes")
  "Docs."
  :group 'personal)

(defcustom my-savefile-dir
  (expand-file-name "savefile" user-emacs-directory)
  "Docs."
  :group 'personal)

(defcustom my-tmp-dir
  (expand-file-name "tmp" user-emacs-directory)
  "Docs."
  :group 'personal)

(defcustom my-font-family
  "Fantasque Sans Mono"
  "Docs."
  :group 'personal)

(defcustom my-font-size
  12
  "Docs."
  :group 'personal)

(defun my-savefile-dir (path)
  "Resolve PATH relative to `my-savefile-dir'."
  (expand-file-name path my-savefile-dir))

(defun my-add-to-load-path (path)
  "Add PATH (relative to `user-emacs-directory') to `load-path'."
  (push (expand-file-name path user-emacs-directory)
	load-path))

(defun my-enable-mode (mode)
  "Enable MODE."
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

(defmacro fprogn (&rest body)
  "Wrap BODY inside an interactive lambda."
  `(lambda ()
     (interactive)
     ,@body))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep emacs custom variables in another file
(setq custom-file (expand-file-name "custom.el" my-savefile-dir))
(load custom-file 'noerror)

(mapc #'my-add-to-load-path my-paths)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)
(require 'pallet)
(pallet-mode t)

(let ((font-and-size (format "%s-%s" my-font-family my-font-size))
      (internal-border 10))
  (add-to-list 'default-frame-alist `(font . ,font-and-size))
  (add-to-list 'default-frame-alist `(internal-border-width . ,internal-border))
  (add-hook 'my-after-load-theme-hook (lambda ()
					(let ((frame (window-frame)))
					  (when frame
					    (set-frame-font font-and-size :keep-size `(,frame))
					    (set-frame-parameter frame 'internal-border-width internal-border))))))

(setq gc-cons-threshold 50000000
      large-file-warning-threshold 100000000

      load-prefer-newer t

      require-final-newline t

      temporary-file-directory my-tmp-dir
      recentf-save-file (my-savefile-dir "recentf")
      semanticdb-default-save-directory (my-savefile-dir "semanticdb")

      backup-directory-alist `(("." . ,temporary-file-directory))
      backup-by-copying t
      delete-old-versions t
      version-control t

      confirm-kill-emacs #'y-or-n-p

      echo-keystrokes 0.2
      ring-bell-function #'ignore

      set-mark-command-repeat-pop t

      inhibit-startup-message t
      initial-scratch-message nil

      scroll-margin 2
      scroll-preserve-screen-position t

      Man-width 79

      indent-tabs-mode nil
      tab-width 8

      )


;; enable dangerous commands
(dolist (cmd '(narrow-to-region
	       narrow-to-page
	       narrow-to-defun
	       upcase-region
	       downcase-region
	       erase-buffer
	       eval-expression
	       set-goal-column))
  (put cmd 'disabled nil))


(my-enable-modes '(global-prettify-symbols-mode
		   global-auto-revert-mode
		   delete-selection-mode
		   column-number-mode
		   savehist-mode
		   show-paren-mode))

(my-disable-modes '(scroll-bar-mode
		    tool-bar-mode
		    menu-bar-mode
		    blink-cursor-mode
		    transient-mark-mode))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-U") (fprogn (upcase-word -1)))
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)
(define-key 'help-command (kbd "C-i") #'info-display-manual)

(use-package my-editing-defuns
  :bind (("M-W" . my-copy-line-as-kill)))

(use-package my-themes
  :bind ([f8] . my-use-next-theme)
  :config (setq my-themes '(default
			    solarized-light
			    solarized-dark
			    ir-black)))

(use-package compilation
  :defer t
  :config (setq compilation-ask-about-save nil
		compilation-always-kill t
		compilation-scroll-output 'first-error))

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
		ediff-diff-options "-w"))

(use-package bookmark
  :defer t
  :config (setq bookmark-default-file (my-savefile-dir "bookmarks")
		bookmark-save-flag 1))

(use-package savehist
  :defer t
  :config (setq savehist-additional-variables '(search-ring regexp-search-ring)
		savehist-autosave-interval 60
		savehist-file (my-savefile-dir "savehist")))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package hippie-expand
  :bind ("M-/" . hippie-expand)
  :config (setq hippie-expand-try-functions-list '(try-expand-dabbrev
						   try-expand-dabbrev-all-buffers
						   try-expand-dabbrev-from-kill
						   try-complete-file-name-partially
						   try-complete-file-name
						   try-expand-all-abbrevs
						   try-expand-list
						   try-expand-line
						   try-complete-lisp-symbol-partially
						   try-complete-lisp-symbol)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'formward
		uniquify-separator "/"
		uniquify-after-kill-buffer-p t
		uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :config (setq save-place-file (expand-file-name "saveplace" my-savefile-dir)
		save-place t))

(use-package tramp
  :defer t
  :config (setq tramp-default-method "ssh"))

(use-package server
  :defer t
  :config (setq server-use-tcp t))

(use-package magit
  :defer t
  :config ((setq magit-status-buffer-switch-function #'switch-to-buffer
		 magit-last-seen-setup-instrucctions "1.4.0")
	   (when (boundp 'magit-status-internal)
	     (defalias 'magit-status-internal 'magit-status))))

(use-package eshell
  :init (setq eshell-where-to-jump 'begin
	      eshell-review-quick-commands nil
	      eshell-smart-space-goes-to-end t))

(use-package yasnippet
  :defer t
  :init (setq yas-wrap-around-region t))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this
	     mc/mark-previous-like-this
	     mc/skip-to-next-like-this
	     mc/skip-to-previous-like-this
	     mc/edit-lines
	     mc/add-cursors-on-click)
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-M->" . mc/skip-to-next-like-this)
	 ("C-M-<" . mc/skip-to-previous-like-this)
	 ("C-S-c C-S-c" . mc/edit-lines)
	 ("C-M-0" . mc/mark-all-like-this)
	 ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config (ace-jump-mode-enable-mark-sync))

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package emacs-lisp
  :mode "\\.el\\'"
  :interpreter "emacs"
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode 1))

(use-package helm-config
  :config
  (setq helm-split-window-in-side-p t
	helm-buffers-fuzzy-matching t
	helm-move-to-line-cycle-in-source t
	helm-ff-search-library-in-sexp t
	helm-ff-file-name-history-use-recentf t)
  (global-set-key (kbd "C-c h") #'helm-command-prefix)

  (define-key helm-command-map (kbd "o") #'helm-occur)
  (define-key helm-command-map (kbd "g") #'helm-do-grep)
  (define-key helm-command-map (kbd "C-c w") #'helm-wikipedia-suggest)
  (define-key helm-command-map (kbd "SPC") #'helm-all-mark-rings)
  (define-key helm-command-map (kbd "r") #'helm-recentf)

  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-m") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-h f") #'helm-apropos)
  (global-set-key (kbd "C-h r") #'helm-info-emacs)
  (global-set-key (kbd "C-h C-l") #'helm-locate-library)

  (define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history))

(when window-system
  (let ((elapsed-time (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s... done (%.3fs)" load-file-name elapsed-time)))

;;; init.el ends here
