;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-

;;; Commentary:

;; Personal emacs configuration.

;;; Code:

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

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

      indent-tabs-mode nil
      tab-width 8)


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

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)
(define-key 'help-command (kbd "C-i") #'info-display-manual)

(use-package recentf-mode
  :defer t
  :config
  (setq recentf-save-file (my-savefile-dir "recentf")))

(use-package semantic-mode
  :defer t
  :config
  (setq semanticdb-default-save-directory (my-savefile-dir "semanticdb")))

(use-package man
  :defer t
  :config
  (setq Man-width 79))

(use-package my-editing-defuns
  :bind (("M-W" . my-copy-line-as-kill)))

(use-package isearch
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp))

  :init
  (use-package my-isearch-defuns)
  (use-package helm-swoop
    :commands (helm-swoop-from-isearch))

  (defvar my-isearch-done-opposite
    nil
    "Wether or not isearch should end at the opposite side of the match.")

  (defadvice isearch-done (after my-isearch-done-opposite activate)
    "After finding a match position the cursor at the opposite side of the match."
    (when my-isearch-done-opposite
      (goto-char isearch-other-end)))

  (define-key isearch-mode-map (kbd "C-<return>") #'my-isearch-done-opposite)
  (define-key isearch-mode-map (kbd "M-o") #'helm-swoop-from-isearch))


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
  :config (setq uniquify-buffer-name-style 'forward
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
  :config
  (setq eshell-where-to-jump 'begin
	eshell-review-quick-commands nil
	eshell-smart-space-goes-to-end t))

(use-package yasnippet
  :config (setq yas-wrap-around-region t))

(use-package expand-region
  :bind
  ("C-@" . er/expand-region))

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

(use-package helm-config
  :commands
  (helm-M-x
   helm-show-kill-ring
   helm-mini
   helm-find-files
   helm-apropos
   helm-info-emacs
   helm-locate-library)

  :bind
  (("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-h f" . helm-apropos)
   ("C-h C-l" . helm-locate-library)
   ("C-c h" . helm-command-prefix))

  :config
  (setq helm-split-window-in-side-p t
	helm-buffers-fuzzy-matching t
	helm-move-to-line-cycle-in-source t
	helm-ff-search-library-in-sexp t
	helm-ff-file-name-history-use-recentf t)
  
  ;; (define-key helm-map (kbd "o") #'helm-occur)
  ;; (define-key helm-map (kbd "SPC") #'helm-all-mark-rings)
  ;; (define-key helm-map (kbd "r") #'helm-recentf)

  ;; (define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history)
  ;; (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  ;; (add-to-list 'helm-sources-using-default-as-input #'helm-source-man-pages)

  (helm-descbinds-mode)
  (my-enable-mode 'helm-mode))

(use-package projectile
  :commands
  (projectile-vc
   projectile-switch-project
   projectile-find-file
   projectile-find-dir
   projectile-ag
   projectile-grep
   projectile-ibuffer)

  :bind
  (("C-c p p" . projectile-switch-project)
   ("C-c p v" . projectile-vc)
   ("C-c p f" . projectile-find-file)
   ("C-c p d" . projectile-find-dir)
   ("C-c p b" . projectile-ibuffer)
   ("C-c p s s" . projectile-ag)
   ("C-c p s g" . projectile-grep))
  
  :config
  (use-package helm-projectile)
  (use-package wgrep-helm)
  
  (setq projectile-enable-caching t
	projectile-cache-file (my-savefile-dir "projectile.cache")
	projectile-known-projects-file (my-savefile-dir "projectile.bookmarks.eld")
	projectile-completion-system 'helm)
  
  (helm-projectile-on)
  (my-enable-mode 'projectile-global-mode))


(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :interpreter "emacs"
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode 1))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (use-package js2-refactor)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  
  (add-hook 'js2-mode-hook (fprogn
			    (setq js2-basic-offset 2
				  js-indent-level 2
				  js2-include-node-externs t)
			    
			    (my-enable-modes '(subword-mode
					       electric-pair-mode
					       aggressive-indent-mode)))))

(use-package html-mode
  :mode "\\.html\\'"
  :config
  (use-package tagedit)
  (add-hook 'html-mode-hook (fprogn
			     (tagedit-add-experimental-features)

			     (setq emmet-indent-after-insert t
				   emmet-indentation 2)
			     
			     (my-enable-modes '(tagedit-mode
						emmet-mode
						electric-pair-mode)))))

(when window-system
  (let ((elapsed-time (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s... done (%.3fs)" load-file-name elapsed-time)))

;;; init.el ends here
