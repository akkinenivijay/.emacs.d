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

(defcustom my-font-family
  "Fantasque Sans Mono"
  "Docs."
  :group 'personal)

(defcustom my-font-size
  18
  "Docs."
  :group 'personal)

(defun my-add-to-load-path (path)
  "Add PATH (relative to `user-emacs-directory') to `load-path'."
  (push (expand-file-name path user-emacs-directory)
        load-path))

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

(defalias 'yes-or-no-p 'y-or-n-p)

(mapc #'my-add-to-load-path my-paths)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
;; (let ((bundle (cask-initialize)))
;;   when hashed bundle != bundle do:
;;   (when window-system
;;     (cask-install bundle)
;;     (cask-update bundle)))

(require 'use-package)
(require 'pallet)
(pallet-mode t)

(let ((font-and-size (format "%s-%s" my-font-family my-font-size)))
  (add-to-list 'default-frame-alist `(font . ,font-and-size)))

(setq-default indent-tabs-mode nil
              tab-width 8)

(setq gc-cons-threshold 50000000
      large-file-warning-threshold 100000000

      load-prefer-newer t
      require-final-newline t

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
      scroll-preserve-screen-position t)


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
                   savehist-mode))

(my-disable-modes '(scroll-bar-mode
                    tool-bar-mode
                    menu-bar-mode
                    blink-cursor-mode
                    transient-mark-mode))

(dolist (r `((?i . (file . ,(expand-file-name "init.el" user-emacs-directory)))
             (?s . (file . ,(expand-file-name "snippets" user-emacs-directory)))))
  (set-register (car r) (cdr r)))

(when window-system
  (unbind-key "C-z"))

(bind-keys ("M-U" . (command (upcase-word -1)))
           ("M-%" . query-replace-regexp)
           ("C-M-;" . comment-or-uncomment-region)
           ("C-<tab>" . mode-line-other-buffer)
           ("s-<up>" . enlarge-window)
           ("s-<down>" . shrink-window)
           ("s-M-<up>" . enlarge-window-horizontally)
           ("s-M-<down>" . shrink-window-horizontally))

(use-package window-numbering
  :defer 1
  :config (my-enable-mode 'window-numbering-mode))

(use-package eyebrowse
  :defer 1
  :config (my-enable-mode 'eyebrowse-mode))

(use-package discover
  :defer 2
  :config (my-enable-mode 'discover-mode))

(use-package my-editing-defuns
  :bind (("M-W" . my-copy-line-as-kill)
         ("s-M-k" . my-kill-sexp-backwards)
         ("C-;" . my-comment-or-uncomment-line)
         ("C-M-s" . my-isearch-forward-regexp-other-window)
         ("C-M-r" . my-isearch-backward-regexp-other-window)
         ("C-x C-e" . my-eval-last-sexp)))

(use-package man
  :defer t
  :config
  (setq Man-width 79))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
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
  :bind (([f8] . my-use-next-theme)
         ([f7] . my-use-prev-theme))
  :config
  (my-set-themes '(spacemacs-light
                   oldlace
                   spacemacs-dark)))

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
  :config (setq bookmark-save-flag 1))

(use-package savehist
  :defer t
  :config (setq savehist-additional-variables '(search-ring regexp-search-ring)
                savehist-autosave-interval 60))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package hippie-expand
  :bind ("M-/" . hippie-expand)
  :init (setq hippie-expand-try-functions-list '(try-expand-dabbrev
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
  :config (setq save-place t))

(use-package tramp
  :defer t
  :config (setq tramp-default-method "ssh"))

(use-package server
  :defer t
  :config (setq server-use-tcp t))

(use-package change-inner
  :commands (change-inner change-outer)
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package yasnippet
  :defer 2
  :config
  (setq yas-wrap-around-region t)
  (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap)
  (yas-reload-all))

(use-package magit
  :defer t
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-status-buffer-switch-function #'switch-to-buffer)

  (use-package fullframe
    :config
    (fullframe magit-status magit-mode-quit-window nil)))

(use-package eshell
  :config
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t))

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

(use-package avy
  :bind (("C-." . avy-goto-char-2)
         ("C-," . avy-goto-char)
         ("M-g w" . avy-goto-word-0)
         ("M-g M-g" . avy-goto-line)))

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
   ("M-Y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-h f" . helm-apropos)
   ("C-h b" . helm-descbinds)
   ("C-h C-l" . helm-locate-library)
   ("C-c h" . helm-command-prefix))

  :config
  (setq helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t)

  ;; (define-key helm-map (kbd "o") #'helm-occur)
  ;; (define-key helm-map (kbd "SPC") #'helm-all-mark-rings)
  ;; (define-key helm-map (kbd "r") #'helm-recentf)

  ;; (define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history)
  ;; (substitute-key-definition 'find-tag 'helm-etags-select global-map)

  (add-to-list 'helm-sources-using-default-as-input #'helm-source-man-pages)
  (helm-descbinds-mode)
  (my-enable-mode 'helm-mode
                  'helm-autoresize-mode))

(use-package projectile
  :defer 1
  :diminish projectile-mode
  :config
  (use-package helm-projectile)
  (use-package wgrep-helm)

  (setq projectile-enable-caching t
        projectile-completion-system 'helm)

  (projectile-load-known-projects)
  (helm-projectile-on)
  (my-enable-mode 'projectile-global-mode))

(use-package whitespace
  :init
  (defun my-cleanup-whitespace-on-write ()
    "Cleanup whitespace on editor writes."
    (interactive)
    (my-enable-mode 'whitespace-mode)
    (add-hook 'before-save-hook #'whitespace-cleanup nil :local))

  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'my-cleanup-whitespace-on-write))

  :config
  (setq whitespace-line-column nil
        whitespace-style '(face tabs empty trailing lines-tail))
  :diminish whitespace-mode)

(or (use-package mic-paren
      :defer 2
      :config (paren-activate))
    (use-package paren
      :defer 2
      :config (my-enable-mode 'show-paren-mode)))

(use-package org-mode
  :mode "\\.org\\'"
  :init (add-hook 'org-mode-hook (defun my-org-setup ()
                                   (interactive)
                                   (my-enable-modes '(electric-pair-mode)))))

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :interpreter "emacs"
  :bind (("C-x C-e" . pp-eval-last-sexp))
  :init
  (fset 'my-emacs-lisp-mode-hook (defun my-elisp-setup ()
                                   (interactive)
                                   (my-enable-modes '(paredit-mode
                                                      aggressive-indent-mode
                                                      rainbow-delimiters-mode
                                                      eldoc-mode))))
  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :init (add-hook 'clojure-mode-hook (defun my-clojure-setup ()
                                       (interactive)
                                       (my-enable-modes '(subword-mode
                                                          paredit-mode
                                                          aggressive-indent-mode
                                                          rainbow-delimiters-mode)))))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init (add-hook 'haskell-mode-hook (defun my-haskell-setup ()
                                       (interactive)
                                       (my-enable-modes '(subword-mode
                                                          ghc-mode
                                                          flycheck-mode
                                                          haskell-doc-mode
                                                          haskell-indent-mode)))))
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package cider-mode
  :init (add-hook 'cider-mode-hook (defun my-clojure-repl-setup ()
                                     (interactive)
                                     (my-enable-modes '(subword-mode
                                                        paredit-mode
                                                        rainbow-delimiters-mode))))
  :config (setq nrepl-log-messages t))


(use-package js2-mode
  :mode "\\.js\\'"
  :init (add-hook 'js2-mode-hook (defun my-js-setup ()
                                   (interactive)
                                   (setq js2-basic-offset 2
                                         js-indent-level 2
                                         js2-include-node-externs t)

                                   (my-enable-modes '(subword-mode
                                                      hungry-delete-mode
                                                      wrap-region-mode
                                                      js2-refactor-mode
                                                      electric-pair-mode
                                                      tern-mode
                                                      aggressive-indent-mode))))
  :config
  (use-package js2-refactor
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

(use-package json-mode
  :mode "\\.json\\'"
  :init (add-hook 'json-mode-hook (defun my-json-setup ()
                                    (interactive)
                                    (setq json-reformat:indent-width 2
                                          js-indent-level 2)

                                    (my-enable-modes '(subword-mode
                                                       electric-pair-mode
                                                       aggressive-indent-mode)))))

(use-package html-mode
  :mode "\\.html\\'"
  :init
  (defun html-attrs-as-columns ()
    "Align all attrs in a tag in the same column."
    (interactive)
    (let ((regexp " [^> ][^=>]+\\(=\"[^\">]*\"\\)?")
          (bound (save-excursion (end-of-line)
                                 (point))))
      (while (search-forward-regexp regexp bound :noerror)
        (newline nil :interactive))))

  (defun html-attrs-as-line ()
    "Align all attrs in a tag in the same line."
    (interactive)
    (save-excursion
      (let ((regexp " [^> ][^=>]+\\(=\"[^\">]*\"\\)?"))
        (while (and (search-forward-regexp regexp nil :noerror)
                    (not (save-excursion
                           (search-forward ">"
                                           (save-excursion (end-of-line)
                                                           (point))
                                           :noerror))))
          (delete-char 1)
          (just-one-space)))))

  (add-hook 'html-mode-hook (defun my-html-setup ()
                              (interactive)
                              (bind-key "s-." 'html-attrs-as-columns)
                              (bind-key "s-," 'html-attrs-as-line)
                              (use-package tagedit)
                              (use-package emmet-mode)
                              (tagedit-add-experimental-features)
                              (setq emmet-indent-after-insert t
                                    emmet-indentation 2
                                    fill-column 999)
                              (my-enable-modes '(tagedit-mode
                                                 emmet-mode
                                                 wrap-region-mode
                                                 whitespace-mode
                                                 hungry-delete-mode
                                                 electric-pair-mode)))))

(use-package centered-window-mode
  :load-path "lisp/centered-window-mode"
  :commands (centered-window-mode))

(use-package js2r-conveniences
  :bind (("s-M-<up>" . move-line-up)
         ("s-M-<down>" . move-line-down)))

(defun my-yasnippet-setup ()
  (interactive)
  (setq fill-column 999)
  (my-enable-mode 'yas-minor-mode))

(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook #'my-yasnippet-setup))

(add-hook 'text-mode-hook (defun my-text-setup ()
                            (interactive)
                            (setq fill-column 99)))

(use-package gfm-mode
  :mode "\\.md\\'")

(when (fboundp 'my-use-next-theme)
  (my-use-next-theme))

(when window-system
  (let ((elapsed-time (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s... done (%.3fs)" load-file-name elapsed-time)))

;;; init.el ends here
