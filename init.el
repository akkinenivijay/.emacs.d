;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-
;;
;; Author: Anler Hp <inbox@anler.me>
;; URL: https://gihub.com/anler/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration of Anler Hp.

;;; Code:
(require 'package)

(setq package-enable-at-startup nil
      package-archives '(
                         ("elpa" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         )
      )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package benchmark-init
  :ensure t
  :config (benchmark-init/activate))

(use-package prelude
  :load-path "~/.emacs.d/core"
  :demand t
  :bind* (("M-%" . query-replace-regexp)
          ("M-`" . other-frame)
          ("M-J" . delete-indentation)
          ("C-M-;" . comment-or-uncomment-region)
          ("C-<tab>" . mode-line-other-buffer)
          ("C-x C-d" . my-duplicate-line)
          ("C-x C-v" . my-find-alternate-file-with-sudo)
          ("S-<delete>" . delete-region)
          ("M-W" . my-copy-line-as-kill)
          ("s-M-k" . my-kill-sexp-backwards)
          ("C-;" . comment-line)
          ("C-M-s" . my-isearch-forward-regexp-other-window)
          ("C-c q" . delete-other-windows)
          ("C-M-r" . my-isearch-backward-regexp-other-window)
          ("C-x C-e" . my-eval-last-sexp)
          ("M-s-<up>" . enlarge-window)
          ("M-s-<down>" . enlarge-window-horizontally)
          )

  :config
  (define-key isearch-mode-map (kbd "C-<return>") #'my-isearch-done-opposite)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  (use-package cus-edit
    :init
    (defconst my-custom-file "~/.emacs.d/custom.el"
      "File used to store settings from Customization UI.")
    (load my-custom-file :noerror :nomessage)
    :config
    (setq custom-file my-custom-file
          custom-buffer-done-kill nil
          custom-buffer-verbose-help nil
          custom-unlispify-names nil
          custom-unlispify-menu-entries nil))

  (setq-default indent-tabs-mode nil
                tab-width 4)

  (setq gc-cons-threshold 100000000
        large-file-warning-threshold 100000000

        load-prefer-newer t
        require-final-newline t

        mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
        mouse-wheel-progressive-speed nil

        ;; auto-backup (filename~)
        backup-directory-alist `(("." . ,temporary-file-directory))
        backup-by-copying t
        delete-old-versions t
        version-control t

        ;; auto-save (#filename#)
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

        confirm-kill-emacs #'y-or-n-p

        echo-keystrokes 0.2
        ring-bell-function #'ignore

        set-mark-command-repeat-pop t

        inhibit-default-init t
        inhibit-startup-screen t
        initial-scratch-message nil

        scroll-margin 2
        scroll-preserve-screen-position t)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (dolist (r `((?i . (file . ,(expand-file-name "init.el" user-emacs-directory)))
               (?p . (file . ,(expand-file-name "core/my-prelude.el" user-emacs-directory)))
               (?s . (file . ,(expand-file-name "snippets" user-emacs-directory)))))
    (set-register (car r) (cdr r)))

  (when (window-system)
    (unbind-key "C-z"))

  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
  (add-hook 'after-init-hook #'my-load-saved-theme)

  (dolist (cmd '(narrow-to-region
                 narrow-to-page
                 narrow-to-defun
                 upcase-region
                 downcase-region
                 erase-buffer
                 eval-expression
                 dired-find-alternate-file
                 set-goal-column))
    (put cmd 'disabled nil))

  (my-disable-modes '(scroll-bar-mode
                      tool-bar-mode
                      blink-cursor-mode
                      transient-mark-mode))

  (when (not (eq system-type 'darwin))
    (my-disable-mode 'menu-bar-mode))

  (my-enable-modes '(delete-selection-mode
                     column-number-mode
                     savehist-mode
                     global-hi-lock-mode))

  ) ;; end prelude

(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package hi-lock
  :diminish hi-lock-mode)

(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-setup-default))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package ediff
  :init
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)

  (setq ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-diff-options "-w")

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))

  :config
  (use-package ediff-keep))

(use-package sh-script
  :config (setq sh-indentation 2
                sh-basic-offset 2))

(use-package man
  :config (setq Man-width 79))

(use-package which-func
  :init (defun my-echo-which-func ()
          (interactive)
          (message "\u0192: %s" (which-function)))
  :commands (which-function)
  :bind (("C-c f" . my-echo-which-func)))

(use-package whitespace
  :bind (("C-x S" . whitespace-cleanup-save-buffer))
  :diminish ((global-whitespace-mode . "")
             (whitespace-mode . ""))
  :init
  (defun whitespace-cleanup-save-buffer ()
    (interactive)
    (whitespace-cleanup)
    (save-buffer))
  (setq-default whitespace-style '(face trailing tab-mark))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package helm
  :ensure t
  :config

  (use-package helm-descbinds
    :ensure t)

  (use-package helm-ag
    :ensure t)

  (use-package helm-swoop
    :ensure t
    :demand isearch
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-multi-swoop)
           :isearch-mode-map
           ("M-i" . helm-swoop-from-isearch)))

  (use-package helm-config
    :diminish helm-mode
    :demand t
    :bind (("M-x" . helm-M-x)
           ("C-h SPC" . helm-all-mark-rings)
           ("M-Y" . helm-show-kill-ring)
           ("C-x b" . helm-mini)
           ("C-x C-b" . helm-buffers-list)
           ("C-x C-S-b" . ibuffer)
           ("C-x C-f" . helm-find-files)
           ("C-x C-r" . helm-recentf)

           ("C-x C-o" . helm-swoop)
           ("C-x C-M-o" . helm-multi-swoop)

           ("C-x c !" . helm-calcul-expression)
           ("M-:" . helm-eval-expression-with-eldoc)

           ("C-h a" . helm-apropos)
           ("C-h i" . helm-info-emacs)
           ("C-h b" . helm-descbinds)
           ("C-h C-l" . helm-locate-library)
           ("C-c h" . helm-command-prefix))

    :config

    (setq helm-split-window-in-side-p t
          helm-buffers-fuzzy-matching t
          helm-buffer-max-length nil
          helm-recentf-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-ff-file-name-history-use-recentf t
          helm-ff-auto-update-initial-value t
          helm-full-frame nil)

    (add-to-list 'helm-sources-using-default-as-input #'helm-source-man-pages)

    (my-enable-modes '(helm-mode
                       helm-descbinds-mode
                       helm-autoresize-mode
                       helm-flx-mode)))

  (use-package helm-projectile
    :ensure projectile
    :pin melpa-stable
    :bind* (("C-c p D" . projectile-dired)
            ("C-c p v" . projectile-vc)
            ("C-c p k" . projectile-kill-buffers)

            ("C-c p p" . helm-projectile-switch-project)
            ("C-c p f" . helm-projectile-find-file)
            ("C-c p F" . helm-projectile-find-file-in-known-projects)
            ("C-c p g" . helm-projectile-find-file-dwin)
            ("C-c p d" . helm-projectile-find-dir)
            ("C-c p C-r" . helm-projectile-recentf)
            ("C-c p b" . helm-projectile-switch-to-buffer)
            ("C-c p s s" . helm-projectile-ag)
            ("C-c p s g" . helm-projectile-grep)
            )
    :init
    (setq-default projectile-enable-caching t
                  projectile-indexing-method 'alien
                  projectile-completion-system 'helm
                  projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))

    :config
    (projectile-global-mode)
    (helm-projectile-on))

  (use-package helm-themes
    :ensure t
    :if (display-graphic-p)
    :bind ([f9] . helm-themes))
  )
;;; end (use-package helm..)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :if (display-graphic-p)
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package paren
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (add-hook 'haskell-mode-hook 'show-paren-mode)
  (add-hook 'js2-mode-hook 'show-paren-mode)
  (add-hook 'css-mode-hook 'show-paren-mode)
  (add-hook 'sgml-mode-hook 'show-paren-mode))

(use-package elec-pair
  :defer t
  :init
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
  (add-hook 'css-mode-hook 'electric-pair-mode)
  (add-hook 'haskell-mode-hook 'electric-pair-mode))

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'js2-mode-hook 'yas-minor-mode)
  (add-hook 'haskell-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package subword
  :defer t
  :diminish subword-mode
  :init
  (add-hook 'js2-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'subword-mode))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :init
  (setq-default js2-indent-switch-body t
                js2-basic-offset 2
                js-indent-level 2
                js-switch-indent-offset 2
                js2-include-node-externs t
                js2-mode-indent-ignore-first-tab t
                js2-mode-show-parse-errors nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-strict-missing-semi-warning nil
                js2-strict-trailing-comma-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-global-externs '("module" "require" "__dirname" "process" "console" "JSON"))

  (add-hook 'js2-mode-hook (command (highlight-lines-matching-regexp "debugger")))
  (add-hook 'js2-mode-hook (command (highlight-lines-matching-regexp "TODO")))

  :config

  (dolist (mode '(js2-jsx-mode js2-mode))
    (font-lock-add-keywords
     mode `(("\\<\\(function\\)("
             (0 (progn
                  (compose-region
                   (match-beginning 1)
                   (match-end 1) "\u0192")
                  nil)))))
    (font-lock-add-keywords
     mode `(("\\<\\(function\\) .*("
             (0 (progn
                  (compose-region
                   (match-beginning 1)
                   (match-end 1) "\u0192")
                  nil))))))
  )

(use-package tern
  :ensure t
  :defer t
  :diminish tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode))

(use-package js2-refactor
  :ensure t
  :defer t
  :diminish js2-refactor-mode
  :init (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package company
  :ensure t
  :diminish company-mode
  :if (display-graphic-p)
  :config
  (setq company-idle-delay 0.3
        company-dabbrev-downcase nil)
  (global-company-mode)
  (use-package company-tern
    :ensure t
    :config (add-to-list 'company-backends 'company-tern))
  (use-package company-emoji
    :ensure t
    :config (add-to-list 'company-backends 'company-emoji)))

(use-package skewer-mode
  :ensure t
  :diminish ((skewer-mode . "")
             (skewer-css-mode . "")
             (skewer-html-mode . "")))

(use-package emmet-mode
  :ensure t
  :diminish emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-,"     . haskell-move-nested-left)
              ("C-."     . haskell-move-nested-right)
              ("C-c C-." . haskell-mode-format-imports)

              ("s-i"     . haskell-navigate-imports)

              ("C-c C-l" . haskell-process-load-or-reload)
              ("C-`"     . haskell-interactive-bring)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c c"   . haskell-process-cabal))
  :init
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-indent-mode)

  :config
  (defun haskell-mode-before-save-handler ()
    "Function that will be called before buffer's saving."
    (haskell-sort-imports)))

(use-package flycheck
  :ensure t
  :defer t
  :if (display-graphic-p)
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'typescript-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode))


(use-package magit
  :ensure t
  :defer t
  :if (display-graphic-p)
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)

  :config
  (setenv "GIT_PAGER" ""))

(use-package git-messenger
  :ensure t
  :if (display-graphic-p)
  :bind ("C-x g p" . git-messenger:popup-message))

(use-package what-the-commit
  :ensure t
  :bind ("C-x g c" . what-the-commit-insert))

(use-package github-browse-file
  :ensure t
  :bind ("C-x g b" . github-browse-file)
  :init (setq github-browse-file-show-line-at-point t))

(use-package smart-window
  :ensure t
  :if (display-graphic-p)
  :bind (("C-c s m" . smart-window-move)
         ("C-c s s" . smart-window-buffer-split)
         ("C-c s S" . smart-window-file-split)
         ("C-c s r" . smart-window-rotate)
         ("C-c 2" . sw-below)
         ("C-c 3" . sw-right)))

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package hungry-delete
  :ensure t
  :defer t
  :diminish hungry-delete-mode
  :init
  (add-hook 'js2-mode-hook 'hungry-delete-mode)
  (add-hook 'sgml-mode-hook 'hungry-delete-mode)
  (add-hook 'html-mode-hook 'hungry-delete-mode)
  (add-hook 'css-mode-hook 'hungry-delete-mode)
  (add-hook 'emacs-lisp-mode-hook 'hungry-delete-mode))

(use-package css-mode
  :mode "\\.css\\'")

(use-package rainbow-mode
  :ensure t
  :defer t
  :init (add-hook 'css-mode-hook 'rainbow-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :bind (("C-c b" . json-mode-beautify))
  :init (setq-default json-reformat:indent-width 2
                      js-indent-level 2))

(use-package dired
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package peep-dired
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-narrow
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package term
  :bind (:map
         term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map
         term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o h" . helm-info-org)))

(use-package simple
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'toggle-word-wrap))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)))

(use-package tagedit
  :ensure t
  :pin elpa
  :diminish tagedit-mode
  :init (add-hook 'html-mode-hook 'tagedit-mode)
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features))

(use-package 0blayout
  :ensure t
  :diminish 0blayout-mode
  :bind (("C-c , b" . 0blayout-switch)
         ("C-c , c" . 0blayout-new)
         ("C-c , k" . 0blayout-kill))
  :config
  (0blayout-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config (setq typescript-indent-level 2))

(use-package ns-win
  :if (eq system-type 'darwin)
  :config
  (setq mac-option-modifier 'meta
        mac-control-modifier 'control
        mac-right-option-modifier 'super))

(use-package osx-trash
  :ensure t
  :if (eq system-type 'darwin)
  :config (osx-trash-setup))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package tldr
  :ensure t
  :defer t)

(use-package fullframe
  :ensure t
  :if (display-graphic-p)
  :config
  (fullframe magit-status magit-mode-quit-window nil)
  (fullframe projectile-vc magit-mode-quit-window nil))

(use-package flymd
  :ensure t
  :if (display-graphic-p)
  :defer t
  :init
  (defun my-flymd-browser-function (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser-function))

(use-package emoji-display
  :ensure t
  :if (display-graphic-p)
  :config
  (emoji-display-mode))

(use-package emoji-cheat-sheet-plus
  :ensure t
  :if (display-graphic-p)
  :defer t)


(use-package markdown-mode
  :ensure t
  :demand markdown-toc
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :bind (:markdown-mode-map
         ("C-c t" . markdown-toc-generate-toc)))

;; (use-package intero
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'haskell-mode-hook 'intero-mode))

(use-package shift-text
  :ensure t
  :bind (("S-<up>" . shift-text-up)
         ("S-<down>" . shift-text-down)
         ("S-<left>" . shift-text-left)
         ("S-<right>" . shift-text-right)))

(use-package sicp
  :ensure t)

(use-package python-mode
  :mode "\\.py\\'"
  :init (setq python-indent-offset 4))

;;; init.el ends here
