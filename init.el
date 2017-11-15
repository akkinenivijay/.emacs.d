(require 'package)
(setq package-archives '(
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

(bind-keys*
 ("M-%" . query-replace-regexp)
 ("M-`" . other-frame)
 ("M-z" . just-one-space)
 ("C-M-;" . comment-or-uncomment-region)
 ("M-M" . man)
 ("C-M-k" . kill-sexp)
 ("C-<tab>" . mode-line-other-buffer)
 ("C-<tab>" . mode-line-other-buffer)
 ("C-S-<tab>" . next-buffer)
 ("C-;" . comment-line)
 ("C-c q" . delete-other-windows)
 )

(add-to-list
 'custom-theme-load-path
 (expand-file-name "themes" user-emacs-directory))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines nil)

(add-hook
 'after-init-hook
 (defun my/set-faces ()
   (custom-set-faces
    '(default ((t (:height 150 :family "Monaco" :weight normal)))))
   ))

(setq custom-file (make-temp-file "emacs-custom-")
      custom-buffer-done-kill nil
      custom-buffer-verbose-help nil
      custom-unlispify-names nil
      custom-unlispify-menu-entries nil

      initial-scratch-message nil
      ;; initial-frame-alist `((scroll-bar-background . nil))
      ;; default-frame-alist `((scroll-bar-background . nil))

      custom-safe-themes t
      enable-local-variables t

      frame-title-format '("" "%b @ Emacs " emacs-version)

      gc-cons-threshold 100000000
      large-file-warning-threshold 100000000

      load-prefer-newer t
      require-final-newline t

      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil

      make-backup-files nil
      ;; auto-backup (filename~)
      ;; backup-directory-alist `((".*" . ,temporary-file-directory))
      ;; backup-by-copying t
      ;; delete-old-versions t
      ;; version-control t

      ;; auto-save (#filename#)
      ;; auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      confirm-kill-emacs nil ;#'y-or-n-p

      echo-keystrokes 0.2
      ring-bell-function #'ignore

      set-mark-command-repeat-pop t

      inhibit-default-init t
      inhibit-startup-screen t
      ;; initial-scratch-message nil

      scroll-margin 2
      scroll-preserve-screen-position t)

(put 'var 'safe-local-variable (lambda [& rest] true))

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (r `((?i . (file . ,(expand-file-name "init.el" user-emacs-directory)))
             (?c . (file . ,(expand-file-name "custom.el" user-emacs-directory)))))
  (set-register (car r) (cdr r)))

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

(when (display-graphic-p)
  (unbind-key "C-z"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(unless (display-graphic-p)
  (menu-bar-mode -1))
(transient-mark-mode -1)
(delete-selection-mode)

(use-package paren :config (show-paren-mode))
(use-package isearch
  :bind (:map isearch-mode-map
              ("C-<return>" . isearch-done-opposite)
              ("M-i" . helm-swoop-from-isearch))
  :init (defun isearch-done-opposite (&optional nopush edit)
          "End current search in the opposite side of the match."
          (interactive)
          (funcall #'isearch-done nopush edit)
          (when isearch-other-end (goto-char isearch-other-end)))
  )
(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))
(use-package hi-lock :diminish hi-lock-mode)
(use-package saveplace :config (save-place-mode))

(use-package centered-window-mode
  :ensure t
  :config (setq cwm-incremental-padding t))

(use-package discover
  :ensure t
  :config (global-discover-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (:map undo-tree-map
              ("C-?" . undo-tree-redo)
              ("C-/" . undo-tree-undo)
              ("C-x u" . undo-tree-visualize)
              )
  :config (global-undo-tree-mode))

(use-package discover-my-major
  :ensure t
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-S-m" . discover-my-mode)))

(use-package ediff
  :config
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)

  (setq
   ;; ediff-window-setup-function 'ediff-setup-windows-plain
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
  :mode "\\.sh\\'"
  :config (setq sh-indentation 2 sh-basic-offset 2))

(use-package man :config (setq Man-width 79) :defer t)

(use-package elisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask" . emacs-lisp-mode)))

(use-package smooth-scrolling
  :ensure t
  :if (display-graphic-p)
  :config (smooth-scrolling-mode))

(use-package whitespace
  :bind (("C-x S" . whitespace-cleanup-save-buffer))
  :diminish ((global-whitespace-mode . "") (whitespace-mode . ""))
  :init
  (defun whitespace-cleanup-save-buffer ()
    (interactive)
    (whitespace-cleanup)
    (save-buffer))
  (setq-default whitespace-style '(face trailing tab-mark))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package electric :config (add-hook 'prog-mode-hook 'electric-indent-mode))
(use-package elec-pair :config (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile")

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode))

(use-package rainbow-delimiters
  :ensure t
  :if (display-graphic-p)
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package avy
  :ensure t
  :if (display-graphic-p)
  :bind (("M-g M-g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-'" . avy-goto-char)
         ("C-M-'" . avy-goto-char-2)))

(use-package hl-todo
  :ensure t
  :bind (("C-c t n" . hl-todo-next)
         ("C-c t p" . hl-todo-previous)
         ("C-c t o" . hl-todo-occur))
  :demand t
  :config (global-hl-todo-mode))

(use-package ace-window
  :ensure t
  :if (display-graphic-p)
  :init (defun other-window-backwards ()
          (interactive)
          (other-window -1))
  :bind (("M-o" . other-window)
         ("M-O" . other-window-backwards)))

(use-package multiple-cursors
  :ensure t
  :if (display-graphic-p)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :ensure t
  :if (display-graphic-p)
  :bind ("C-@" . er/expand-region))

(use-package subword
  :diminish subword-mode
  :if (display-graphic-p)
  :config
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'scala-mode-hook 'subword-mode)
  (add-hook 'elm-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook 'subword-mode)
  )

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (add-hook 'haskell-mode-hook 'hungry-delete-mode)
  (add-hook 'scala-mode-hook 'hungry-delete-mode)
  (add-hook 'elm-mode-hook 'hungry-delete-mode)
  (add-hook 'js2-mode-hook 'hungry-delete-mode)
  )

(use-package sml-mode
  :ensure t
  :mode "\\.sml\'")

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (defun haskell-mode-before-save-handler ()
    "Function that will be called before buffer's saving."
    (when (projectile-project-p)
      (haskell-mode-stylish-buffer)
      (haskell-sort-imports)))
  )

(use-package flycheck
  :ensure t
  :if (display-graphic-p))

(use-package gitignore-mode :ensure t :mode ".gitignore'")
(use-package git-messenger
  :ensure t
  :if (display-graphic-p)
  :bind ("C-x g p" . git-messenger:popup-message))
(use-package git-timemachine
  :ensure t
  :if (display-graphic-p)
  :bind ("C-x g t" . git-timemachine-toggle))
(use-package what-the-commit
  :ensure t
  :bind ("C-x g c" . what-the-commit-insert))
(use-package github-browse-file
  :ensure t
  :bind ("C-x g b" . github-browse-file)
  :config (setq github-browse-file-show-line-at-point t))
(use-package magit
  :ensure t
  :pin melpa-stable
  :if (display-graphic-p)
  :config
  (when (functionp 'ivy-completing-read)
    (setq magit-completing-read-function 'ivy-completing-read))
  (add-hook 'magit-mode-hook 'hl-line-mode))

(use-package css-mode
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

;; (use-package rainbow-mode
;;   :ensure t
;;   :mode "\\.css\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config (setq-default json-reformat:indent-width 2
                        js-indent-level 2))

(use-package osx-trash
  :ensure t
  :if (eq system-type 'darwin)
  :config (osx-trash-setup))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package tldr :ensure t)

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package markdown-mode
  :ensure t
  :mode (("\\.md" . gfm-mode)
         ("\\.markdown" . gfm-mode))
  :init
  (add-hook
   'markdown-mode-hook
   (defun configure-markdown-mode ()
     (interactive)
     (use-package markdown-edit-indirect :ensure t :defer t)
     (use-package markdown-toc :ensure t :defer t)
     (local-set-key (kbd "C-c '") 'markdown-edit-indirect)
     (local-set-key (kbd "C-c t") 'markdown-toc-generate-toc)))
  )

(use-package python-mode
  :mode "\\.py\\'"
  :init (setq python-indent-offset 2))

(use-package yasnippet
  :ensure t
  :defer 2
  :diminish yas-minor-mode
  :if (display-graphic-p)
  :init (setq yas-indent-line t)
  :config (yas-global-mode))

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'")

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :init
  (setq elm-format-on-save t))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package gradle-mode
  :ensure t
  :mode "\\.gradle\\'")

(use-package server
  :config
  (setq server-socket-dir "~/.emacs.d/server")
  (unless (server-running-p) (server-mode)))

(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-setup-default))

(use-package fancy-narrow
  :ensure t
  :diminish fancy-narrow-mode
  :if (display-graphic-p)
  :config (fancy-narrow-mode))

(use-package dired+ :ensure t)
(use-package dired-explorer :ensure t)
(use-package dired-imenu :ensure t)
(use-package dired-k :ensure t :bind (:map dired-mode-map ("K" . dired-k)))

(use-package mac :if (eq system-type 'darwin))

(use-package wgrep :ensure t)
(use-package wgrep-ag :ensure t)

(use-package untitled-new-buffer
  :ensure t
  :bind (("M-N" . untitled-new-buffer-with-select-major-mode)))

(use-package helm
  :ensure t
  :pin melpa-stable
  :if (display-graphic-p)
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-Y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-S-b" . ibuffer)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)

         ("C-x c !" . helm-calcul-expression)
         ("M-:" . helm-eval-expression-with-eldoc)

         ("C-h a" . helm-apropos)
         ("C-h i" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-c h i" . helm-semantic-or-imenu)
         )
  :config
  (setq helm-command-prefix-key "C-c h"
        helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-buffer-max-length nil
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-ff-auto-update-initial-value t
        helm-full-frame nil
        )
  (helm-mode)
  )

(use-package helm-descbinds
  :ensure t
  :if (display-graphic-p)
  :bind (("C-h b" . helm-descbinds))
  )

(use-package helm-ag :ensure t :if (display-graphic-p) :defer t)
(use-package helm-tramp :ensure t :if (display-graphic-p) :defer t)

(use-package helm-themes
  :ensure t
  :if (display-graphic-p)
  :bind ("C-c h t" . helm-themes))

(use-package helm-swoop
  :ensure t
  :if (display-graphic-p)
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop)))

(use-package helm-projectile
  :ensure t
  :if (display-graphic-p)
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
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode)
  (helm-projectile-on))

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package golden-ratio :ensure t)

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                js2-missing-semi-one-line-override nil
                js2-bounce-indent-p nil))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  )

(use-package aggressive-indent
  :ensure t
  :config
  (setq aggressive-indent-sit-for-time 0.5)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package prog-mode
  :config
  (setq clojure--prettify-symbols-alist '(("fn" . 955)
                                          ))
  (add-hook 'clojure-mode-hook 'prettify-symbols-mode))

(use-package idris-mode
  :ensure t)

(use-package helm-idris
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :bind (:map clojure-mode-map
              ("C-c C-;" . cider-eval-defun-to-comment)
              ("C-c C-SPC" . cider-format-buffer))
  :config
  (setq cider-lein-parameters-default cider-lein-parameters
        cider-lein-parameters (concat "with-profile test " cider-lein-parameters-default)))
(use-package helm-cider :ensure t :config (helm-cider-mode))
(use-package 4clojure :ensure t)
(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :config
  (setq clojure-indent-style ':align-arguments
        cider-cljs-lein-repl (paredit-unescape-string
                              (with-output-to-string
                               (print '(cond
                                        (and (resolve 'user/run)
                                             (resolve 'user/browser-repl))
                                        (eval '(do (user/run)
                                                   (user/browser-repl)))

                                        (try
                                         (require '[figwheel-sidecar.repl-api :as sidecar])
                                         (resolve 'sidecar/start-figwheel!)
                                         (catch Throwable _))
                                        (eval '(do (sidecar/start-figwheel!)
                                                   (sidecar/cljs-repl)))

                                        (try
                                         (require '[cemerick.piggieback :as piggie])
                                         (resolve 'piggie/cljs-repl)
                                         (catch Throwable _))
                                        (eval '(piggie/cljs-repl (cljs.repl.rhino/repl-env)))

                                        :else
                                        (throw (ex-info "Failed to initialize CLJS repl.")))))))
  (define-clojure-indent
    (defcomponent '(2 nil nil (:defn)))))

(use-package hideshow
  :config
  (add-hook 'clojure-mode-hook 'hs-minor-mode))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        ))

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")

(use-package cc-mode
  :config
  (add-hook 'java-mode-hook (defun my/java-mode-setup ()
                              (setq c-basic-offset 2
                                    tab-width 2
                                    indent-tabs-mode nil))))


(use-package scala-mode
  :ensure t
  :mode (("\\.scalaX?\\'" . scala-mode)
         ("\\.scX?\\'" . scala-mode)))

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package remember-last-theme
  :ensure t
  :if (display-graphic-p)
  :config (remember-last-theme-with-file-enable (expand-file-name "last-theme.el" user-emacs-directory)))

(use-package cmake-ide
  :ensure rtags
  :config (cmake-ide-setup))

(use-package smart-window
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package atom-one-dark-theme :ensure t :defer t)
(use-package birds-of-paradise-plus-theme :ensure t :defer t)
(use-package bliss-theme :ensure t :defer t)
(use-package borland-blue-theme :ensure t :defer t)
(use-package cyberpunk-theme :ensure t :defer t)
(use-package eclipse-theme :ensure t :defer t)
(use-package espresso-theme :ensure t :defer t)
(use-package faff-theme :ensure t :defer t)
(use-package greymatters-theme :ensure t :defer t)
(use-package idea-darkula-theme :ensure t :defer t)
(use-package minimal-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)
(use-package noctilux-theme :ensure t :defer t)
(use-package railscasts-theme :ensure t :defer t)
(use-package rebecca-theme :ensure t :defer t)
(use-package soothe-theme :ensure t :defer t)
(use-package subatomic-theme :ensure t :defer t)
(use-package white-theme :ensure t :defer t)
(use-package madhat2r-theme :ensure t :defer t)
(use-package kosmos-theme :ensure t :defer t)
(use-package plain-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package paganini-theme :ensure t :defer t)
(use-package nord-theme :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package ir-black-theme :ensure t :defer t)
(use-package goose-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package github-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package leuven-theme :ensure t :defer t)
(use-package sublime-themes :ensure t :defer t)
(use-package paper-theme :ensure t :defer t)
(use-package tao-theme :ensure t :defer t)
(use-package organic-green-theme :ensure t :defer t)
(use-package clues-theme :ensure t :defer t)
(use-package spacegray-theme :ensure t :defer t)
(use-package soft-morning-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package paganini-theme :ensure t :defer t)
(use-package nord-theme :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package ir-black-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package goose-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package github-theme :ensure t :defer t)
(use-package dracula-theme :ensure t :defer t)
(use-package afternoon-theme :ensure t :defer t)
(use-package avk-emacs-themes :ensure t :defer t)
(use-package grayscale-theme :ensure t :defer t)
(use-package sunburn-theme :ensure t :defer t)
(use-package hemera-theme :ensure t :defer t)
(use-package basic-theme :ensure t :defer t)
