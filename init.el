(require 'package)

(setq elpa '("elpa" . "http://elpa.gnu.org/packages/")
      org '("org" . "http://orgmode.org/elpa/")
      melpa/stable '("melpa-stable" . "http://stable.melpa.org/packages/")
      melpa '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil
      package-archives `(,elpa ,org ,melpa/stable ,melpa)
      )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package defuns
  :load-path "~/.emacs.d/site-lisp")

(add-hook
 'after-init-hook
 (defun my/configure-emacs ()
   (custom-set-faces
    '(default ((t (:height 140 :family "DejaVu Sans Mono" :weight normal)))))
   ))

(if (file-exists-p "~/src/public/remember-last-theme")
    (use-package remember-last-theme
      :load-path "~/src/public/remember-theme"
      :config (remember-last-theme-with-file-enable "~/.emacs-theme"))
  (use-package remember-last-theme
    :ensure t
    :config (remember-last-theme-with-file-enable "~/.emacs-theme")))

(bind-keys*
 ("M-%" . query-replace-regexp)
 ("M-`" . other-frame)
 ("C-M-;" . comment-or-uncomment-region)
 ("C-M-k" . kill-sexp)
 ("C-<tab>" . mode-line-other-buffer)
 ("C-S-<tab>" . next-buffer)
 ("C-;" . comment-line)
 ("C-c q" . delete-other-windows)
 )

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines t)

(setq custom-file (make-temp-file "emacs-custom-")
      custom-buffer-done-kill nil
      custom-buffer-verbose-help nil
      custom-unlispify-names nil
      custom-unlispify-menu-entries nil

      custom-safe-themes t

      frame-title-format '("" "%b @ Emacs " emacs-version)

      gc-cons-threshold 100000000
      large-file-warning-threshold 100000000

      load-prefer-newer t
      require-final-newline t

      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil

      ;; auto-backup (filename~)
      backup-directory-alist `((".*" . ,temporary-file-directory))
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
             (?d . (file . ,(expand-file-name "site-lisp/defuns.el" user-emacs-directory)))
             ))
  (set-register (car r) (cdr r)))

(when (window-system) (unbind-key "C-z"))

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

(my/disable-modes '(scroll-bar-mode transient-mark-mode))

(defun activate-presentation-mode ()
  (interactive)
  (custom-set-faces
   '(default ((t (:height 200)))))
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

(defun activate-development-mode ()
  (interactive)
  (custom-set-faces
   '(default ((t (:height 140 :family "DejaVu Sans Mono" :weight normal)))))
  (tool-bar-mode +1)
  (blink-cursor-mode +1))

(when (not (eq system-type 'darwin))
  (my/disable-mode 'menu-bar-mode))

(my/enable-modes '(delete-selection-mode
                   column-number-mode
                   savehist-mode
                   global-hi-lock-mode
                   semantic-mode))

(use-package editing-extras
  :load-path "~/.emacs.d/site-lisp"
  :bind* (

          ("C-M-S-k" . my/kill-sexp-backwards)
          ("C-x C-v" . my/find-alternate-file-with-sudo)
          ("C-M-s"   . my/isearch-forward-regexp-other-window)
          ("C-M-r"   . my/isearch-backward-regexp-other-window)
          ("C-x C-e" . my/eval-last-sexp)
          ("C-<down>". my/scroll-one-down)
          ("C-<up>"  . my/scroll-one-up)

          ))

(use-package paren :config (show-paren-mode))

(use-package isearch
  :init
  (defun my/isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match.

The arguments NOPUSH and EDIT are passed to the wrapped function `isearch-done'."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end)))
  :bind (:map isearch-mode-map ("C-<return>" . my/isearch-done-opposite)))

(use-package autorevert :diminish auto-revert-mode :config (global-auto-revert-mode))
(use-package hi-lock :diminish hi-lock-mode)

(use-package save-place :ensure t :config (save-place-mode +1))

(use-package ediff
  :init
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

(use-package sh-script :init (setq sh-indentation 2 sh-basic-offset 2) :defer t)

(use-package man :init (setq Man-width 79) :defer t)

(use-package elisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask" . emacs-lisp-mode)))

(use-package smooth-scrolling :ensure t :config (smooth-scrolling-mode))

(use-package whitespace
  :init
  (defun whitespace-cleanup-save-buffer ()
    (interactive)
    (whitespace-cleanup)
    (save-buffer))
  (setq-default whitespace-style '(face trailing tab-mark))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :bind (("C-x S" . whitespace-cleanup-save-buffer))
  :diminish ((global-whitespace-mode . "") (whitespace-mode . "")))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h b" . counsel-descbinds)
         ("C-h C-l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)

         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-recentf)
         ("C-x l" . counsel-locate)
         ("C-M-y" . counsel-yank-pop)

         ([f9] . counsel-load-theme)
         ))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "%d/%d ")
  :bind (("M-s" . swiper)
         ("C-c C-r" . ivy-resume)
         :map isearch-mode-map
         ("M-s" . swiper-from-isearch)
         )
  :config (ivy-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line ""
        projectile-remember-windows-config t
        projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-on))

(use-package dockerfile-mode :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :if (display-graphic-p)
  :init (add-hook 'emacs-lisp-mode 'rainbow-delimiters-mode))

(use-package emmet-mode
  :ensure t
  :init (add-hook 'html-mode 'emmet-mode))

(use-package elnode :ensure t)

(use-package elec-pair
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'electric-pair-mode)
  (add-hook 'scala-mode-hook 'electric-pair-mode)
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
  (add-hook 'elm-mode-hook 'electric-pair-mode)
  )

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

(use-package ace-window
  :ensure t
  :init (defun other-window-backwards ()
          (interactive)
          (other-window -1))
  :bind (("M-o" . other-window)
         ("M-O" . other-window-backwards)))

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

(use-package subword
  :defer t
  :diminish subword-mode
  :init
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'scala-mode-hook 'subword-mode)
  (add-hook 'elm-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook 'subword-mode)
  )

(use-package sml-mode
  :ensure t
  :mode "\\.sml\'")

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
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  :config
  (defun haskell-mode-before-save-handler ()
    "Function that will be called before buffer's saving."
    (when (projectile-project-p)
      (haskell-mode-stylish-buffer)
      (haskell-sort-imports))))

(use-package flycheck-haskell
  :ensure t
  :init (eval-after-load 'flycheck
          '(add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)))

(use-package flycheck
  :ensure t
  :if (display-graphic-p))


(use-package gitignore-mode :ensure t)
(use-package github-pullrequest :ensure t)
(use-package git-messenger
  :ensure t
  :if (display-graphic-p)
  :bind ("C-x g p" . git-messenger:popup-message))
(use-package git-timemachine
  :ensure t
  :bind ("C-x g t" . git-timemachine-toggle))
(use-package what-the-commit
  :ensure t
  :bind ("C-x g c" . what-the-commit-insert))
(use-package github-browse-file
  :ensure t
  :bind ("C-x g b" . github-browse-file)
  :init (setq github-browse-file-show-line-at-point t))
(use-package magit
  :ensure t
  :defer t
  :pin melpa-stable
  :if (display-graphic-p)
  :init
  (when (functionp 'ivy-completing-read)
    (setq magit-completing-read-function 'ivy-completing-read))
  (add-hook 'magit-mode-hook 'hl-line-mode)

  :config
  (setenv "GIT_PAGER" ""))

(use-package smart-window
  :ensure t
  :if (display-graphic-p)
  :demand t
  :bind (("C-c s m" . smart-window-move)
         ("C-c s s" . smart-window-buffer-split)
         ("C-c s S" . smart-window-file-split)
         ("C-c s r" . smart-window-rotate)
         ("C-c 2"   . sw-below)
         ("C-c 3"   . sw-right)))

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(use-package hungry-delete
  :ensure t
  :defer t
  :diminish hungry-delete-mode
  :init
  (add-hook 'haskell-mode-hook 'hungry-delete-mode)
  (add-hook 'emacs-lisp-mode-hook 'hungry-delete-mode)
  (add-hook 'scala-mode-hook 'hungry-delete-mode)
  )

(use-package css-mode
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init (add-hook 'css-mode-hook 'rainbow-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
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
(use-package dired+ :ensure t)
(use-package dired-subtree :ensure t)

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
         )
  :demand t
  :init
  (setq org-agenda-files '("~/Documents")
        org-default-notes-file "~/Documents/notes.org"
        org-src-fontify-natively t
        )
  (eval-after-load "org" '(require 'ox-md nil t)))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)))

(use-package 0blayout
  :ensure t
  :diminish 0blayout-mode
  :bind (("C-c , b" . 0blayout-switch)
         ("C-c , c" . 0blayout-new)
         ("C-c , k" . 0blayout-kill))
  :config (0blayout-mode))

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

(use-package markdown-edit-indirect :ensure t :defer t)
(use-package markdown-toc :ensure t :defer t)
(use-package flymd :ensure t
  :init (setq flymd-browser-open-function (defun my-flymd-browser-function (url)
                                            (let ((process-environment (browse-url-process-environment)))
                                              (apply 'start-process
                                                     (concat "firefox " url)
                                                     nil
                                                     "/usr/bin/open"
                                                     (list "-a" "firefox" url))))))
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook (defun configure-markdown-mode ()
                                  (interactive)
                                  (local-set-key (kbd "C-c '") 'markdown-edit-indirect)
                                  (local-set-key (kbd "C-c t") 'markdown-toc-generate-toc)))
  :mode (("\\.md" . gfm-mode)
         ("\\.markdown" . gfm-mode)))

(use-package python-mode
  :mode "\\.py\\'"
  :init (setq python-indent-offset 2))

(use-package yasnippet
  :if (display-graphic-p)
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init (setq yas-indent-line nil)
  :config (yas-global-mode))

(use-package exec-path-from-shell
  :ensure t
  :init (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  (setenv "SUPPRESS_NO_CONFIG_WARNING" "yes"))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :init
  (setq ;projectile-switch-project-action 'neotree-projectile-action
        neo-theme (if window-system 'icons 'arrow)
        neo-window-position 'right
        neo-smart-open t
        neo-window-width 30
        neo-window-fixed-size t
        neo-auto-indent-point t)

  (defun neotree-project-toggle ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (if (neotree-toggle)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  )

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'")

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :init
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :ensure t
  :init (eval-after-load 'flycheck
          '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :init (setq all-the-icons-scale-factor 0.8))
(use-package all-the-icons-ivy
  :ensure t
  :config (all-the-icons-ivy-setup))
(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package mac :if (eq system-type 'darwin))

(use-package wgrep :ensure t)
(use-package wgrep-ag :ensure t)

(use-package mykie
  :ensure t
  :init
  (defun my-kill-buffer-and-file (&optional buffer-or-name)
    "Kill BUFFER-OR-NAME and its associated file.
If BUFFER-OR-NAME is not specified the current buffer is used."
    (interactive
     (list (read-buffer (format "Kill buffer and its file (default %s): "
                                (buffer-name (current-buffer))))))
    (let* ((buffer (get-buffer buffer-or-name))
           (filename (buffer-file-name buffer)))
      (kill-buffer buffer)
      (when filename
        (delete-file filename))))

  :config
  (mykie:set-keys nil
    "C-x k"
    :default kill-buffer
    :C-u     my-kill-buffer-and-file
    ))

(use-package untitled-new-buffer
  :ensure t
  :bind (("M-N" . untitled-new-buffer-with-select-major-mode)))

(use-package cc-mode
  :defer t
  :init
  (add-hook 'java-mode-hook (defun my/java-mode-setup ()
                              (setq c-basic-offset 2
                                    tab-width 2
                                    indent-tabs-mode nil))))

(use-package ensime
  :ensure t
  :pin melpa
  :diminish ensime-mode
  :config (setq ensime-startup-notification nil
                ensime-startup-snapshot-notification nil))
(use-package sbt-mode
  :ensure t
  :pin melpa)
(use-package scala-mode
  :ensure t
  :pin melpa
  :mode (("\\.scalaX?\\'" . scala-mode)
         ("\\.scX?\\'" . scala-mode)))

;; themes
(advice-add
 'load-theme
 :after
 (defun my/apply-theme-customizations (theme &optional no-cofirm no-enable)
   (let ((customize-theme (intern (format "customize-%s-theme" theme))))
     (when (functionp customize-theme)
       (funcall customize-theme)))))

(use-package darcula-theme :ensure t
  :defer t
  :init
  (defun customize-darcula-theme ()
    (custom-theme-set-faces
     'darcula
     '(isearch ((t (:background "#214283" :box (:line-width -1 :color "#bbbbbb")))))
     '(font-lock-constant-face ((t (:foreground "#6897bb" :italic t :weight bold :inherit 'font-lock-variable-name-face))))
     )))

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
(use-package avk-emacs-themes :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package ir-black-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package goose-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package github-theme :ensure t :defer t)
