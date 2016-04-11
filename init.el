;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-

;;; Commentary:

;; Personal emacs configuration.
;; npm i -g tern eslint babel-eslint eslint-plugin-react

;;; Code:
(require 'package)

(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

(setq-default tab-width 4)

(use-package my-prelude
  :load-path "~/.emacs.d/lisp"
  :bind (("s-k" . my-copy-buffer-file-name))
  :demand t
  :config
  ;; enable dangerous commands
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
                      menu-bar-mode
                      blink-cursor-mode
                      transient-mark-mode)))

(setq-default indent-tabs-mode nil
              tab-width 8)
(setq gc-cons-threshold 50000000
      large-file-warning-threshold 100000000

      load-prefer-newer t
      require-final-newline t

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

      inhibit-startup-message t
      initial-scratch-message nil

      scroll-margin 2
      scroll-preserve-screen-position t)

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (r `((?i . (file . ,(expand-file-name "init.el" user-emacs-directory)))
             (?p . (file . ,(expand-file-name "lisp/my-prelude.el" user-emacs-directory)))
             (?s . (file . ,(expand-file-name "snippets" user-emacs-directory)))))
  (set-register (car r) (cdr r)))

(bind-keys ("M-U"        . (command (upcase-word -1)))
           ("M-%"        . query-replace-regexp)
           ("C-M-;"      . comment-or-uncomment-region)
           ("C-<tab>"    . mode-line-other-buffer)
           ("C-<up>"     . my-switch-to-next-buffer)
           ("C-<down>"   . my-switch-to-previous-buffer)
           ("s-<down>"   . shrink-window)
           ("s-M-<up>"   . enlarge-window-horizontally)
           ("s-M-<down>" . shrink-window-horizontally)
           ("C-x a a"    . align)
           ("C-x a r"    . align-regexp)
           ("C-c w"      . delete-region))

(use-package my-editing-defuns
  :load-path "~/.emacs.d/lisp"
  :bind (("M-W" . my-copy-line-as-kill)
         ("s-M-k" . my-kill-sexp-backwards)
         ("C-;" . my-comment-or-uncomment-line)
         ("C-M-s" . my-isearch-forward-regexp-other-window)
         ("C-M-r" . my-isearch-backward-regexp-other-window)
         ("C-x C-e" . my-eval-last-sexp)
         ("C-c o o" . prelude-open-with)))

;; (use-package iedit
;;   :ensure t
;;   :init (setq iedit-toggle-key-default (kbd "M-s-;")))

(use-package autorevert
  :diminish global-auto-revert-mode
  :config (my-enable-mode 'global-auto-revert-mode))

(use-package prog-mode
  :diminish global-prettify-symbols-mode
  :config (my-enable-mode 'global-prettify-symbols-mode))

(use-package which-func
  :config (bind-key "s-f" (command (message (which-function)))))

(my-enable-modes '(delete-selection-mode
                   column-number-mode
                   savehist-mode
                   global-hi-lock-mode))

(use-package mykie
  :ensure t
  :config
  (setq mykie:use-major-mode-key-override t)
  (mykie:initialize)

  (mykie:global-set-key "C-x k"
    :default kill-buffer
    :C-u     my-kill-buffer-and-file)

  (mykie:global-set-key "C-c r"
    :default rename-buffer
    :C-u     my-rename-buffer-and-file))

(use-package window-numbering
  :ensure t
  :config (my-enable-mode 'window-numbering-mode))

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("C-M-1" . zygospore-toggle-delete-other-windows)))

(use-package linkd
  :load-path "lisp/linkd"
  :init (setq linkd-use-icons t
              linkd-icons-directory "~/.emacs.d/lisp/linkd/icons"))

;; (use-package auto-highlight-symbol
;;   :ensure t
;;   :config (my-enable-mode 'global-auto-highlight-symbol-mode))

(use-package align :ensure t)
(use-package auth-password-store :ensure t)
(use-package web-beautify :ensure t)

(use-package nvm
  :ensure t
  :config
  (nvm-use "v5.8.0")
  (add-to-list 'exec-path (getenv "NVM_BIN")))

(use-package sh-script
  :config (setq sh-indentation 2
                sh-basic-offset 2))

(use-package dockerfile-mode :ensure t)
(use-package docker :ensure t)
(use-package docker-tramp :ensure t)

(use-package persistent-scratch
  :ensure t
  :config (persistent-scratch-setup-default))

(use-package discover
  :ensure t
  :config (my-enable-mode 'discover-mode))

(use-package man
  :config (setq Man-width 79))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package hl-line
  :init (unbind-key [f3])
  :bind ([f3] . global-hl-line-mode))

(use-package compile
  :config (setq compilation-ask-about-save nil
                compilation-always-kill t
                compilation-scroll-output 'first-error))

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-diff-options "-w"))

(use-package bookmark
  :config (setq bookmark-save-flag 1))

(use-package savehist
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
  :init (setq-default save-place t))

(use-package tramp
  :config (setq tramp-default-method "ssh"))

;; (use-package server
;;   :config
;;   (setq server-use-tcp t)
;;   (unless (server-running-p)
;;     (server-start)))

(use-package cask-mode :ensure t)
(use-package restclient-helm :ensure t)

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config
  (setq yas-wrap-around-region t)
  (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap)
  (yas-reload-all))

(use-package magit
  :pin melpa-stable
  :ensure t
  :init (setq magit-last-seen-setup-instructions "1.4.0"))
(use-package git-messenger
  :ensure t
  :bind ("C-x v p" . git-messenger:popup-message))
(use-package git-timemachine
  :ensure t)
(use-package what-the-commit
  :ensure t
  :bind ("s-c" . what-the-commit-insert))
(use-package github-browse-file
  :ensure t
  :init (setq github-browse-file-show-line-at-point t))

(use-package swap-regions
  :ensure t
  :bind (("C-c C-t" . swap-regions)))

(use-package fullframe
  :ensure t
  :config
  (fullframe projectile-vc magit-mode-quit-window nil))

(use-package eshell
  :config
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t))

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package multiple-cursors
  :ensure t

  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("M-g w" . avy-goto-word-0)
         ("M-g M-g" . avy-goto-line)))

(use-package helm :ensure t :pin melpa-stable)
(use-package helm-descbinds :ensure t :pin melpa-stable)
(use-package helm-swoop :ensure t :pin melpa-stable)
(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :demand t
  :defer t
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
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

  :init (setq helm-split-window-in-side-p t
              helm-buffers-fuzzy-matching t
              helm-recentf-fuzzy-match t
              helm-apropos-fuzzy-match t
              helm-move-to-line-cycle-in-source t
              helm-ff-search-library-in-sexp t
              helm-ff-file-name-history-use-recentf t
              helm-ff-auto-update-initial-value t)

  :config
  (add-to-list 'helm-sources-using-default-as-input #'helm-source-man-pages)

  (my-enable-modes '(helm-mode
                     helm-descbinds-mode
                     helm-autoresize-mode
                     helm-flx-mode)))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-o" . helm-swoop))

  :init
  (use-package my-isearch-defuns)
  (bind-keys :map isearch-mode-map

             ("C-<return>" . my-isearch-done-opposite)
             ("M-o" . helm-swoop-from-isearch)
             ("C-h b" . helm-descbinds)))

(use-package projectile
  :ensure t
  :bind (("C-c p D" . projectile-dired)
         ("C-c p v" . projectile-vc)
         ("C-c p k" . projectile-kill-buffers)
         ("C-c p f" . projectile-find-file))

  :init
  (setq projectile-enable-caching t)
  (add-hook 'projectile-mode-hook (defun my-helm-projectile-setup ()
                                    (interactive)
                                    (setq projectile-completion-system 'helm)
                                    (helm-projectile-on)))
  :config
  (projectile-load-known-projects)
  (my-enable-mode 'projectile-global-mode))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config (setq neo-theme 'ascii))

(use-package helm-projectile
    :ensure t
    :bind (("C-c p p" . helm-projectile-switch-project)
           ("C-c p s s" . helm-projectile-ag)
           )

    :init
    (setq projectile-enable-caching t)

    (use-package helm :ensure t)
    (use-package wgrep-helm :ensure t)
    (use-package helm-ag :ensure t))


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

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :interpreter "emacs"
  :bind (("C-x C-e" . pp-eval-last-sexp))
  :init
  (fset 'my-emacs-lisp-mode-hook (defun my-elisp-setup ()
                                   (interactive)
                                   (my-enable-modes '(paredit-mode
                                                      hungry-delete-mode
                                                      rainbow-delimiters-mode
                                                      eldoc-mode
                                                      show-paren-mode))))
  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :init (add-hook 'clojure-mode-hook (defun my-clojure-setup ()
                                       (interactive)
                                       (my-enable-modes '(subword-mode
                                                          paredit-mode
                                                          clj-refactor-mode
                                                          rainbow-delimiters-mode))))
  :config
  (use-package cljr-refactor
    :config (cljr-add-keybindings-with-prefix "C-c C-j")))

(use-package ggtags
  :ensure t)

(use-package pyvenv :ensure t :pin melpa-stable :mode "\\.py\\'")
(use-package python-info :ensure t)
(use-package python-mode
  :mode "\\.py\\'"
  :init (add-hook
         'python-mode-hook
         (defun my-python-setup ()
           (interactive)
           (setq python-python-command "python3"
                 flycheck-python-pycompile-executable "python3"
                 python-shell-interpreter "ipython"
                 python-indent-offset 4)

           (setq-local whitespace-style (append
                                         whitespace-style
                                         '(face lines-tail)))
           (let ((maxcol 99))
             (setq-local whitespace-line-column maxcol)
             (setq fill-column maxcol
                   flycheck-flake8-maximum-line-length maxcol)
             )
           (my-enable-modes '(flycheck-mode
                              ggtags-mode
                              hungry-delete-mode
                              electric-pair-mode
                              subword-mode
                              show-paren-mode))
           (highlight-lines-matching-regexp "i?pdb\."))))

(use-package helm-hoogle
  :ensure t
  :bind ("C-c h g" . helm-hoogle))

(use-package helm-hayoo
  :ensure t
  :bind ("C-c h y" . helm-hayoo))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init (add-hook
         'haskell-mode-hook
         (defun my-haskell-setup ()
           (interactive)
           (my-enable-modes '(subword-mode
                              electric-pair-mode
                              flycheck-mode
                              wrap-region-mode
                              haskell-doc-mode
                              interactive-haskell-mode
                              haskell-indentation-mode))))

  :config (setq haskell-process-suggest-remove-import-lines  t
                haskell-process-auto-import-loaded-modules t
                haskell-process-log t
                haskell-stylish-on-save t)
  (bind-keys
   :map haskell-mode-map
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
   ("C-c c"   . haskell-process-cabal)))

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'"
  :init
  (add-hook 'purescript-mode-hook (defun my-purescript-setup ()
                                    (interactive)
                                    (my-enable-modes '(purescript-decl-scan-mode
                                                       electric-pair-mode
                                                       flycheck-mode))
                                    (turn-on-purescript-indentation)
                                    (flycheck-select-checker 'pulp)
                                    ))
  (use-package flycheck
    :ensure t
    :config

    (flycheck-define-checker pulp
      "Use Pulp to flycheck PureScript code."
      :command ("pulp" "--monochrome" "build")
      :error-patterns
      ((error line-start
              (or  (and "Error at " (file-name) " line " line ", column " column
                        (one-or-more not-newline)
                        (message (one-or-more (not (in "*")))))

                   (and "psc: " (one-or-more not-newline) "\n"
                        (message (one-or-more not-newline) "\n")
                        "at \"" (file-name) "\" (line " line ", column " column ")")
                   (and "Unable to parse module:\n"
                        "  \"" (file-name) "\" (line " line ", column " column "):\n"
                        (message (one-or-more not-newline) "\n"
                                 (one-or-more not-newline) "\n"
                                 (one-or-more not-newline) "\n"))
                   )

              line-end
              ))
      :modes (purescript-mode))))

(use-package psci
  :ensure t
  :init (add-hook 'purescript-mode-hook 'inferior-psci-mode))

(use-package repl-toggle
  :ensure t
  :init (setq rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
                                     (purescript-mode . psci))))

(use-package flycheck :ensure t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :init (add-hook 'json-mode-hook (defun my-json-setup ()
                                    (interactive)
                                    (setq json-reformat:indent-width 2
                                          js-indent-level 2)
                                    (my-enable-modes '(electric-pair-mode)))))

(use-package web-mode
  :ensure t
  :init (add-hook 'web-mode-hook (defun my-web-mode-setup ()
                                   (interactive)
                                   (my-enable-modes '(electric-pair-mode)))))


;; (use-package web-mode
;;   :mode "\\.js[x]?\\'"
;;   :init (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
;;               web-mode-code-indent-offset 2))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-jsx-mode)
  :init
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  (setq-default js2-include-node-externs t)
  (setq-default js2-mode-indent-ignore-first-tab t)
  (setq-default js2-mode-show-parse-errors nil)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-strict-var-hides-function-arg-warning nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning nil)
  (setq-default js2-strict-cond-assign-warning nil)
  (setq-default js2-strict-var-redeclaration-warning nil)
  (setq-default js2-global-externs
                '("module" "require" "__dirname" "process" "console" "define"
                  "JSON" "$" "_" "Backbone" "buster" "sinon" "moment" "_gaq"
                  "Zenbox" "Mousetrap" "Comoyo"))

  (add-hook 'js2-jsx-mode-hook (defun my-js-setup ()
                                 (interactive)
                                 (use-package js2-refactor
                                   :config (js2r-add-keybindings-with-prefix "C-c C-j"))

                                 (highlight-lines-matching-regexp "debugger")

                                 (my-enable-modes '(subword-mode
                                                    hungry-delete-mode
                                                    wrap-region-mode
                                                    js2-refactor-mode
                                                    electric-pair-mode
                                                    hl-todo-mode
                                                    tern-mode
                                                    emmet-mode
                                                    pretty-mode
                                                    ))))

  :config
  (dolist (mode '(js2-jsx-mode js2-mode))
    (font-lock-add-keywords
     mode `(("\\<\\(function\\)("
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "\u0192") nil)))))
    (font-lock-add-keywords
     mode `(("\\<\\(function\\) .*("
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "\u0192") nil)))))))

(use-package 0blayout
  :ensure t
  :diminish 0blayout-mode
  :config
  (my-enable-mode '0blayout-mode)
  (0blayout-add-keybindings-with-prefix "C-x ,"))

(use-package smart-window
  :ensure t
  :init (setq smart-window-remap-keys nil)
  :bind (("C-c s m" . smart-window-move)
         ("C-c s b" . smart-window-buffer-split)
         ("C-c s f" . smart-window-file-split)
         ("C-c s R" . smart-window-rotate)
         ("C-M-2" . sw-below)
         ("C-M-3" . sw-right)))

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.gv\\'"
  :config (setq graphviz-dot-auto-indent-on-braces t
                graphviz-dot-auto-indent-on-newline t
                graphviz-dot-indent-width 2))

(use-package css-mode
  :mode "\\.css\\'"
  :config (setq css-indent-offset 2))

(use-package emmet-mode
  :ensure t
  :diminish emmet-mode
  :init
  (setq emmet-indent-after-insert t
        emmet-indentation 2)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package skewer-mode
  :ensure t
  :diminish skewer-mode
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode))

(use-package exec-path-from-shell :ensure t)

(use-package elm-mode
  :ensure t)

(use-package html-mode
  :mode "\\.htm\\'"
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
                              ( bind-key "s-." 'html-attrs-as-columns)
                              (bind-key "s-," 'html-attrs-as-line)
                              (use-package emmet-mode)
                              (setq emmet-indent-after-insert t
                                    emmet-indentation 2
                                    sgml-basic-offset 2
                                    fill-column 999)
                              (my-enable-modes '(emmet-mode
                                                 wrap-region-mode
                                                 whitespace-mode
                                                 yas-minor-mode
                                                 hungry-delete-mode
                                                 electric-pair-mode)))))

(use-package centered-window-mode
  :load-path "lisp/centered-window-mode"
  :commands (centered-window-mode))

(defun my-yasnippet-setup ()
  (interactive)
  (setq fill-column 999)
  (my-enable-mode 'yas-minor-mode))

(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook #'my-yasnippet-setup))

(add-hook 'text-mode-hook (defun my-text-setup ()
                            (interactive)
                            (setq fill-column 999
                                  whitespace-line-column 999)
                            (my-enable-modes '(visual-line-mode))))

(use-package elfeed
  :ensure t
  :bind (([f1] . elfeed))
  :init
  (add-hook 'elfeed-show-mode-hook (defun my-elfeed-setup ()
                                     (interactive)
                                     (my-enable-modes '(visual-line-mode))))
  :config
  (setq elfeed-feeds
        '("http://emacshorrors.com/feed.atom"
          "http://eli.thegreenplace.net/feeds/articles.atom.xml"
          "http://emacsninja.com/feed.atom")
        )
  )

(use-package org-mode
  :mode "\\.org\\'"
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o h" . helm-info-org))
  :init
  (setq org-log-done t
        org-agenda-files '("~/Dropbox/todo.org")
        org-default-notes-file "~/Dropbox/notes.org"
        org-agenda-ndays 7
        org-deadline-warning-days 14
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday nil
        org-reverse-note-order t
        org-fast-tag-selection-single-key 'expert
        org-src-fontify-natively t)
  (add-hook 'org-mode-hook 'org-hide-block-all)
  (add-hook 'org-mode-hook (defun my-org-setup ()
                             (interactive)
                             (org-babel-do-load-languages
                              'org-babel-load-languages
                              '((emacs-lisp . t)
                                (sh . t)
                                (python . t)
                                (ruby . t)
                                (perl . t)))
                             (my-enable-modes '(electric-pair-mode
                                                auto-fill-mode
                                                org-indent-mode
                                                visual-line-mode)))))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config (setq markdown-open-command "xdg-open"))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package recursive-narrow
  :ensure t
  :bind (("C-x n n" . recursive-narrow-or-widen-dwim)
         ("C-x n w" . recursive-widen-dwim)))

(use-package ag
  :ensure t
  :init (add-hook 'ag-mode-hook (defun my-ag-setup ()
                                  (interactive)
                                  (defun ag-delete-matching-lines ()
                                    (interactive)
                                    (read-only-mode -1)
                                    (call-interactively 'delete-matching-lines)
                                    (read-only-mode 1))

                                  (defun ag-delete-non-matching-lines ()
                                    (interactive)
                                    (read-only-mode -1)
                                    (call-interactively 'delete-non-matching-lines)
                                    (read-only-mode 1))

                                  (local-set-key (kbd "d") 'ag-delete-matching-lines)
                                  (local-set-key (kbd "f") 'ag-delete-non-matching-lines)))

  :config (setq ag-reuse-buffers t
                ag-reuse-window t))

(use-package dired
  :config (define-key dired-mode-map "|" (defun my-dired-create-file (filename)
                                           "Create FILENAME from Dired in if not exists.
If FILENAME already exists do nothing."
                                           (interactive "FCreate file: ")
                                           (shell-command (format "touch %s" filename))
                                           (when (file-exists-p filename)
                                             (dired-add-file filename)
                                             (dired-move-to-filename)))))

(use-package dired-x
  ;; Easily copy from one dired split to another
  :config (setq dired-dwim-target t))

(eval-after-load "info"
  '(use-package info+ :ensure t))

(use-package aggressive-indent :ensure t)
(use-package hungry-delete :ensure t :diminish hungry-delete-mode)
(use-package paredit :ensure t)
(use-package wrap-region :ensure t :diminish wrap-region-mode)
(use-package rainbow-delimiters :ensure t)
(use-package hl-todo :ensure t :diminish hl-todo-mode)
(use-package tern :ensure t :diminish tern-mode)
(use-package js2-refactor :ensure t :diminish js2-refactor-mode)
(use-package fancy-narrow :ensure t)
(use-package demo-it :ensure t)
(use-package cdnjs :ensure t)
(use-package tldr :ensure t)
(use-package markdown-preview-mode :ensure t)

(use-package helm-themes
  :ensure t
  :bind ([f9] . helm-themes))

(use-package my-themes
  :config
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
  (let ((transparency 100)
        (font&size (format "%s-%s" my-font-family my-font-size)))
    (set-frame-parameter (selected-frame) 'alpha (list transparency transparency))
    (add-to-list 'default-frame-alist `(alpha ,transparency ,transparency))
    (add-to-list 'default-frame-alist `(font . ,font&size))
    (add-hook 'my-load-theme-hook (defun my-theme-setup ()
                                    (interactive)
                                    (when (member my-font-family (font-family-list))
                                      (set-frame-font font&size))))
    (add-hook 'after-init-hook #'my-load-saved-theme)))

(use-package org-tree-slide
  :ensure t
  :init (setq org-tree-slide-never-touch-face t)
  :bind (([f6] . org-tree-slide-mode)
         ([f7] . org-tree-slide-skip-done-toggle))
  :config (org-tree-slide-simple-profile))

(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (defun my-org-bullets-setup ()
                                   (interactive)
                                   (my-enable-modes '(org-bullets-mode)))))

(defun my-frames-setup (&optional frame)
  (interactive)
  (when (window-system frame)
    (unbind-key "C-z")

    (use-package beacon
      :ensure t
      :config (my-enable-mode 'beacon-mode))))

(add-hook 'after-make-frame-functions #'my-frames-setup)
(add-hook 'after-init-hook #'my-frames-setup)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;;; init.el ends here
