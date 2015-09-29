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
  "Hack"
  "My preferred font family."
  :group 'personal)

(defcustom my-font-size
  17
  "My preferred font size."
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

(require 'use-package)
(require 'pallet)
(pallet-mode t)

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
               dired-find-alternate-file
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

(defun my-switch-to-next-buffer ()
  "Switch to the next buffer associated to a file."
  (interactive)
  (cl-labels ((next-non-special-buffer
               ()
               (let ((buffer (next-buffer)))
                 (if (buffer-file-name buffer)
                     buffer
                   (next-non-special-buffer)))))
    (next-non-special-buffer)))

(defun my-switch-to-previous-buffer ()
  "Switch to the previous buffer associated to a file."
  (interactive)
  (cl-labels ((previous-non-special-buffer
               ()
               (let ((buffer (previous-buffer)))
                 (if (buffer-file-name buffer)
                     buffer
                   (previous-non-special-buffer)))))
    (previous-non-special-buffer)))

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

(use-package my-prelude
  :config (dolist (hook '(css-mode-hook
                          html-mode-hook
                          conf-mode-hook
                          emacs-lisp-mode))
            (add-hook hook 'xah-syntax-color-hex)))

(use-package my-editing-defuns
  :bind (("M-W" . my-copy-line-as-kill)
         ("s-M-k" . my-kill-sexp-backwards)
         ("C-;" . my-comment-or-uncomment-line)
         ("C-M-s" . my-isearch-forward-regexp-other-window)
         ("C-M-r" . my-isearch-backward-regexp-other-window)
         ("C-x C-e" . my-eval-last-sexp)))

(use-package centered-window-mode)

(use-package iedit
  :init (setq iedit-toggle-key-default (kbd "M-s-;")))

(use-package volatile-highlights
  :config (my-enable-mode 'volatile-highlights-mode))

(use-package mykie
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
  :config (my-enable-mode 'window-numbering-mode))

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("C-M-1" . zygospore-toggle-delete-other-windows)))

(use-package smart-window
  :init (setq smart-window-remap-keys nil)
  :bind (("C-x w" . smart-window-move)
         ("C-x W" . smart-window-buffer-split)
         ("C-x M-W" . smart-window-file-split)
         ("C-x R" . smart-window-rotate)
         ("C-M-2" . sw-below)
         ("C-M-3" . sw-right)))

(use-package align)

(use-package nvm
  :config
  (nvm-use "v0.12.7")
  (add-to-list 'exec-path (getenv "NVM_BIN")))

(use-package 0blayout
  :config
  (my-enable-mode '0blayout-mode)
  (0blayout-add-keybindings-with-prefix "C-x ,"))

(use-package discover
  :config
  (my-enable-mode 'discover-mode))

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
  :init (use-package default-black-theme)
  :bind (([f8] . my-use-prev-theme)
         ([f9] . my-use-next-theme))
  :config
  (my-set-themes '( default-black default ))
  (add-hook 'my-load-theme-hook (defun my-load-theme-setup ()
                                  (interactive)
                                  (let ((font-and-size (format "%s-%s" my-font-family my-font-size)))
                                    (add-to-list 'default-frame-alist `(font . ,font-and-size))
                                    (set-default-font font-and-size)))))

(use-package hl-line
  :bind ([f10] . global-hl-line-mode)
  :demand t
  :config (global-hl-line-mode))

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
  :init (setq-default save-place t))

(use-package tramp
  :defer t
  :config (setq tramp-default-method "ssh"))

(use-package server
  :defer t
  :config (setq server-use-tcp t))

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
  :bind ("C-@" . er/expand-region))

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
  :bind (("C-:" . avy-goto-char)
         ("M-g w" . avy-goto-word-0)
         ("M-g M-g" . avy-goto-line)))

(use-package helm-config
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
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

  :init
  (setq helm-split-window-in-side-p t
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
                     helm-autoresize-mode)))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  (add-hook 'projectile-mode-hook (defun my-helm-projectile-setup ()
                                    (interactive)
                                    (when (fboundp 'helm)
                                      (setq projectile-completion-system 'helm)
                                      (use-package helm-projectile
                                        :config (helm-projectile-on))
                                      (use-package wgrep-helm))
                                    ))
  :config
  (projectile-load-known-projects)
  (my-enable-mode 'projectile-global-mode))

(use-package recursive-narrow
  :bind (("C-x n n" . recursive-narrow-or-widen-dwim)
         ("C-x n w" . recursive-widen-dwim)))


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

(use-package emacs-lisp-mode

  :mode "\\.el\\'"
  :interpreter "emacs"
  :bind (("C-x C-e" . pp-eval-last-sexp))
  :init
  (fset 'my-emacs-lisp-mode-hook (defun my-elisp-setup ()
                                   (interactive)
                                   (my-enable-modes '(paredit-mode
                                                      ;; aggressive-indent-mode
                                                      rainbow-delimiters-mode
                                                      eldoc-mode))))
  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook))

(use-package clojure-mode
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

(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook (defun my-haskell-setup ()
                                 (interactive)
                                 (my-enable-modes '(subword-mode
                                                    flycheck-mode
                                                    wrap-region-mode
                                                    electric-pair-mode
                                                    haskell-doc-mode
                                                    interactive-haskell-mode
                                                    haskell-indentation-mode))))

  :config
  (setq haskell-process-suggest-remove-import-lines  t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-stylish-on-save t)
  (bind-keys :map haskell-mode-map
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
             ("C-c c"   . haskell-process-cabal)
             ("SPC"     . haskell-mode-contextual-space)))

(use-package elm-mode
  :mode "\\.elm\\'"
  :init (add-hook 'elm-mode-hook (defun my-elm-setup ()
                                   (interactive)
                                   (my-enable-modes '(subword-mode
                                                      wrap-region-mode
                                                      electric-pair-mode)))))

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
                                                      hl-todo-mode
                                                      tern-mode
                                                      skewer-mode
                                                      aggressive-indent-mode)))
                  :config)
  (use-package js2-refactor
    :config (js2r-add-keybindings-with-prefix "C-c C-j")))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(use-package flycheck
  :config
  (flycheck-define-checker jsxhint-checker
    "A JSX syntax and style checker based on JSXHint."
    :command ("jsxhint" "--babel" source)
    :error-patterns
    ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
    :modes (web-mode))

  (add-hook 'web-mode-hook (defun web-mode-jsx-flycheck-setup ()
                             (setq web-mode-code-indent-offset 2
                                   web-mode-markup-indent-offset 2)
                             (when (equal web-mode-content-type "jsx")
                               (flycheck-select-checker 'jsxhint-checker)
                               (my-enable-mode 'flycheck-mode)))))

(use-package json-mode
  :mode "\\.json\\'"
  :init (add-hook 'json-mode-hook (defun my-json-setup ()
                                    (interactive)
                                    (setq json-reformat:indent-width 2
                                          js-indent-level 2)

                                    (my-enable-modes '(subword-mode
                                                       electric-pair-mode)))))

(use-package graphviz-dot-mode
  :mode "\\.gv\\'"
  :config (setq graphviz-dot-auto-indent-on-braces t
                graphviz-dot-auto-indent-on-newline t
                graphviz-dot-indent-width 2))

(use-package css-mode
  :mode "\\.css\\'"
  :config (setq css-indent-offset 2))

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
                              (use-package emmet-mode)
                              (tagedit-add-experimental-features)
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
                            (setq fill-column 999
                                  whitespace-line-column 999)
                            (my-enable-modes '(visual-line-mode))))

(use-package gfm-mode
  :mode "\\.md\\'")

(use-package erc
  :bind (([f6] . my-erc-start-or-switch))
  :init (defun my-erc-start-or-switch ()
          "Connect to ERC or switch to last active buffer."
          (interactive)
          (if (get-buffer "irc.freenode.net:6667")
              (erc-track-switch-buffer 1)
            (when (y-or-n-p "Start ERC? ")
              (erc :server    "irc.freenode.net"
                   :port      6667
                   :nick      "anler"
                   :full-name "Anler Hernandez Peral"))))
  :config
  (setq erc-autojoin-channels-alist '((".*\\.freenode.net"
                                       "#emacs"
                                       "#haskell"
                                       "#haskell.es"
                                       "#purescript"
                                       "#hackerrank"
                                       ))
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))

  (my-enable-modes '(erc-autojoin-mode
                     erc-track-mode)))

(use-package twittering-mode
  :config
  (setq twittering-icon-mode t
        twittering-use-icon-storage t))

(use-package elfeed
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

(use-package scheme-mode
  :mode "\\.scm\\'"
  :init
  (setq scheme-program-name "racket")
  (add-hook 'scheme-mode-hook (defun my-scheme-setup ()
                                (interactive)
                                (my-enable-modes '(electric-pair-mode
                                                   aggressive-indent-mode
                                                   rainbow-delimiters-mode
                                                   paredit-mode
                                                   eldoc-mode
                                                   )))))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :init
  (add-hook 'racket-mode-hook (defun my-racket-setup ()
                                (interactive)
                                (my-enable-modes '(electric-pair-mode
                                                   aggressive-indent-mode
                                                   rainbow-delimiters-mode
                                                   paredit-mode
                                                   eldoc-mode
                                                   )))))

(use-package libmpdee
  :config (setq mpd-db-root "~/.mpd"))

;; (use-package emms-setup
;;   :config
;;   (bind-key [f2] (defun my-emms ()
;;                    (interactive)
;;                    (emms-player-mpd-connect)
;;                    (emms)))
;;   (setq emms-source-file-default-directory "~/Music/"
;;         emms-info-asynchronously t
;;         emms-show-format "♪ %s")

;;   (use-package emms-player-mpd
;;     :config (setq emms-player-mpd-server-name "localhost"
;;                   emms-player-mpd-server-port "6600"
;;                   emms-player-mpd-music-directory "~/Music")
;;     (add-to-list 'emms-info-functions 'emms-info-mpd)
;;     (add-to-list 'emms-player-list 'emms-player-mpd)

;;     (use-package emms-volume
;;       :config (setq emms-volume-change-function 'emms-volume-mpd-change)))

;;   (use-package emms-playlist-mode
;;     :config (setq emms-playlist-buffer-name "*Music*"
;;                   emms-playlist-default-major-mode 'emms-playlist-mode)))

(when (fboundp 'my-use-next-theme)
  (my-use-next-theme))

(when window-system
  (let ((elapsed-time (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s... done (%.3fs)" load-file-name elapsed-time)))

;;; init.el ends here
