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

  (add-hook 'js2-jsx-mode-hook
            (defun my-js-setup ()
              (interactive)
              (use-package js2-refactor
                :config (js2r-add-keybindings-with-prefix "C-c C-j"))

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
