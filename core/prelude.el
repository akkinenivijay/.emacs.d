;;; prelude.el --- Utility functions. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)

(defvar my-isearch-done-opposite
  nil
  "Wether or not isearch should end at the opposite side of the match.")

(defun my-isearch-done-opposite (&optional nopush edit)
  "End current search in the opposite side of the match.

The arguments NOPUSH and EDIT are passed to the wrapped function `isearch-done'."
  (interactive)
  (let ((my-isearch-done-opposite t))
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end
      (goto-char isearch-other-end))))

(defvar my-load-theme-hook
  nil
  "Hooks to run after loading a theme.")

(defvar my-saved-theme-filename "~/.emacs.d/.emacs-theme")

(advice-add 'load-theme :after #'my-save-theme)
(advice-add 'disable-theme :after #'my-save-default-theme)
(advice-add 'load-theme :after #'my-run-theme-hooks)

(defun my-run-theme-hooks (theme &optional no-confirm no-enable)
  (run-hooks 'my-load-theme-hook))

(defun my-save-default-theme (disabled-theme)
  (my-save-theme 'default))

(defun my-save-theme (theme &optional no-confirm no-enable)
  (with-temp-buffer
    (insert (symbol-name theme))
    (when (file-writable-p my-saved-theme-filename)
      (write-region (point-min)
                    (point-max)
                    my-saved-theme-filename))))

(defun my-load-saved-theme ()
  (interactive)
  (when (file-exists-p my-saved-theme-filename)
    (let ((theme (intern (with-temp-buffer
                           (insert-file-contents my-saved-theme-filename)
                           (buffer-string)))))
      (unless (eq theme 'default)
        (load-theme theme :no-confirm)))))

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

(defun my-inside-project-p ()
  "Check if we are inside a project."
  (and (fboundp 'projectile-project-p)
       (projectile-project-p)))

(defun my-copy-buffer-file-name (buff)
  "Copy filename of BUFF buffer."
  (interactive "b")
  (let ((filename (buffer-file-name (get-buffer buff))))
    (when filename
      (kill-new filename))))

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

;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare"
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a' "
  (interactive)
  (if (region-active-p)
      (let (rlt-buf
            diff-output
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))

        (setq rlt-buf (get-buffer-create "*Diff-region-output*"))
        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb)))
          ;; show the diff output
          (if (string= diff-output "")
              (message "Two regions are SAME!")
            (save-current-buffer
              (switch-to-buffer-other-window rlt-buf)
              (set-buffer rlt-buf)
              (erase-buffer)
              (insert diff-output)
              (diff-mode))))

        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))

(defun my-copy-line-as-kill (arg)
  "Copy ARG lines as kill."
  (interactive "p")
  (save-excursion
    (let ((beg (progn
                 (beginning-of-line)
                 (point)))
          (end (progn
                 (forward-line arg)
                 (point))))
      (copy-region-as-kill beg end))))

(defun my-kill-sexp-backwards ()
  "Kill the sexp leading point."
  (interactive)
  (kill-sexp -1))

(defun my-comment-or-uncomment-line (&optional arg)
  "Comment current line or, if at EOL, call `comment-dwim' with ARG."
  (interactive "*P")
  (comment-normalize-vars)
  (when (and (not (region-active-p))
             (not (looking-at "[ \t]*$")))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

(defun my-isearch-forward-regexp-other-window ()
  "`isarch-forward-regexp' in `other-window'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(defun my-isearch-backward-regexp-other-window ()
  "`isarch-backward-regexp' in `other-window'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-backward-regexp)))

(defun my-eval-last-sexp (arg)
  "Extension over eval-last-sexp that replaces the last sexp with the
    result if called with the universal argument twice."
  (interactive "P")
  (if (= 16 (prefix-numeric-value arg))
      (my-replace-last-sexp)
    (eval-last-sexp arg)))

(defun my-replace-last-sexp ()
  "Eval last sexp and replaces it in the buffer with its result."
  (interactive)
  (let ((result (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" result))))

(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
    file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      "xdg-open")
                    " "
                    buffer-file-name))))

(defun my-duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(defun my-find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(provide 'prelude)
;;; prelude.el ends here
