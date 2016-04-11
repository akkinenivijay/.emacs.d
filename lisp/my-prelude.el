;;; my-prelude.el --- Utility functions. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'cl)

(defcustom my-font-family
  "Hack"
  "My preferred font family."
  :group 'personal)

(defcustom my-font-size
  14
  "My preferred font size."
  :group 'personal)

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

(defun my-cycle (list)
  "Return copy of LIST turned into an infinite list."
  (let ((newlist (copy-list list)))
    (setf (cdr (last newlist))
          newlist)
    newlist))

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
    (delete-file filename)))

(defun my-rename-buffer-and-file (newname newfilename)
  "Rename current buffer to NEWNAME and its file to NEWFILENAME."
  (interactive
   (let* ((newname (read-string "Rename buffer (to new name): "))
          (newfilename (read-string "Rename file (to new name): " newname)))
     (list newname newfilename)))
  (let ((ask-if-exists 1))
    (rename-file (buffer-file-name) newfilename ask-if-exists)
    (set-visited-file-name newfilename :no-query :along-with-file)))

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

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

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

(provide 'my-prelude)
;;; my-prelude.el ends here
