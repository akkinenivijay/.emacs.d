
(defun my/package-upgrade-installed ()
  "Upgrade installed packages."
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (list-packages :no-fetch)
    (package-menu-mark-upgrades)
    (package-menu-execute :no-query)))

(defun my/enable-mode (mode)
  "Enable MODE."
  (my/disable-mode mode)
  (when (fboundp mode)
    (funcall mode +1)))

(defun my/disable-mode (mode)
  "Disable MODE."
  (when (boundp mode)
    (funcall mode -1)))

(defalias 'my/enable-modes (apply-partially 'mapc #'my/enable-mode)
  "Enable all modes in argument.")

(defalias 'my/disable-modes (apply-partially 'mapc #'my/disable-mode)
  "Disable all modes in argument.")

(defmacro command (&rest body)
  "Wrap BODY inside an interactive lambda."
  `(lambda ()
     (interactive)
     ,@body))

(defun my/inside-project-p ()
  "Check if we are inside a project."
  (and (fboundp 'projectile-project-p)
       (projectile-project-p)))

(defun my/copy-buffer-file-name (buff)
  "Copy filename of BUFF buffer."
  (interactive "b")
  (let ((filename (buffer-file-name (get-buffer buff))))
    (when filename
      (kill-new filename))))

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

(defun my/open ()
  "Simple function that allows us to open the underlying
    file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat (if (eq system-type 'darwin) "open" "xdg-open") " " buffer-file-name))))

(defun my/duplicate-line ()
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

(defun my/scroll-one-up ()
  (interactive)
  (scroll-down 1)
  (next-line -1))

(defun my/scroll-one-down ()
  (interactive)
  (scroll-up 1)
  (next-line))

(defun my/save-theme ()
  (interactive)
  (custom-set-variables `(,'custom-enabled-themes (quote ,custom-enabled-themes)))
  (custom-save-all))

(provide 'defuns)
