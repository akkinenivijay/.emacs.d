(deftheme default-light
  "Automatically created 2013-05-20.")

(custom-theme-set-faces
 'default-light
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'default-light)
