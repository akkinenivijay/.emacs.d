(deftheme default
  "Automatically created 2013-05-20.")

(custom-theme-set-faces
 'default
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "#eeeeee"))))
 '(region ((nil (:background "honeydew"))))
 '(hl-line ((nil (:background "honeydew"))))
 '(yas-field-highlight-face ((nil (:background "lavender"))))
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(show-paren-match ((nil (:background "lavender"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'default)
