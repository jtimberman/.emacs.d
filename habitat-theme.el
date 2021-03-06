(deftheme habitat
  "Created 2016-07-21.")

(custom-theme-set-faces
 'habitat
 '(default ((t (:inherit nil :stipple nil :background "#F8FCFC" :foreground "#0E1818" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Menlo"))))
 '(cursor ((t (:background "#E27028" :foreground "#FFFFFF"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#9E4840"))))
 '(minibuffer-prompt ((t (:foreground "#6E977C" :inverse-video nil :underline nil :slant normal :weight bold))))
 '(highlight ((t (:background "#eee8d5"))))
 '(region ((t (:background "#363C40" :foreground "#CEAD8C" :inverse-video t :underline nil :slant normal :weight normal))))
 '(shadow ((t (:foreground "#B0B2AF"))))
 '(secondary-selection ((t (:background "#eee8d5"))))
 '(trailing-whitespace ((t (:foreground "#D14B3E" :inverse-video t :underline nil :slant normal :weight normal))))
 '(font-lock-builtin-face ((t (:foreground "#6E977C" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#B0B2AF" :inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-comment-face ((t (:foreground "#B0B2AF" :inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-constant-face ((t (:foreground "#6E977C" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-doc-face ((t (:foreground "#B0B2AF" :inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "#377B93" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#6E977C" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-negation-char-face ((t (:foreground "#D14B3E" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-preprocessor-face ((t (:foreground "#D14B3E" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#FBA913" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#D14B3E" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-string-face ((t (:foreground "#9E4840" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-type-face ((t (:foreground "#FBA913" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "#377B93" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-warning-face ((t (:foreground "#D14B3E" :inverse-video nil :underline nil :slant normal :weight bold))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "#5AA2DB" :inverse-video nil :underline t :slant normal :weight normal))))
 '(link-visited ((t (:foreground "#9E4840" :inverse-video nil :underline t :slant normal :weight normal))))
 '(fringe ((t (:background "#CED0D0" :foreground "#F8FCFC"))))
 '(header-line ((t (:background "#F8FCFC" :foreground "#363C40" :inverse-video t :underline nil :slant normal :weight normal))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "#F8FCFC" :foreground "#363C40" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:background "#B0B2AF" :foreground "#FCF8F8" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(isearch ((t (:background "#FCF8F8" :foreground "#9E4840" :inverse-video t :underline nil :slant normal :weight normal))))
 '(isearch-fail ((t (:background "#fdf6e3" :foreground "#D14B3E" :inverse-video t :underline nil :slant normal :weight normal))))
 '(lazy-highlight ((t (:background "#FCF8F8" :foreground "#FBA913" :inverse-video t :underline nil :slant normal :weight normal))))
 '(match ((t (:background "#FCF8F8" :foreground "#FBA913" :inverse-video t :underline nil :slant normal :weight normal))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'habitat)
