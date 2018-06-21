;; Disable splash page
(setq inhibit-startup-message t)


;; Set default font
(setq my-font (cond ((window-system) "Menlo")
                    ("Monospace")))
(set-face-attribute 'default nil :family my-font :height 160)
(set-default-font (concat my-font "-16"))
(set-frame-font (concat my-font "-16"))

;;(load-theme 'habitat t)
(load-theme 'solarized-light t)
;; Disable menu-bar
(menu-bar-mode 0)

;; Disable toolbar
(if window-system
    (tool-bar-mode -1))

;; Show matching paranthensis
(show-paren-mode t)

;; Show column number globally
(column-number-mode t)

;; Show line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; Whitespace
(setq-default show-trailing-whitespace t)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; format for time strings
(setq insert-time-string-default-format "iso-8601-date")
