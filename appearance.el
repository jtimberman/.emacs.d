;; Disable splash page
(setq inhibit-startup-message t)


;; Set default font
(setq my-font (cond ((window-system) "Menlo")
                    ("Monospace")))
(set-face-attribute 'default nil :family my-font :height 180)
(set-default-font (concat my-font "-18"))
(set-frame-font (concat my-font "-18"))

;; Use the ir-black theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'ir-black t)

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
(setq show-trailing-whitespace t)

;; format for time strings
(setq insert-time-string-default-format "iso-8601-date")

