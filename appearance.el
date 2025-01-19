;; Disable splash page
(setq inhibit-startup-message t)


;; Set default font
(setq my-font (cond ((window-system) "Menlo")
                    ("Monospace")))
(set-face-attribute 'default nil :family my-font :height 140)
(set-frame-font (concat my-font "-14"))

(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)
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
(global-display-line-numbers-mode 1)

;; Whitespace
(setq-default show-trailing-whitespace t)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
