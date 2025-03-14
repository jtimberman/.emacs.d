;;
;; This configuration defines the look and feel of the editor
;;
(tooltip-mode -1)
(menu-bar-mode 0)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(column-number-mode t)
(global-hl-line-mode 1)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq-default frame-title-format "%b (%f)")

;; Turn on lnie numbers everywhere, except...
(global-display-line-numbers-mode 1)
(dolist (mode '(term-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; Set default font
(setq my-font (cond ((window-system) "BlexMono Nerd Font")
                    ("Monospace")))
(set-face-attribute 'default nil :family my-font :height 140)
(set-frame-font (concat my-font "-14"))

;; Configure a nice modeline with doom
;; https://github.com/doomemacs/doomemacs/issues/724
;; M-x all-the-icons-install-fonts
;; M-x nerd-icons-install-fonts
(setup (:package all-the-icons))
(setup (when (not (string-equal system-type "windows-nt"))
         (:package doom-modeline)
         (doom-modeline-mode t)))

;; install and configure a nice theme
(setup (:package solarized-theme))
(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-light t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))
