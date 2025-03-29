(require 'package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Melpa Stable only for now, but if we want to use unstable we have
;; it available:
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Allegedly this will no longer be required in Emacs 30, but that's
;; not the case quite yet?
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(load-file (concat user-emacs-directory "install.el"))
(load-file (concat user-emacs-directory "appearance.el"))
(load-file (concat user-emacs-directory "behavior.el"))
(load-file (concat user-emacs-directory "helpers.el"))
(load-file (concat user-emacs-directory "keybindings.el"))

(defun load-dir (dir)
  (add-to-list 'load-path dir)
  (mapc 'load (directory-files dir nil "^[^#].*el$")))

(load-dir (concat user-emacs-directory "vendor/"))
(load-dir (concat user-emacs-directory "modules/"))

(load-file (concat user-emacs-directory "work.el"))

;; Custom variables file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(setq initial-major-mode 'org-mode)

;; EOF - my config
