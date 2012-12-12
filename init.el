;; emacs configuration

(load-file "~/.emacs.d/env.el")
(load-file "~/.emacs.d/packages.el")
(load-file "~/.emacs.d/helpers.el")
(load-file "~/.emacs.d/behavior.el")
(load-file "~/.emacs.d/appearance.el")
(load-file "~/.emacs.d/keybindings.el")

(defun load-dir (dir)
  (add-to-list 'load-path dir)
  (mapc 'load (directory-files dir nil "^[^#].*el$")))

(load-dir (concat user-emacs-directory "modules/"))
(load-dir (concat user-emacs-directory "vendor/"))

;; Custom variables file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; Initialize packages in packages.el
(initialize-packages)

(setq initial-major-mode 'org-mode)
