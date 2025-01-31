;; Setup package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(if (package-installed-p 'setup)
    nil
  (if (memq 'setup package-archive-contents)
      nil
    (package-refresh-contents))
  (package-install 'setup))
(require 'setup)

(defun load-dir (dir)
  (add-to-list 'load-path dir)
  (mapc 'load (directory-files dir nil "^[^#].*el$")))

(load-dir (concat user-emacs-directory "modules/"))
(load-dir (concat user-emacs-directory "vendor/"))

(load-file (concat user-emacs-directory "appearance.el"))
(load-file (concat user-emacs-directory "behavior.el"))
(load-file (concat user-emacs-directory "helpers.el"))
(load-file (concat user-emacs-directory "keybindings.el"))

;; Custom variables file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(setq initial-major-mode 'org-mode)
