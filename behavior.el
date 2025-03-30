;;
;; This configuration defines various aspects of editor and user behavior
;;
(electric-pair-mode) ;; auto complete brackets
(setq-default tab-width 2) ;; Default tab-width of 2 spaces
(setq standard-indent 2) ;; Default tab-width of 2 spaces
(setq-default indent-tabs-mode nil) ;; Always indent with spaces
(fset 'yes-or-no-p 'y-or-n-p) ;; Answer yes/no questions by typing y/n
(setq ring-bell-function 'ignore) ;; No visual bell please

;; Backup directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Don't echo passwords
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(setq insert-time-string-default-format "iso-8601-date")
(server-start) ;; Start Emacs server

;; wrap-region
(use-package wrap-region
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "`" "`"))

(setq x-select-enable-clipboard t)
;;
;; Use ivy as the completion and M-x framework
;;
(use-package ivy
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :bind (("C-s" . swiper)
         ("s-f" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-b" . counsel-switch-buffer)
         ("M-x" . counsel-M-x)))

(use-package ivy-rich
  :config (ivy-rich-mode))

;; https://projectile.mx/
(use-package projectile
  :config (projectile-mode +1)
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))

;; counsel-projectile integrates projectile with
;; counsel's browse-and-select UI
(use-package counsel-projectile)

(which-key-mode 1)
(which-key-setup-side-window-right-bottom)

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :bind (("M-0" . treemacs-select-window)
         ("M-o" . ace-window)
         ("s-," . treemacs)))

(use-package treemacs-projectile)
(use-package treemacs-magit)

(use-package dired-preview
  :hook (after-init . dired-preview-global-mode))
