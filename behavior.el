;;
;; This configuration defines various aspects of editor and user behavior
;;
;; (electric-pair-mode)
;; enable paredit
(setq-default tab-width 2)
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

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

(use-package toggle-quotes
  :bind ("C-'" . toggle-quotes))

;;
;; Inspired by https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/
;; Replaces previous use of ivy, counsel, etc.
;;
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

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

;; Use ripgrep for rgrep
(grep-apply-setting 'grep-template "rg --no-heading -H -uu -g <F> <R> <D>")
