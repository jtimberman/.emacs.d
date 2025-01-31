;;
;; This configuration defines various aspects of editor and user behavior
;;
;; Default tab-width of 2 spaces
(setq-default tab-width 2)
(setq standard-indent 2)

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
(require 'wrap-region)
(wrap-region-global-mode t)
(wrap-region-add-wrapper "`" "`")

;; zomg https://gist.github.com/kevsmith/9391287
;; is this necessary? Or can we use x-select-enable-clipboard?
;; e.g., `(setq x-select-enable-clipboard t)`
(setq x-select-enable-clipboard t)
;; (setq interprogram-cut-function
;;       (lambda (text &optional push)
;;         (let* ((process-connection-type nil)
;;                (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
;;           (process-send-string pbproxy text)
;;           (process-send-eof pbproxy))))
;; (setq interprogram-paste-function
;;       (lambda ()
;;         (shell-command-to-string "pbpaste")))


;;
;; Use ivy as the completion and M-x framework
;;
(setup (:package counsel)
  (ivy-mode)
  (:option ivy-use-virtual-buffers t
           ivy-re-builders-alist '((t . ivy--regex-ignore-order))
           ivy-count-format "%d/%d ")
  (:global "C-s" swiper
           "s-f" swiper
           "C-x C-f" counsel-find-file
           "C-x C-b" counsel-switch-buffer
           "M-x" counsel-M-x))

(setup (:package ivy-rich)
  (ivy-rich-mode))

;; https://projectile.mx/
(setup (:package projectile)
  (projectile-mode +1)
  (:bind "s-p" projectile-command-map
         "C-c p" projectile-command-map))

;; counsel-projectile integrates projectile with
;; counsel's browse-and-select UI
(setup (:package counsel-projectile))

(setup (:package which-key)
  (which-key-mode)
  (:option which-key-idle-delay 1.0))

;; https://github.com/Alexander-Miller/treemacs
(setup (:package treemacs treemacs-projectile treemacs-magit)
  (:global "M-0" treemacs-select-window
           "M-o" ace-window ;; treemacs brings ace-window as a dependency
           "s-," treemacs))
