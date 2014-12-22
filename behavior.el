;; Default tab-width of 2 spaces
(setq-default tab-width 2)
(setq standard-indent 2)

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

;; Answer yes/no questions by typing y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; No visual bell please
(setq ring-bell-function 'ignore)

;; Backup directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Don't echo passwords
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; insert-time-string format
(setq insert-time-string-default-format "iso-8601-date")

;; Start Emacs server
(server-start)

;; wrap-region
(require 'wrap-region)
(wrap-region-global-mode t)
(wrap-region-add-wrapper "`" "`")

;; zomg https://gist.github.com/kevsmith/9391287
(setq interprogram-cut-function
      (lambda (text &optional push)
        (let* ((process-connection-type nil)
               (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
          (process-send-string pbproxy text)
          (process-send-eof pbproxy))))

(setq interprogram-paste-function
      (lambda ()
        (shell-command-to-string "pbpaste")))
