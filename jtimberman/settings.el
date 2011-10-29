(server-start)
(setq user-mail-address "opensource@housepub.org")
;; path sanity
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

;; Best color theme.
(require 'color-theme)
(color-theme-initialize)
(color-theme-ir-black)

;; Use Zsh
(setq shell-file-name "/bin/zsh")

;; Lets have readable fonts for all the times.
(set-face-attribute 'default nil :height 140)
(set-default-font "Menlo-14")
(set-frame-font "Menlo-14")

;; Full screen
(global-set-key [C-M-return] 'ns-toggle-fullscreen)
;; Away all whitespace!
(global-set-key "\C-cw" 'delete-trailing-whitespace)
(setq show-trailing-whitespace t)

;; Some useful default settings
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-ci" 'indent-region)
;;Don't echo passwords when communicating with interactive programs:
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;;keep backup files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "C-c t") 'insert-time-string)
(setq insert-time-string-default-format "iso-8601-date")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MobileOrg
(setq org-directory "~/Documents/org")
(setq org-mobile-inbox-for-pull "~/Documents/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown for certain files
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mkd" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrap-region
(require 'wrap-region)
(wrap-region-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ruby-block-mode t)
;; don't use the deep indentations, ffs.
(setq ruby-deep-indent-paren nil)
;; knife, thor, chef and proc are ruby, others are in esk.
(add-to-list 'auto-mode-alist '("\\.knife$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile$" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto indent on paste
;; http://www.emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confluence mode with sane longlines

(global-set-key "\C-cG" 'confluence-get-page)
(global-set-key "\C-cs" 'confluence-search)

;; confluence editing support (with longlines mode)
(autoload 'confluence-get-page "confluence" nil t)

 (eval-after-load "confluence"
   '(progn
      (require 'longlines)
      (progn
        (add-hook 'confluence-mode-hook 'longlines-mode)
        (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
        (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
        (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

 (autoload 'longlines-mode "longlines" "LongLines Mode." t)

 (eval-after-load "longlines"
   '(progn
      (defvar longlines-mode-was-active nil)
      (make-variable-buffer-local 'longlines-mode-was-active)

      (defun longlines-suspend ()
        (if longlines-mode
            (progn
              (setq longlines-mode-was-active t)
              (longlines-mode 0))))

      (defun longlines-restore ()
        (if longlines-mode-was-active
            (progn
              (setq longlines-mode-was-active nil)
              (longlines-mode 1))))

      ;; longlines doesn't play well with ediff, so suspend it during diffs
      (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                              activate compile preactivate)
        "Suspend longlines when running ediff."
        (with-current-buffer (ad-get-arg 0)
          (longlines-suspend)))

      (add-hook 'ediff-cleanup-hook
                '(lambda ()
                   (dolist (tmp-buf (list ediff-buffer-A
                                          ediff-buffer-B
                                          ediff-buffer-C))
                     (if (buffer-live-p tmp-buf)
                         (with-current-buffer tmp-buf
                           (longlines-restore))))))))
