(defun pry ()
  (interactive)
  (run-ruby "pry" "pry")
  (setq comint-get-old-input #'pry-get-old-input))

(defun pry-get-old-input ()
  (let ((inf-ruby-first-prompt-pattern "pry.*(.*).*> *"))
    (inf-ruby-get-old-input)))


;; don't use the deep indentations, ffs.
(setq ruby-deep-indent-paren nil)
;; knife, thor, chef and proc are ruby, others are in esk.
(add-to-list 'auto-mode-alist '("\\.knife$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Collanderfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Kitchenfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)


(require 'flymake)

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Berksfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Capfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Cheffile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Collanderfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Gemfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Guardfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Kitchenfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Procfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Thorfile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Vagrantfile$" flymake-ruby-init) flymake-allowed-file-name-masks)


(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))
