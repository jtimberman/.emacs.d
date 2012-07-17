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
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Kitchenfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)
