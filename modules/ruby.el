;; don't use the deep indentations, ffs.
(setq ruby-deep-indent-paren nil)
;; knife, thor, chef and proc are ruby, others are in esk.
(add-to-list 'auto-mode-alist '("\\.knife$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)
