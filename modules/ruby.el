(use-package enh-ruby-mode
  :defer t)
(use-package inf-ruby)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.knife$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Collanderfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Kitchenfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rantfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . enh-ruby-mode))

(add-hook 'enh-ruby-mode
          (lambda () (auto-fill-mode -1)))
