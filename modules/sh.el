(add-to-list 'auto-mode-alist '("\*\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc" . sh-mode))

(add-hook 'sh-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (setq tab-width 4)))

(setq sh-basic-offset 2
      sh-indentation 2)
