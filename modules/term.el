(use-package vterm
  :bind ("C-c v" . vterm)
  :commands (vterm)
  :config
  (setq vterm-max-scrollback 100000)
  :hook ((vterm-mode . (lambda() display-line-numbers-mode 0))
         (vterm-mode . (lambda() (setq show-trailing-whitespace nil)))))
