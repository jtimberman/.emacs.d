(use-package vterm
  :bind ("C-c v" . vterm)
  :hook ((vterm-mode . (lambda() display-line-numbers-mode 0))
         (vterm-mode . (lambda() (setq show-trailing-whitespace nil)))))
