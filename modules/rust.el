(use-package rust-mode
  :defer t
  :hook (rustfmt-enable-on-save . cargo-minor-mode)
  :bind ("C-c C-f" . fustfmt-format-buffer))

(use-package cargo-mode)
