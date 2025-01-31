;; (add-hook 'rust-mode-hook 'rustfmt-enable-on-save)
;; (define-key rust-mode-map (kbd "C-c C-f") 'rustfmt-format-buffer)

(setup (:package rust-mode cargo-mode)
  (:hook rustfmt-enable-on-save)
  (:bind "C-c C-f" rustfmt-format-buffer))

(setup rust-mode
  (:hook cargo-minor-mode))
