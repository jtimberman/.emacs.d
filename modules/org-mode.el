(use-package org
  :mode (("\\.org$" . org-mode))
  :hook (visual-line-mode)
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-indent-mode t)
  (setq org-src-fontify-natively t)
  (setq org-startup-folded t)
  (setq org-startup-truncated nil))
