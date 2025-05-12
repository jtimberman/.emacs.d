(use-package org
  :mode (("\\.org$" . org-mode))
  :hook (visual-line-mode)
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-indent-mode t)
  (setq org-src-fontify-natively t)
  (setq org-startup-folded t)
  (setq org-startup-truncated nil))

;; export an org mode file as a presentation using Reveal.js
(use-package ox-reveal)
(setq org-export-with-broken-links 'mark)

(remove-hook 'org-mode-hook 'electric-indent-mode)

(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                                     '((python . t)
                                       (ruby . t)
                                       (perl . t)
                                       )))


