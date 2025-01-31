(setup (:package markdown-mode))

(setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mkd" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.text" . gfm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt" . gfm-mode) auto-mode-alist))

(remove-hook 'text-mode-hook 'turn-on-auto-fill) ;; auto-fill-mode, just say no
