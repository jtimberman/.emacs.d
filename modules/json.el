(use-package json-mode
  :defer t)
(use-package json-reformat)

(setq auto-mode-alist (cons '("\\.json" . json-mode) auto-mode-alist))
