(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "M-Y") 'yank-pop-forwards)

(global-set-key (kbd "C-M-=") 'align-equal-signs)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c G") 'confluence-get-page)
(global-set-key (kbd "C-c s") 'confluence-search)
(global-set-key (kbd "C-c t") 'insert-time-string)
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; Niceties on Mac OS X
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key [C-M-return] 'ns-toggle-fullscreen)
