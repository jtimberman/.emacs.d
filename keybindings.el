;; Define keybindings by default that are not associated with any
;; specific modes, modules, or packages. Customizations and bindings
;; for such will be wherever those are installed.
;;
;; Some keybindings here are for functions defined within this
;; configuration, however!
;;
;; http://www.emacswiki.org/emacs/AutoIndentation works in most modes
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Let's not make assumptions on macOS about modifier keys
(when (eq system-type 'darwin)
  (setq
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-control-modifier 'control
   mac-function-modifier 'hyper))

(keymap-global-set "C-c C-c M-x" 'execute-extended-command)
(keymap-global-set "C-x C-k" 'kill-buffer)
(keymap-global-set "M-Y" 'yank-pop-forwards)
(keymap-global-set "C-M-=" 'align-equal-signs)
(keymap-global-set "C-M-h" 'backward-kill-word)
(keymap-global-set "C-c u" 'move-line-up)
(keymap-global-set "C-c d" 'move-line-down)
(keymap-global-set "C-c C-j" 'join-line)
(keymap-global-set "C-c w" 'delete-trailing-whitespace)
(keymap-global-set "C-c t" 'insert-time-string)
(keymap-global-set "C-c f" 'find-file-in-project)
(keymap-global-set "C-c F" 'find-grep-dired)
(keymap-global-set "C-c r" 'revert-buffer)
(keymap-global-set "C-c l"
                   (lambda ()
                     (interactive)
                     (toggle-solarized)
                     (set-cursor-color "darkorange")))

;; Command-up, down, left, and right behave like other parts of macOS
(keymap-global-set "s-<up>" 'beginning-of-buffer)
(keymap-global-set "s-<down>" 'end-of-buffer)
(keymap-global-set "s-<left>" 'beginning-of-line)
(keymap-global-set "s-<right>" 'end-of-line)

;; Page up and page down work like we expect. Why is up down and down
;; up? Because macOS.
(keymap-global-set "H-<up>" 'scroll-down)
(keymap-global-set "H-<down>" 'scroll-up)

(keymap-global-set "s-a" 'mark-whole-buffer)
(keymap-global-set "s-v" 'yank)
(keymap-global-set "s-c" 'kill-ring-save)
(keymap-global-set "s-s" 'save-buffer)
(keymap-global-set "s-l" 'goto-line)
(keymap-global-set "s-w" 'delete-frame)
(keymap-global-set "s-n" 'make-frame)
(keymap-global-set "s-z" 'undo)
