;;
;; Switch between Solarized Light and Dark
;;

(defun toggle-solarized ()
  "Toggles between solarized light and dark."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'solarized-light)
    (load-theme 'solarized-dark)))


;;
;; Auto-indent on Paste
;;

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;;
;; Move lines
;; - from http://www.emacswiki.org/emacs/MoveLine

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


;;
;; Duplicate Line
;;

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))


;;
;; Align Equal Signs
;; corrected form from:
;;   http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
(defun align-equal-signs (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

;;
;; Yank Pop Forwards
;;

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))


;;
;; Window Switching
;;

(defun other-window-reverse ()
  "Switch to the previous window"
  (interactive)
  (other-window -1))


;;
;; Window Re-sizing
;;

(defun enlarge-window-down  () (interactive) (enlarge-window 1))
(defun enlarge-window-up    () (interactive) (enlarge-window -1))
(defun enlarge-window-left  () (interactive) (enlarge-window -1 t))
(defun enlarge-window-right () (interactive) (enlarge-window 1 t))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;;
;; Window Swapping
;; - from https://gist.github.com/287633
;;

(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(defun swap-with-down  () (interactive) (swap-with 'down))
(defun swap-with-up    () (interactive) (swap-with 'up))
(defun swap-with-left  () (interactive) (swap-with 'left))
(defun swap-with-right () (interactive) (swap-with 'right))

;;
;; Helpers for confluence mode
;;

(defun wiki-corp()
  (interactive)
  (setq
   confluence-url "https://wiki.corp.opscode.com/rpc/xmlrpc"
   confluence-default-space-alist (list (cons confluence-url "CORP"))))

(defun wiki-chef()
  (interactive)
  (setq
   confluence-url "https://wiki.opscode.com/rpc/xmlrpc"
   confluence-default-space-alist (list (cons confluence-url "chef"))))

;; Confluence mode with sane longlines
(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))

     (add-hook 'ediff-cleanup-hook
               '(lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))


;; From Jim Weirich; Thanks Jim!
;; https://github.com/jimweirich/emacs-setup-esk
(defconst jw-eval-buffer-commands
  '(("js" . "/usr/local/bin/node")
    ("rb" . "/Users/jtimberman/.rbenv/shims/ruby")
    ("coffee" . "/usr/local/bin/coffee")
    ("clj" . "/Users/jim/local/bin/clojure")))

(defconst jw-eval-buffer-name "*EVALBUFFER*")

(defun jw-eval-buffer ()
  "Evaluate the current buffer and display the result in a buffer."
  (interactive)
  (save-buffer)
  (let* ((file-name (buffer-file-name (current-buffer)))
         (file-extension (file-name-extension file-name))
         (buffer-eval-command-pair (assoc file-extension jw-eval-buffer-commands)))
    (if buffer-eval-command-pair
        (let ((command (concat (cdr buffer-eval-command-pair) " " file-name)))
          (shell-command-on-region (point-min) (point-max) command jw-eval-buffer-name nil)
          (pop-to-buffer jw-eval-buffer-name)
          (other-window 1)
          (jw-eval-buffer-pretty-up-errors jw-eval-buffer-name)
          (message ".."))
      (message "Unknown buffer type"))))

(defun jw-eval-buffer-pretty-up-errors (buffer)
  "Fix up the buffer to highlight the error message (if it contains one)."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((pos (search-forward-regexp "\\.rb:[0-9]+:\\(in.+:\\)? +" (point-max) t)))
      (if pos (progn
                (goto-char pos)
                (insert-string "\n\n")
                (end-of-line)
                (insert-string "\n"))))))

(defun jw-clear-eval-buffer ()
  (interactive)
  (save-excursion
    (set-buffer jw-eval-buffer-name)
    (kill-region (point-min) (point-max))))

(defun jw-eval-or-clear-buffer (n)
  (interactive "P")
  (cond ((null n) (jw-eval-buffer))
        (t (jw-clear-eval-buffer)))  )
