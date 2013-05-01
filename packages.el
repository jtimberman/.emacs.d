;;
;; Load Package/ELPA
;;
(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar elpa-packages '(clojure-mode
                        coffee-mode
                        color-theme-solarized
                        confluence
                        dash
                        feature-mode
                        find-file-in-project
                        full-ack
                        gist
                        haml-mode
                        haskell-mode
                        http-twiddle
                        ido-ubiquitous
                        inf-ruby
                        jira
                        json
                        magit
                        magithub
                        markdown-mode
                        maxframe
                        paredit
                        powershell
                        python
                        ruby-block
                        ruby-tools
                        rvm
                        sass-mode
                        scala-mode
                        scss-mode
                        slime
                        slime-repl
                        smex
                        starter-kit
                        starter-kit-bindings
                        starter-kit-eshell
                        starter-kit-js
                        starter-kit-lisp
                        starter-kit-ruby
                        wrap-region
                        xlicense
                        xml-rpc
                        yaml-mode
                        yasnippet))

(dolist (p elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;
;; Initialize Packages
;;

(defun initialize-packages ()
  (require 'xlicense)
  (require 'full-ack)
  (require 'yaml-mode))
