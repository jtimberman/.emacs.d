;;
;; Load Package/ELPA
;;
(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))


(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar elpa-packages '(ag
                        apache-mode
                        auto-complete
                        clojure-mode
                        coffee-mode
                        color-theme-solarized
                        dash
                        feature-mode
                        find-file-in-project
                        full-ack
                        gist
                        haml-mode
                        haskell-mode
                        http-twiddle
                        ido-completing-read+
                        ido-vertical-mode
                        inf-ruby
                        json
                        json-mode
                        magit
                        magit-gh-pulls
                        markdown-mode
                        maxframe
                        nginx-mode
                        paredit
                        powershell
                        python
                        enh-ruby-mode
                        robe
                        ruby-block
                        ruby-hash-syntax
                        ruby-tools
                        rust-mode
                        sass-mode
                        scala-mode
                        scratch
                        scss-mode
                        slime
                        slime-repl
                        smartparens
                        smex
                        ssh-config-mode
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
  (require 'yaml-mode)
  (require 'ruby-hash-syntax)
  (require 'ido-vertical-mode)
  (require 'auto-complete-config)
  (require 'smartparens-config)
  (require 'rust-mode)
  (ac-config-default))
