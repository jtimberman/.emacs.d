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
                        coffee-mode
                        dash
                        dockerfile-mode
                        feature-mode
                        find-file-in-project
                        full-ack
                        gist
                        go-mode
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
                        python
                        enh-ruby-mode
                        puppet-mode
                        robe
                        scratch
                        slime
                        smartparens
                        smex
                        solarized-theme
                        terraform-mode
                        wrap-region
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
  (require 'full-ack)
  (require 'yaml-mode)
  (require 'ido-vertical-mode)
  (require 'auto-complete-config)
  (require 'smartparens-config)
  (ac-config-default))
