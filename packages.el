;;
;; Load Package/ELPA
;;
(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))


(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar elpa-packages '(ag
                        auto-complete
                        cargo-mode
                        clojure-mode
                        dash
                        dockerfile-mode
                        elein
                        enh-ruby-mode
                        find-file-in-project
                        gist
                        go-mode
                        ido-completing-read+
                        ido-vertical-mode
                        inf-clojure
                        inf-ruby
                        json-mode
                        json-reformat
                        magit
                        magit-gh-pulls
                        markdown-mode
                        maxframe
                        paredit
                        rust-mode
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
  (require 'rust-mode)
  (ac-config-default))

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(use-package smartparens
  :ensure smartparens
  :hook (enh-ruby-mode ruby-mode python-mode elisp markdown-mode)
  :config
  (require 'smartparens-config))
