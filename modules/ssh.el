(setup (:package ssh-config-mode))

(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
