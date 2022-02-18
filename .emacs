;; example .emacs file from ubuntu with emacs 28
(setq efs/user-dir-emacs "~/git/emacs/")

(setq efs/user-dir-org "~/git/Org/")

(setq efs/personal-dir-org  efs/user-dir-org)

(setq efs/user-dir-readme (concat efs/user-dir-emacs "readme.org"))

(setq efs/user-dir-config "~/.emacs")

(setq efs/computer-id "xps-ubuntu")

(setq debug-on-error t)
(load-file (concat efs/user-dir-emacs "init.el"))

(setq debug-on-error nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Numbers are helpful.")
 '(org-block-begin :background)
 '(org-icalendar-include-todo 'all)
 '(org-icalendar-store-UID t)
 '(package-selected-packages
   '(dired-git-info undo-tree dired-async ahk-mode jupyter quelpa tiny lsp-python-ms pdf-tools dap-mode lsp-ivy lsp-treemacs lsp-ui lsp-mode org-appear emojify org-download magit counsel-projectile projectile visual-fill-column org-bullets hydra helpful ivy-rich counsel which-key doom-modeline all-the-icons doom-themes undo-fu evil-collection evil general rainbow-delimiters use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "initialization complete")
