;; example .emacs file from ubuntu with emacs 28
(setq ads/user-dir-emacs "~/git/emacs/")

(setq ads/user-dir-org "~/git/Org/")

(setq ads/personal-dir-org  ads/user-dir-org)

(setq ads/user-dir-readme (concat ads/user-dir-emacs "readme.org"))

(setq ads/user-dir-config "~/.emacs")

(setq ads/computer-id "work")

(setq debug-on-error t)
(load-file (concat ads/user-dir-emacs "init.el"))

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
