;;; mac.el --- macOS-only configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; Modifier Keys, Frame Appearance, Homebrew PATH, Toggle themes, nixconfig,
;; Menu bar, Load work config, Dwim commands
;;; Code:

(setq mac-command-modifier 'control
      mac-option-modifier 'meta
      mac-control-modifier 'meta
      mac-pass-command-to-system nil)

(add-to-list 'default-frame-alist '(undecorated . t))

(dolist (dir (delq nil
                   (list "/opt/homebrew/bin"
                         (expand-file-name "~/.cargo/bin")
                         (expand-file-name "~/.local/bin")
                         (car (last (file-expand-wildcards
                                     (expand-file-name "~/.nvm/versions/node/*/bin"))))
                         "/Library/TeX/texbin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

(let ((libgs "/opt/homebrew/lib/libgs.dylib"))
  (when (file-exists-p libgs)
    (setenv "LIBGS" libgs)))

(defun mac/dark-mode-emacs-align ()
  "Align OSX theme with emacs light or dark mode"
  (let ((dark (if (string-search "vivendi"
				 (symbol-name (modus-themes-get-current-theme)))
		  "true"
		"false")))
    (start-process
     "mac/dark-mode" nil "osascript" "-e"
     (concat "tell app \"System Events\" to tell appearance preferences to set dark mode to "
	     dark))))

(add-hook 'modus-themes-after-load-theme-hook 'mac/dark-mode-emacs-align)

(ads/leader-def "cn" "nix config" (projectile-switch-project-by-name "~/nix"))
(ads/leader-def "ch" "home-manager" (projectile-switch-project-by-name "~/home-manager"))

(menu-bar-mode -1)

(load-file (concat git-directory "konfig/work.el"))

(ads/leader-keys
  "tm" 'dwim-shell-commands-macos-toggle-menu-bar-autohide
  "tC" 'dwim-shell-commands-macos-caffeinate)

;;; mac.el ends here
