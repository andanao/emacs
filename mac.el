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

;; konfig is a separate repo with its own tangle model, so work.el is read by
;; explicit path rather than supplied by the flake.  It adds to
;; `org-babel-auto-tangle-file-list', which this config used to define and no
;; longer does; the stub keeps that line from aborting startup 44 lines into a
;; 2343-line file.  Restoring the hook that acted on it is konfig's to do.
(defvar org-babel-auto-tangle-file-list nil
  "Files konfig asks to be tangled on save.  Nothing acts on this yet.")

;; Presence of the file is the machine test.  The old `system-name' check was
;; not reliable this early in startup, and a Mac without konfig checked out is
;; exactly a Mac that should skip it.
(let ((work (concat git-directory "konfig/work.el")))
  (when (file-exists-p work)
    (with-demoted-errors "konfig: work.el stopped early: %S"
      (load-file work))))

(ads/leader-keys
  "tm" 'dwim-shell-commands-macos-toggle-menu-bar-autohide
  "tC" 'dwim-shell-commands-macos-caffeinate)

;;; mac.el ends here
