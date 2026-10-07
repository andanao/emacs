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

(defvar mac/sync-os-appearance nil
  "Whether a theme change should drag macOS light/dark along with it.
Off while the Ef themes are being chosen: every reload would otherwise
repaint the whole desktop.")

(defun mac/dark-mode-emacs-align ()
  "Align OSX theme with emacs light or dark mode"
  ;; Asking the background whether it is dark covers every theme.  Matching
  ;; the name only ever worked for modus-vivendi.  `modus-themes-color-dark-p'
  ;; has no `ef-themes-' alias, so it keeps the Modus name.
  (when mac/sync-os-appearance
    (let ((dark (if (modus-themes-color-dark-p (ads/theme-color 'bg-main))
		    "true"
		  "false")))
      (start-process
       "mac/dark-mode" nil "osascript" "-e"
       (concat "tell app \"System Events\" to tell appearance preferences to set dark mode to "
	       dark)))))

(add-hook 'ef-themes-after-load-theme-hook 'mac/dark-mode-emacs-align)

(ads/leader-def "cn" "nix config" (projectile-switch-project-by-name "~/nix"))
(ads/leader-def "ch" "home-manager" (projectile-switch-project-by-name "~/home-manager"))

(menu-bar-mode -1)

;; konfig is a separate repo with its own tangle model, so work.el is read by
;; explicit path rather than supplied by the flake.
;; Off until cutover.  work.el also calls `server-mode', so a test daemon
;; contends with the live editor for the "server" socket; and konfig is being
;; nixified separately anyway.  Flip this back on when the trial is over.
(defvar ads/load-work-config nil
  "Whether to load konfig's `work.el'.  Off while the nix port is in progress.")

;; Presence of the file is the machine test.  The old `system-name' check was
;; not reliable this early in startup, and a Mac without konfig checked out is
;; exactly a Mac that should skip it.
(let ((work (concat git-directory "konfig/work.el")))
  (when (and ads/load-work-config (file-exists-p work))
    (with-demoted-errors "konfig: work.el stopped early: %S"
      (load-file work))))

(ads/leader-keys
  "tm" 'dwim-shell-commands-macos-toggle-menu-bar-autohide
  "tC" 'dwim-shell-commands-macos-caffeinate)

;;; mac.el ends here
