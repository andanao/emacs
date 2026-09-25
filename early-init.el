;;; early-init.el --- Pre-init frame, GC and package settings  -*- lexical-binding: t; -*-
;;; Commentary:
;; early-init, Set directory variables, Set utf-8 encoding,
;; Garbage Collection, UI Changes, Package Usage, Backup files,
;; Environment setup, Provide Early init
;;; Code:

(setq debug-on-error t)
(add-hook 'after-init-hook '(lambda () (setq debug-on-error nil)))

(if (eq system-type 'windows-nt)
    (setq git-directory (concat "c:/Users/" user-login-name "/git/"))
    (setq git-directory (expand-file-name "~/git/")))

(setq org-directory (concat git-directory "org/"))

(setq ads/config-file (concat git-directory "emacs/readme.org"))

(set-language-environment "UTF-8")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq ;;file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(add-hook 'after-init-hook '+reset-init-values)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(scroll-bar-mode -1)                    ;; Disable the visible scrollbar
(tool-bar-mode -1)                      ;; Disablet the toolbar
(tooltip-mode -1)                       ;; Disable tooltips
(menu-bar-mode -1)                      ;; Disable the menu bar

(customize-set-variable 'indent-tabs-mode nil)

(setq server-client-instructions nil)

(setq frame-inhibit-implied-resize t)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(setq native-comp-async-report-warnings-errors 'silent)

;; Packages come from the flake and are already on load-path.  package.el
;; stays off so it cannot put an elpa directory in front of them, and
;; `use-package-always-ensure' stays nil so no use-package form tries to
;; install anything at runtime.  The flake's alwaysEnsure covers the same
;; forms at build time instead.
(setq package-enable-at-startup nil
      package-archives nil
      use-package-always-ensure nil)

(when (eq system-type 'gnu/linux)
  (setq use-package-always-demand t))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)

;;; early-init.el ends here
