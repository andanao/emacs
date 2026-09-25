;;; platform.el --- Per-machine and per-system loading  -*- lexical-binding: t; -*-
;;; Commentary:
;; Computer specific configs
;;; Code:

(when (eq system-type 'windows-nt)
  (load-file (concat user-emacs-directory "ms-windows.el")))

(when (eq system-type 'gnu/linux)
  (load-file (concat user-emacs-directory "linux.el")))

(when (eq system-type 'darwin)
  (load-file (concat user-emacs-directory "mac.el")))

;; not reliable when called at startup
;; (when (string-equal-ignore-case system-name "k2-mac.local")
;;   (load-file (concat git-directory "konfig/work.el")))

(when (string-equal-ignore-case system-name "ganymede")
  (load-file (concat git-directory "windows-config/ganymede.el")))

;;; platform.el ends here
