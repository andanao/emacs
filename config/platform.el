;;; platform.el --- Per-machine and per-system loading  -*- lexical-binding: t; -*-
;;; Commentary:
;; Computer specific configs
;;; Code:

;; Beside this config, not under user-emacs-directory.  The two are the
;; same thing when Emacs is started with --init-directory, and different
;; after cutover, when ~/.emacs.d holds symlinks and nothing else.
(when-let* ((system (pcase system-type
                      ('windows-nt "ms-windows")
                      ('gnu/linux "linux")
                      ('darwin "mac"))))
  (ads/load-config system))

;; not reliable when called at startup
;; (when (string-equal-ignore-case system-name "k2-mac.local")
;;   (load-file (concat git-directory "konfig/work.el")))

(when (string-equal-ignore-case system-name "ganymede")
  (load-file (concat git-directory "windows-config/ganymede.el")))

;;; platform.el ends here
