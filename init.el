;;; init.el --- Emacs configuration entry point  -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads every file under config/ and lisp/ in a fixed order.  That order
;; matches the sequence the old literate config tangled into, so behaviour
;; does not depend on how the sections were regrouped into files.
;;; Code:

(defun ads/load-config (relative)
  "Load RELATIVE, an elisp file below `user-emacs-directory'.
Loaded by explicit path rather than with `require', so that a file named
after a package (org.el, dired.el) cannot shadow the real one."
  (load (expand-file-name relative user-emacs-directory) nil :nomessage))

(ads/load-config "config/settings")
(ads/load-config "config/theme")
(ads/load-config "config/keybindings")
(ads/load-config "config/agent-shell")
(ads/load-config "config/ui")
(ads/load-config "config/org/extras")
(ads/load-config "config/prog")
(ads/load-config "config/files")
(ads/load-config "config/text")
(ads/load-config "config/completion")
(ads/load-config "config/d2")
(ads/load-config "config/dired")
(ads/load-config "config/modeline")
(ads/load-config "config/vc")
(ads/load-config "config/evil")
(ads/load-config "config/ghostel")
(ads/load-config "lisp/gps-time")
(ads/load-config "lisp/insert-variable-value")
(ads/load-config "config/knockknock")
(ads/load-config "config/org/org")
(ads/load-config "lisp/org-reviews")
(ads/load-config "config/org/agenda")
(ads/load-config "config/org/appearance")
(ads/load-config "config/org/babel")
(ads/load-config "config/org/capture")
(ads/load-config "lisp/org-latex-preview")
(ads/load-config "lisp/org-meetings")
(ads/load-config "lisp/org-prettify-symbols")
(ads/load-config "config/org/roam")
(ads/load-config "lisp/org-inbox-review")
(ads/load-config "config/org/timegrid")
(ads/load-config "config/org/transclusion")
(ads/load-config "lisp/quartz")
(ads/load-config "lisp/read-only-directories")
(ads/load-config "config/toggl")
(ads/load-config "lisp/window-resize")
(ads/load-config "config/platform")

;;; init.el ends here
