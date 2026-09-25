;;; toggl.el --- Time zones and toggl time tracking  -*- lexical-binding: t; -*-
;;; Commentary:
;; time-zones, saved timers, the mode line, the package, toggl sketchybar
;;; Code:

(use-package time-zones)

(defvar ads/toggl-saved-timers
  '(;; Personal productivity
    ("emacs"        "Configuration" t)
    ("nix"          "Configuration" t)
    ("mac"          "Configuration" t)
    ("CAD"          "Design"        t)
    ("Anki"         "生词"          t)
    ("Moving"       "Moving")
    ;; General
    ("Admin"        "Admin")
    ("Housekeeping" "Housekeeping")
    ("Walk"         "Walks")
    ("Cooking"      "Cooking")
    ("Chess"        "Chess")
    ("Editing"      "Photography")
    ;; Dong something good for me
    ("Weights"       "Gym")
    ("Run"           "Gym")
    ("Bike"          "Gym"))
  "Timers started by hand, as (DESCRIPTION PROJECT &optional BILLABLE TAGS).")

(defface ads/modeline-context '((t :inherit shadow))
  "Mode line text that is context rather than the thing itself."
  :group 'mode-line-faces)

(defun ads/toggl-mode-line ()
  "Return the running Toggl entry for the mode line, or nil."
  (when-let* ((entry (toggl-current-entry)))
    (let ((minutes (/ (or (toggl-entry-elapsed entry) 0) 60))
          (project (toggl-entry-project-name entry))
          (description (toggl-entry-description entry)))
      (concat
       (propertize (if (< minutes 60)
                       (format " %dm" minutes)
                     (format " %dh%02d" (/ minutes 60) (% minutes 60)))
                   'face 'ads/modeline-context)
       (when description
         (propertize (concat " " description) 'face 'ads/modeline-context))
       (when project
         ;; The client marker is already a separator, and a coloured one, so a
         ;; bullet in front of it is two dots doing one job.  Supply the
         ;; separator only for a project with no client to mark it.
         (let ((label (toggl-colorize-project project)))
           (concat (if (string-prefix-p toggl-project-marker label)
                       " "
                     (propertize " · " 'face 'ads/modeline-context))
                   label)))))))

(use-package plz)

(use-package toggl
  :vc (:url "git@github.com:andanao/emacs-toggl-track.git" :rev :newest)
  :custom
  ;; The note's repo decides which client a newly created project lands under.
  (toggl-org-client-directory-alist '(("~/git/org/personal" . "Adrian")))
  (toggl-default-client "K2")
  ;; Dailies are titled after their date, so clocking in from one must not
  ;; create a project per calendar day.
  (toggl-org-daily-project "Admin")
  ;; Timers I start by hand.  Work entries get appended in konfig.
  (toggl-saved-timers ads/toggl-saved-timers)
  :config
  (require 'toggl-org)
  (require 'toggl-transient)
  (toggl-mode 1)
  (toggl-org-mode 1)
  ;; `toggl-mode' puts its own string in the mode line; the clock is drawn by a
  ;; doom-modeline segment instead, so the parts can carry their own faces.  The
  ;; string stays live and its one second timer is what keeps that segment fresh.
  (setq global-mode-string (delq 'toggl-mode-line-string global-mode-string))
  ;; The transient is the hub: saved timers, start, stop, edits and the entry
  ;; list all hang off it.  Stop keeps its own key because it is the one thing
  ;; worth hitting without reading a menu first.
  (ads/leader-keys
    "C-t" 'toggl-dispatch
    "C-S-t" 'toggl-stop))

(defun ads/toggl-sketchybar-color (entry)
  "Return ENTRY's project colour as a sketchybar ARGB string.

Taken from Toggl rather than a table kept beside the bar.  The old
plugin hardcoded a colour per project name, which drifted the moment a
project was renamed, recoloured or added — and silently, since a miss
just fell through to white."
  (let* ((name (and entry (toggl-entry-project-name entry)))
         (hex (and name (toggl-project-color (toggl-project-by-name name)))))
    (if (and hex (= (length hex) 7))
        (concat "0xff" (downcase (substring hex 1)))
      "0xffffffff")))

(defun ads/toggl-sketchybar-update (entry)
  "Push ENTRY to sketchybar immediately.
Writes the cache file the plugin reads, then triggers a redraw."
  (with-temp-file "/tmp/toggl_sketchybar_cache"
    (insert
     (if entry
         (format "%d|%s|%s|%s"
                 (floor (float-time (toggl-entry-start entry)))
                 (or (toggl-entry-description entry) "")
                 (or (toggl-entry-project-name entry) "")
                 (ads/toggl-sketchybar-color entry))
       "STOPPED")))
  (call-process "/opt/homebrew/bin/sketchybar" nil 0 nil
                "--trigger" "toggl_update"))

(with-eval-after-load 'toggl
  (add-hook 'toggl-entry-changed-functions #'ads/toggl-sketchybar-update))

;;; toggl.el ends here
