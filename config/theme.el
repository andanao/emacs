;;; theme.el --- Fonts, Modus themes and per-project colours  -*- lexical-binding: t; -*-
;;; Commentary:
;; fonts, modus-themes, modus-tweaks, force reload, project colors
;;; Code:

(setq
 mono "FiraCode Nerd Font"
 sans "Cantarell"
 serif "EtBembo")

;; Set Font sizes
(defvar default-font-size 140)

;; Set default font
(set-face-attribute 'default nil
		    :font mono
		    :family mono
		    :height default-font-size)

(set-face-attribute 'fixed-pitch nil
		    :font mono
		    :family mono
		    :height 1.0)

(set-face-attribute 'variable-pitch nil
		    :font serif
		    :family serif
		    :height 1.1
		    :weight 'regular)

(customize-set-variable 'line-spacing 0.25)

(use-package modus-themes
  :demand t
  :ensure t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-bold-constructs t))

(setq modus-themes-to-toggle
      '(modus-operandi modus-vivendi))

(setq modus-themes-headings
  '((0 . (regular 1.75))
    (1 . (regular 1.25))
    (2 . (regular 1.20))
    (3 . (regular 1.15))
    (t . (regular 1.10))
    ))

;; all
(setq modus-themes-common-palette-overrides
  '((bg-prose-block-contents bg-main)
    (bg-prose-block-delimiter bg-main)
    (fg-heading-0 fg-main)
    (fg-heading-1 fg-main)
    (fg-heading-2 fg-main)
    (fg-heading-3 fg-main)
    (fg-heading-4 fg-main)
    (fg-heading-5 fg-main)
    (fg-heading-6 fg-main)
    (fg-heading-7 fg-main)
    (fg-heading-8 fg-main)
    (fringe bg-main)
    (bg-mode-line-active bg-dim)
    (bg-mode-line-inactive bg-main)
    ;; links are underlined, never coloured
    (fg-link fg-main)
    (fg-link-symbolic fg-main)
    (fg-link-visited fg-main)
    (underline-link fg-main)
    (underline-link-symbolic fg-main)
    (underline-link-visited fg-main)
    ;; line numbers sit flush against the buffer
    (bg-line-number-active bg-main)
    (bg-line-number-inactive bg-main)
    ;; org
    (prose-done fg-dim)
    (prose-table fg-main)
    (fg-prose-code fg-alt)
    (fg-prose-verbatim fg-main)
    (date-event fg-main)
    (date-scheduled fg-main)
    (date-scheduled-subtle fg-main)))

;; dark theme
(setq modus-vivendi-palette-overrides
      '((cursor "#ff2060")
        (bg-main "#111111")))

;;light theme
(setq modus-operandi-palette-overrides
    '((cursor red-intense)
      (bg-main "#fffff8")))
;; (modus-themes-select (modus-themes-get-current-theme))

(defun ads/modus-color (name)
  "Return the current Modus palette value for NAME, overrides included."
  (or (modus-themes-get-color-value name :with-overrides) 'unspecified))

(defun ads/modus-tweaks (&optional theme &rest _)
  "Apply custom face tweaks for the Modus THEME just enabled.
Called with no THEME, tweak whichever Modus theme is current."
  (when (or (null theme) (string-prefix-p "modus-" (symbol-name theme)))
    (with-demoted-errors "ads/modus-tweaks: %S"
      (let ((c '((class color) (min-colors 256)))
            (bg-main (ads/modus-color 'bg-main))
            (bg-dim (ads/modus-color 'bg-dim))
            (bg-inactive (ads/modus-color 'bg-inactive))
            (bg-blue-nuanced (ads/modus-color 'bg-blue-nuanced))
            (bg-blue-subtle (ads/modus-color 'bg-blue-subtle))
            (fg-main (ads/modus-color 'fg-main))
            (fg-dim (ads/modus-color 'fg-dim))
            (gold (ads/modus-color 'gold)))
        (custom-set-faces
         ;; org mode
         `(org-checkbox ((,c :foreground ,fg-main)))
         `(org-quote ((,c :inherit italic :foreground ,gold :height 1.1)))
         `(org-document-info ((,c :foreground ,fg-main)))
         `(org-drawer ((,c :height 0.9)))
         `(org-property-value ((,c :height 0.9)))
         `(org-ellipsis ((,c :foreground ,bg-main)))
         `(org-modern-label ((,c :height 0.7 :inherit fixed-pitch)))
         `(org-scheduled-previously ((,c :inherit org-scheduled-today :weight normal)))
         `(org-modern-date-active ((,c :inherit org-modern-label :background ,bg-blue-nuanced)))
         `(org-modern-time-active ((,c :inherit org-modern-label :background ,bg-blue-subtle)))
         `(org-modern-date-inactive ((,c :inherit org-modern-label :background ,bg-dim)))
         `(org-modern-time-inactive ((,c :inherit org-modern-label :background ,bg-inactive)))
         ;; markdown mode - headings borrow org's so both scale identically
         `(markdown-ts-heading-1 ((,c :inherit org-level-1)))
         `(markdown-ts-heading-2 ((,c :inherit org-level-2)))
         `(markdown-ts-heading-3 ((,c :inherit org-level-3)))
         `(markdown-ts-heading-4 ((,c :inherit org-level-4)))
         `(markdown-ts-heading-5 ((,c :inherit org-level-5)))
         `(markdown-ts-heading-6 ((,c :inherit org-level-6)))
         `(markdown-ts-list-marker ((,c :foreground ,fg-main)))
         `(markdown-ts-code-span ((,c :inherit fixed-pitch :foreground ,fg-main)))
         `(markdown-ts-code-block ((,c :inherit fixed-pitch :background ,bg-dim :extend t)))
         `(markdown-ts-language-keyword ((,c :background ,bg-dim )))
         ;; line numbers
         `(line-number ((,c :height 0.8)))
         `(line-number-current-line ((,c :inherit line-number)))
         ;; mode line - the buffer name and the clocked project are what you read
         `(ads/modeline-context ((,c :foreground ,fg-dim)))
         ;; misc
         `(bookmark-face ((,c :foreground ,fg-dim :distant-foreground ,fg-dim)))))
      )))

(add-hook 'enable-theme-functions #'ads/modus-tweaks)

(defun ads/modus-tolerant-hooks (fn &rest args)
  "Call FN with `modus-themes-after-load-theme-hook' made error tolerant.
`run-hooks' stops at the first error, which would otherwise let one bad
hook leave the rest of a toggle half applied."
  (let ((modus-themes-after-load-theme-hook
         (mapcar (lambda (f)
                   (if (functionp f)
                       (lambda () (with-demoted-errors "modus theme hook: %S" (funcall f)))
                     f))
                 modus-themes-after-load-theme-hook)))
    (apply fn args)))

(advice-add 'modus-themes-load-theme :around #'ads/modus-tolerant-hooks)

(load-theme 'modus-vivendi t)

(defvar ads/modus-reload-log (concat user-emacs-directory "modus-reload.log")
  "File where forced theme reloads are recorded, one entry each.")

(defun ads/modus--recent-messages (n)
  "The last N lines of the *Messages* buffer, or nil if there is none."
  (when-let* ((buf (get-buffer "*Messages*")))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line (- n))
        (buffer-substring-no-properties (point) (point-max))))))

(defun ads/modus-log-reload (theme)
  "Record a forced reload of THEME: where it was asked for and what was said."
  (let ((header (format "%s %s from %s (%s)\n"
                        (format-time-string "[%Y-%m-%d %a %H:%M]")
                        theme (buffer-name) major-mode))
        (tail (string-trim-right (or (ads/modus--recent-messages 20) ""))))
    (write-region (concat header
                          (unless (string= tail "")
                            (concat (replace-regexp-in-string "^" "  | " tail) "\n"))
                          "\n")
                  nil ads/modus-reload-log :append :silent)))

(defun ads/modus-reload ()
  "Load the current Modus theme again, palette overrides and tweaks included."
  (interactive)
  (let ((theme (modus-themes-get-current-theme)))
    (ads/modus-log-reload theme)
    (modus-themes-load-theme theme)
    (message "Reloaded %s" theme)))

(defconst ads/project-palette
  '(bg-blue-subtle bg-green-subtle bg-yellow-subtle bg-magenta-subtle bg-cyan-subtle
    bg-red-subtle bg-lavender bg-sage bg-clay bg-ochre)
  "Colours to hand out, one per project.  All exist in operandi and vivendi.")

(defvar ads/project-colour-pins
  '(("spotify" . bg-green-subtle)
    ("zmk-config" . bg-cyan-suble))
  "Projects whose colour never moves, keyed by the name of their root directory.
A nil colour means no tint at all.  Work projects are pushed on in konfig.")

(defvar ads/project-colour-exclude
  '("~/git/emacs/" "~/git/konfig/" "~/git/kode/" "~/git/org/" "~/Downloads/")
  "Projects under these directories keep the plain modeline.
Matched as directories, so =~/git/emacs/= leaves =~/git/emacs-toggl= alone.")

(defvar ads/project--assigned nil
  "Alist of project root -> colour for the projects currently open.")

(defvar-local ads/project--remap nil
  "Face-remap cookies owned by this buffer, so re-running can't leak them.")

(defun ads/project-root ()
  "Root of the git project this buffer is in, or nil if it gets no colour.
Not only file buffers: the agent shell, dired and magit all sit on a project's
`default-directory' and want the same colour as the code they're about."
  (and default-directory
       (not (file-remote-p default-directory))
       (when-let* ((dir (locate-dominating-file default-directory ".git"))
                   (root (file-name-as-directory (expand-file-name dir))))
         (and (not (seq-some
                    (lambda (d)
                      (string-prefix-p (file-name-as-directory (expand-file-name d)) root))
                    ads/project-colour-exclude))
              (directory-file-name root)))))

(defun ads/project--live-roots ()
  "Roots with at least one buffer still open."
  (delete-dups
   (delq nil (mapcar (lambda (b) (with-current-buffer b (ads/project-root)))
                     (buffer-list)))))

(defun ads/project-colour (root)
  "Colour for ROOT, allocating one on first sight, or nil if it stays bare.
A pinned project is answered as-is - assoc, not the cdr, so a pin to nil
reads as \"no tint\" rather than as a cache miss."
  (if-let* ((pin (assoc (file-name-nondirectory root) ads/project-colour-pins)))
      (cdr pin)
    (or (cdr (assoc root ads/project--assigned))
        (let ((live (ads/project--live-roots)))
          ;; Only prune here: a colour is free the moment its project is closed,
          ;; but nobody needs to know until something else asks for one.
          (setq ads/project--assigned
                (seq-filter (lambda (cell) (member (car cell) live))
                            ads/project--assigned))
          (let* ((taken (delq nil (append (mapcar #'cdr ads/project-colour-pins)
                                          (mapcar #'cdr ads/project--assigned))))
                 (colour (or (seq-find (lambda (c) (not (memq c taken)))
                                       ads/project-palette)
                             (car ads/project-palette))))
            (push (cons root colour) ads/project--assigned)
            colour)))))

(defun ads/project--tint (face bg)
  "Remap FACE to background BG, recolouring its :box to match.
spacious-padding draws the modeline padding as a :box in the background
colour, so tinting only :background leaves the outer ring untinted."
  (let ((box (face-attribute face :box nil t)))
    (apply #'face-remap-add-relative face :background bg
           (when (consp box)
             (list :box (plist-put (copy-sequence box) :color bg))))))

(defun ads/project-colourise ()
  "Tint this buffer's modeline with its project colour, if it has one."
  (when-let* ((root (ads/project-root)))
    (mapc #'face-remap-remove-relative ads/project--remap)
    (setq ads/project--remap nil)
    (when-let* ((colour (ads/project-colour root))
                (bg (ads/modus-color colour)))
      (when (stringp bg)
        (setq ads/project--remap
              (list (ads/project--tint 'mode-line-active bg)
                    (ads/project--tint 'mode-line-inactive bg)))))))

(defun ads/project-recolourise ()
  "Re-resolve the tints after a theme change - the old values are stale hex."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when ads/project--remap (ads/project-colourise)))))

(add-hook 'after-change-major-mode-hook #'ads/project-colourise)
(add-hook 'modus-themes-after-load-theme-hook #'ads/project-recolourise)

;;; theme.el ends here
