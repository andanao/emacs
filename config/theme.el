;;; theme.el --- Fonts, Ef themes and per-project colours  -*- lexical-binding: t; -*-
;;; Commentary:
;; fonts, cjk width, ef-themes, theme tweaks, force reload,
;; project colors
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

(let ((cjk "Hiragino Sans"))
  (when (member cjk (font-family-list))
    (dolist (charset '(han cjk-misc kana bopomofo))
      (set-fontset-font t charset (font-spec :family cjk)))
    (setf (alist-get cjk face-font-rescale-alist nil nil #'equal) 1.2)))

;; ef-themes 2.x is built on the Modus engine, so the `ef-themes-' options are
;; aliases onto the Modus ones.  They only exist once ef-themes has loaded,
;; which is why these are set after the form rather than in `:custom' - that
;; runs first and would quietly create plain variables nothing reads.
(use-package ef-themes
  :demand t
  :ensure t)

(setq ef-themes-mixed-fonts t)
(setq ef-themes-bold-constructs t)

(setq ef-themes-to-toggle
      '(ef-cyprus ef-autumn))

(setq ef-themes-headings
      '((0 . (regular 1.75)) ;; title
        (1 . (regular 1.25))
        (2 . (regular 1.20))
        (3 . (regular 1.15))
        (t . (regular 1.10))
        ))

;; all
(setq ef-themes-common-palette-overrides
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

;; dark theme.  Both values match ef-autumn's own defaults; they are written
;; out so a palette change upstream cannot move them, and so the alternatives
;; stay next to what they are alternatives to.
(setq ef-autumn-palette-overrides
      '((fg-main "#cfbcba")                ; default; #e4d9d8 and #f9f6f6 are brighter
        (cursor "#ffaa33")))               ; orange; #ff3388 pink, #ff4433 red, #88ff33 green

;; light theme.  ef-cyprus's own bg-main is already a warm cream, so it needs
;; no paper tint of its own.
(setq ef-cyprus-palette-overrides
      '((cursor red-intense)))

(defun ads/theme-color (name)
  "Return the current theme's palette value for NAME, overrides included."
  (or (ef-themes-get-color-value name :with-overrides) 'unspecified))

(defun ads/theme-tweaks (&optional theme &rest _)
  "Apply custom face tweaks for the Ef THEME just enabled.
Called with no THEME, tweak whichever Ef theme is current."
  (when (or (null theme)
            (string-prefix-p "ef-" (symbol-name theme)))
    (with-demoted-errors "ads/theme-tweaks: %S"
      (let ((c '((class color) (min-colors 256)))
            (bg-main (ads/theme-color 'bg-main))
            (bg-dim (ads/theme-color 'bg-dim))
            (bg-inactive (ads/theme-color 'bg-inactive))
            (bg-blue-nuanced (ads/theme-color 'bg-blue-nuanced))
            (bg-blue-subtle (ads/theme-color 'bg-blue-subtle))
            (fg-main (ads/theme-color 'fg-main))
            (fg-dim (ads/theme-color 'fg-dim))
            (gold (ads/theme-color 'gold)))
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

(add-hook 'enable-theme-functions #'ads/theme-tweaks)

(defun ads/theme-tolerant-hooks (fn &rest args)
  "Call FN with `ef-themes-after-load-theme-hook' made error tolerant.
`run-hooks' stops at the first error, which would otherwise let one bad
hook leave the rest of a toggle half applied."
  (let ((ef-themes-after-load-theme-hook
         (mapcar (lambda (f)
                   (if (functionp f)
                       (lambda () (with-demoted-errors "theme hook: %S" (funcall f)))
                     f))
                 ef-themes-after-load-theme-hook)))
    (apply fn args)))

;; Must advise the Modus name, not `ef-themes-load-theme'.  That is a defalias,
;; so advice on it only catches calls made through the alias - and the Ef
;; commands, `ef-themes-toggle' included, call the Modus function directly.
;; Advised here it fires for both.
(advice-add 'modus-themes-load-theme :around #'ads/theme-tolerant-hooks)

(load-theme 'ef-autumn t)

(defvar ads/theme-reload-log (concat user-emacs-directory "theme-reload.log")
  "File where forced theme reloads are recorded, one entry each.")

(defun ads/theme--recent-messages (n)
  "The last N lines of the *Messages* buffer, or nil if there is none."
  (when-let* ((buf (get-buffer "*Messages*")))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line (- n))
        (buffer-substring-no-properties (point) (point-max))))))

(defun ads/theme-log-reload (theme)
  "Record a forced reload of THEME: where it was asked for and what was said."
  (let ((header (format "%s %s from %s (%s)\n"
                        (format-time-string "[%Y-%m-%d %a %H:%M]")
                        theme (buffer-name) major-mode))
        (tail (string-trim-right (or (ads/theme--recent-messages 20) ""))))
    (write-region (concat header
                          (unless (string= tail "")
                            (concat (replace-regexp-in-string "^" "  | " tail) "\n"))
                          "\n")
                  nil ads/theme-reload-log :append :silent)))

(defun ads/theme-reload ()
  "Load the current theme again, palette overrides and tweaks included."
  (interactive)
  ;; `modus-themes-get-current-theme' has no `ef-themes-' alias; it is the one
  ;; reader left that has to go by the Modus name.
  (let ((theme (modus-themes-get-current-theme)))
    (ads/theme-log-reload theme)
    (ef-themes-load-theme theme)
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

(defvar ads/project-colour-file
  (expand-file-name "tmp/project-colours.eld" user-emacs-directory)
  "Where the colours set with \\[ads/project-modeline-color-set] are kept.")

(defvar ads/project-colour-overrides
  (with-demoted-errors "Reading project colours: %S"
    (when (file-exists-p ads/project-colour-file)
      (with-temp-buffer
        (insert-file-contents ads/project-colour-file)
        (read (current-buffer)))))
  "Colours set by hand, keyed like `ads/project-colour-pins' and beating them.")

(defun ads/project--save-overrides ()
  "Write `ads/project-colour-overrides' out for the next session."
  (with-demoted-errors "Saving project colours: %S"
    (make-directory (file-name-directory ads/project-colour-file) t)
    (with-temp-file ads/project-colour-file
      (prin1 ads/project-colour-overrides (current-buffer)))))

(defvar ads/project-colour-exclude
  '("~/git/emacs/" "~/git/konfig/" "~/git/kode/" "~/git/org/" "~/Downloads/")
  "Projects under these directories keep the plain modeline.
Matched as directories, so =~/git/emacs/= leaves =~/git/emacs-toggl= alone.")

(defvar ads/project--assigned nil
  "Alist of project root -> colour for the projects currently open.")

(defvar-local ads/project--remap nil
  "Face-remap cookies owned by this buffer, so re-running can't leak them.")

(defun ads/project-root ()
  "Root of the git project this buffer is in, or nil if it isn't in one.
Not only file buffers: the agent shell, dired and magit all sit on a project's
`default-directory' and want the same colour as the code they're about."
  (and default-directory
       (not (file-remote-p default-directory))
       (when-let* ((dir (locate-dominating-file default-directory ".git")))
         (directory-file-name (expand-file-name dir)))))

(defun ads/project--excluded-p (root)
  "Non-nil if ROOT sits under one of `ads/project-colour-exclude'."
  (seq-some (lambda (d)
              (string-prefix-p (file-name-as-directory (expand-file-name d))
                               (file-name-as-directory root)))
            ads/project-colour-exclude))

(defun ads/project--live-roots ()
  "Roots with at least one buffer still open."
  (delete-dups
   (delq nil (mapcar (lambda (b) (with-current-buffer b (ads/project-root)))
                     (buffer-list)))))

(defun ads/project-colour (root)
  "Colour for ROOT, allocating one on first sight, or nil if it stays bare.
A pin or an override is answered as-is - assoc, not the cdr, so one set to
nil reads as \"no tint\" rather than as a cache miss."
  (let ((name (file-name-nondirectory root)))
    (cond
     ;; Asked for by hand, so it beats the exclude list too.
     ((assoc name ads/project-colour-overrides)
      (cdr (assoc name ads/project-colour-overrides)))
     ((ads/project--excluded-p root) nil)
     ((assoc name ads/project-colour-pins)
      (cdr (assoc name ads/project-colour-pins)))
     (t
      (or (cdr (assoc root ads/project--assigned))
          (let ((live (ads/project--live-roots)))
            ;; Only prune here: a colour is free the moment its project is closed,
            ;; but nobody needs to know until something else asks for one.
            (setq ads/project--assigned
                  (seq-filter (lambda (cell) (member (car cell) live))
                              ads/project--assigned))
            (let* ((taken (delq nil (append (mapcar #'cdr ads/project-colour-pins)
                                            (mapcar #'cdr ads/project-colour-overrides)
                                            (mapcar #'cdr ads/project--assigned))))
                   (colour (or (seq-find (lambda (c) (not (memq c taken)))
                                         ads/project-palette)
                               (car ads/project-palette))))
              (push (cons root colour) ads/project--assigned)
              colour)))))))

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
                (bg (ads/theme-color colour)))
      (when (stringp bg)
        (setq ads/project--remap
              (list (ads/project--tint 'mode-line-active bg)
                    (ads/project--tint 'mode-line-inactive bg)))))))

(defun ads/project-recolourise ()
  "Re-resolve the tints after a theme change - the old values are stale hex."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when ads/project--remap (ads/project-colourise)))))

(defun ads/project--recolourise-root (root)
  "Re-tint every buffer sitting in ROOT, tinted until now or not."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (equal (ads/project-root) root) (ads/project-colourise)))))

(defun ads/project--read-root ()
  "The project to recolour: this buffer's, or one that is already open."
  (or (ads/project-root)
      (let ((roots (ads/project--live-roots)))
        (unless roots (user-error "No project here and none open"))
        (completing-read "Project: " roots nil t))))

(defun ads/project-modeline-color-set (root colour)
  "Tint ROOT's modeline with COLOUR, now and in every session after.
An empty COLOUR means no tint, which is how to mute one project without
touching `ads/project-colour-exclude'."
  (interactive
   (let* ((root (ads/project--read-root))
          (current (ads/project-colour root)))
     (list root
           (completing-read (format "Colour for %s (empty for none): "
                                    (file-name-nondirectory root))
                            (mapcar #'symbol-name ads/project-palette)
                            nil nil nil nil (and current (symbol-name current))))))
  (let ((colour (and (not (string-empty-p colour)) (intern colour)))
        (name (file-name-nondirectory root)))
    (when (and colour (not (stringp (ads/theme-color colour))))
      (user-error "`%s' is not a colour in this theme" colour))
    (setf (alist-get name ads/project-colour-overrides nil nil #'equal) colour)
    ;; Drop any allocation, so the colour it was holding goes back in the pool.
    (setq ads/project--assigned (assoc-delete-all root ads/project--assigned))
    (ads/project--save-overrides)
    (ads/project--recolourise-root root)
    (message "%s: %s" name (or colour "no tint"))))

(defun ads/project-modeline-color-unset (root)
  "Forget ROOT's hand-set colour, back to its pin or the next one going."
  (interactive (list (ads/project--read-root)))
  (let ((name (file-name-nondirectory root)))
    (unless (assoc name ads/project-colour-overrides)
      (user-error "%s has no colour set by hand" name))
    (setq ads/project-colour-overrides
          (assoc-delete-all name ads/project-colour-overrides))
    (ads/project--save-overrides)
    (ads/project--recolourise-root root)
    (message "%s: back to %s" name (or (ads/project-colour root) "no tint"))))

(add-hook 'after-change-major-mode-hook #'ads/project-colourise)
(add-hook 'ef-themes-after-load-theme-hook #'ads/project-recolourise)

;;; theme.el ends here
