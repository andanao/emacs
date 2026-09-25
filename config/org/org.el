;;; org.el --- Core org setup, tags, todo keywords, keybindings  -*- lexical-binding: t; -*-
;;; Commentary:
;; org, org-tags, org-todo-keywords, org keybindings,
;; resize the image at point, org url links
;;; Code:

(use-package org
  :custom
  ;; (org-directory "~/org") ;; org directory set in early init
  (org-ellipsis " ·")
  (org-log-done 'time)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-hidden-keywords '(title))
  (org-hide-emphasis-markers t)
  ;; List, because a bare a number here means org never reads a per-image
  (org-image-actual-width '(0.75))
  (org-startup-with-inline-images t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-cycle-separator-lines 0)
  (org-id-link-to-org-use-id nil) ;; Use org roam linking
  ;; Headings inherit the file level roam ID, so attachments work without a :DIR: drawer
  (org-attach-use-inheritance t)
  (org-fast-tag-selection-single-key t)
  (org-blank-before-new-entry '((heading . 1) (plain-list-item . nil)))
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")))
  (org-refile-use-outline-path 'title)
  (org-outline-path-complete-in-steps nil)
  ;; Clocking in starts a toggl timer, so the mode line was counting the same
  ;; work twice, from two different start times.  Toggl keeps the clock.
  (org-clock-clocked-in-display nil)
  (calendar-week-start-day 0)
  :hook
  (org-mode . variable-pitch-mode))

(setq org-tag-alist
   '(("article" . ?r)
     ("book" . ?b)
     ("private" . ?P)
     ("thoughts" . ?t)
     ("public" . ?u)
     ;; Projects
     ("project" . ?p)   ; Generic/idea stage
     ("project_a" . ?a) ; Active
     ("project_h" . ?h) ; Hold
     ("project_c" . ?c) ; Complete
     ))

(add-to-list 'org-todo-keywords
'(sequence
  "TIX(i)"  ;; Ticketed but not started
  "DEV(e!)" ;; I'm writing code
  "REV(v!)" ;; Someone else is reviewing code
  "|"
  "DONE(d!)" ;; Victory
  "CANC(k!)" ;; Doesn't matter anymore
  "DELE(L!)" ;; Delegated out to someone
  ) t )

(general-define-key
 :states '(normal) :keymaps 'org-mode-map
 (kbd "<tab>") 'org-cycle
 (kbd "<backtab>") 'org-shifttab
 "C-j" 'org-next-visible-heading
 "C-k" 'org-previous-visible-heading)

(general-define-key
 :states  '(motion) :keymaps 'org-mode-map
 (kbd "RET") 'org-open-at-point)

(defun ads/org-scratch ()
  "Open ~/scratch.org"
  (interactive)
  (if (eq system-type 'windows-nt)
      (find-file (concat "c:/users/" user-login-name "/scratch.org"))
      (find-file "~/scratch.org")))

(ads/leader-keys
  "oM" 'org-mode
  "oS" 'org-save-all-org-buffers
  "C-c" 'org-clock-goto
  "C-s" 'ads/org-scratch)
(ads/leader-def "od" "dirvish org" (dirvish org-directory))

;; Scoped to `org-mode-map' rather than `:major-modes', which does not restrict
;; bindings placed in the override map.  `oo', `of' and `ns' now live in the
;; global narrow section as mode-dispatching commands.
(ads/leader-keys
  :keymaps 'org-mode-map
  "oh" 'consult-org-heading
  "o TAB" 'org-cycle-global
  ;; Inactive and with the time of day, the stamp `%U' writes into :CREATED:.
  "it" '((lambda () (interactive) (org-insert-time-stamp (current-time) t t))
         :wk "timestamp")
  "ti" 'org-link-preview-refresh
  "tI" 'org-link-preview
  "ne" 'org-narrow-to-element
  "nb" 'org-narrow-to-block
  )

(defun ads/org-image--overlay ()
  "The preview overlay showing an image at point, or nil.
Point counts as on an image when the char it's on carries one or, so a
cursor resting just past it still counts, when the char before it does.
The overlay, not the display property: its bounds are exactly this one
image's, and being a marker pair they stay correct across the edit that
resizing makes."
  (and (derived-mode-p 'org-mode)
       (seq-some (lambda (pos)
                   (seq-find (lambda (o)
                               (eq (car-safe (overlay-get o 'display)) 'image))
                             (overlays-at pos)))
                 (delq nil (list (point)
                                 (and (> (point) (point-min)) (1- (point))))))))

(defun ads/org-image-at-point ()
  "Non-nil when point is on an org inline image."
  (and (ads/org-image--overlay) t))

(defun ads/org-image--link-at-point ()
  "The link element behind the image at point."
  (unless (ads/org-image-at-point) (user-error "No image at point"))
  (or (org-element-lineage (org-element-context) 'link t)
      (user-error "No link at point")))

(defun ads/org-image--width (link)
  "The width recorded for LINK, or the `org-image-actual-width' default."
  (let* ((paragraph (org-element-lineage link 'paragraph))
         (attr (and paragraph
                    (org-export-read-attribute :attr_org paragraph :width)))
         (default (org-property-or-variable-value 'org-image-actual-width)))
    (or (and (stringp attr)
             (string-match-p "\\`[0-9.]+\\'" attr)
             (string-to-number attr))
        (and (consp default) (numberp (car default)) (car default))
        (and (numberp default) default)
        0.75)))

(defun ads/org-image--set-width (link width)
  "Record WIDTH for LINK on a `#+ATTR_ORG:' line above it, or drop it when nil.
Any other attributes already on that line are kept."
  (save-excursion
    (goto-char (org-element-begin link))
    (beginning-of-line)
    (let ((indent (make-string (current-indentation) ?\s))
          (attrs ""))
      (when (and (not (bobp))
                 (save-excursion
                   (forward-line -1)
                   (looking-at "[ \t]*#\\+ATTR_ORG:[ \t]*\\(.*?\\)[ \t]*$")))
        (setq attrs (match-string 1))
        (forward-line -1)
        (delete-region (line-beginning-position) (line-beginning-position 2)))
      (setq attrs (string-trim
                   (replace-regexp-in-string ":width[ \t]+[^ \t]+" "" attrs)))
      (cond (width
             (insert indent (format "#+ATTR_ORG: :width %.2f" width)
                     (if (string-empty-p attrs) "" (concat " " attrs))
                     "\n"))
            ((not (string-empty-p attrs))
             (insert indent "#+ATTR_ORG: " attrs "\n"))))))

(defun ads/org-image--rewidth (width)
  "Give the image at point WIDTH, or the default when nil, and re-display it.
Only this image: the range is its own overlay's, held as markers so the
`#+ATTR_ORG:' line going in above it doesn't shift them out from under
us.  Clearing first is the point - `org-link-preview-region' refreshes
\"only if necessary\" and would otherwise keep the overlay it has,
leaving the new width off the screen."
  (let* ((overlay (or (ads/org-image--overlay) (user-error "No image at point")))
         ;; All three advance past an insertion at their own position, which
         ;; is the usual one: the cursor sits at the start of the image, the
         ;; overlay starts there too, and that is exactly where the
         ;; `#+ATTR_ORG:' line goes in.  A plain marker would stay put and
         ;; leave point stranded on the new line, one keypress from the image.
         (beg (copy-marker (overlay-start overlay) t))
         (end (copy-marker (overlay-end overlay) t))
         (origin (copy-marker (point) t)))
    (unwind-protect
        (progn
          (ads/org-image--set-width (ads/org-image--link-at-point) width)
          (org-link-preview-clear beg end)
          (org-link-preview-region nil t beg end)
          (goto-char origin))
      (set-marker beg nil)
      (set-marker end nil)
      (set-marker origin nil))))

(defun ads/org-image-scale (direction)
  "Step the width of the image at point by DIRECTION, a twentieth each way.
Widths are a fraction of the text width - what `org-image-actual-width'
already deals in - so a resized image still reflows with the window."
  (let ((width (max 0.05 (min 2.0 (+ (ads/org-image--width
                                      (ads/org-image--link-at-point))
                                     (* direction 0.05))))))
    (ads/org-image--rewidth width)
    (message "Image width: %d%% of text width" (round (* width 100)))))

(defun ads/org-image-scale-increase ()
  "Widen the image at point by one step."
  (interactive)
  (ads/org-image-scale 1))

(defun ads/org-image-scale-decrease ()
  "Narrow the image at point by one step."
  (interactive)
  (ads/org-image-scale -1))

(defun ads/org-image-scale-reset ()
  "Drop the image at point's own width, back to `org-image-actual-width'."
  (interactive)
  (ads/org-image--rewidth nil)
  (message "Image width: default"))

(general-define-key
 :states '(normal) :keymaps 'org-mode-map
 "+" (ads/image-key ads/org-image-at-point 'ads/org-image-scale-increase)
 "-" (ads/image-key ads/org-image-at-point 'ads/org-image-scale-decrease)
 "0" (ads/image-key ads/org-image-at-point 'ads/org-image-scale-reset))

(defun org-insert-link-from-kill ()
  "Insert an org-mode link using URL from kill ring and prompting for description.
First tries the most recent kill ring item, then searches kill ring history for a URL."
  (interactive)
  (let* ((first-item (current-kill 0))
         (url (if (string-match-p "^https?://" first-item)
                  first-item
                (cl-loop for i from 0 below (min kill-ring-max 20)
                         for item = (ignore-errors (current-kill i))
                         when (and item (string-match-p "^https?://" item))
                         return item)))
         (prompt-text (if url
                          (format "Link URL: %s\n"
                                  (if (> (length url) 100)
                                      (concat (substring url 0 100) "...")
                                    url))
                        (format "Not car of kill ring!\nLink URL: %s\n"
                                (if (> (length first-item) 100)
                                    (concat (substring first-item 0 100) "...")
                                  first-item))))
         (description (if url
                          (read-string prompt-text)
                        (progn
                          (message "No URL found in kill ring history")
                          nil))))
    (cond
     ((null description)
      (message "No URL available to create link"))
     ((string-empty-p description)
      (message "No description provided, link not inserted"))
     (t
      (insert (format "[[%s][%s]]" url description))))))

(ads/leader-keys "ol" 'org-insert-link-from-kill)

;;; org.el ends here
