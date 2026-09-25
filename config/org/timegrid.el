;;; timegrid.el --- org-timegrid calendar  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-timegrid
;;; Code:

(defvar ads/org-timegrid--underline nil
  "What to underline while a linked heading's block is being drawn.
`t' on the all-day rail, which draws its title and nothing else, and the
list of wrapped lines on a timed block, which also draws its time range.")

(defun ads/org-timegrid--category-icon (position)
  "Return the icon leading the Org category at POSITION, or nil.
Every agenda file sets =CATEGORY= to an icon, a space, and a name, so the
icon is the first character — unless that character is ASCII, which means
the file has no category and `org-get-category' fell back to its name."
  (when-let* ((category (ignore-errors (org-get-category position)))
              ((> (length category) 0))
              (icon (substring category 0 1))
              ((not (string-match-p "[[:ascii:]]" icon))))
    icon))

(defun ads/org-timegrid-note-category (fn file headline &rest args)
  "Record HEADLINE's category icon on the event it becomes.
It rides in the event's metadata rather than glued onto the title, which
is what the rename prompt starts from."
  (let ((event (apply fn file headline args)))
    (when-let* ((icon (ads/org-timegrid--category-icon
                       (org-element-property :begin headline))))
      (setf (org-timegrid-event-metadata event)
            (plist-put (org-timegrid-event-metadata event) :category-icon icon)))
    event))

(defun ads/org-timegrid--block-icon (block)
  "Return the category icon recorded on BLOCK's event, or nil."
  (when-let* ((event (org-timegrid-block-event block)))
    (plist-get (org-timegrid-event-metadata event) :category-icon)))

(defun ads/org-timegrid-plain-title (fn svg block &rest args)
  "Draw BLOCK titled by its category icon, org links as their description.
Both surfaces get a copy of the block, so none of this reaches the event
and the rename prompt still starts from the real heading."
  (if (not (org-timegrid-block-p block))
      (apply fn svg block args)
    (let* ((raw (or (org-timegrid-block-title block) ""))
           (icon (ads/org-timegrid--block-icon block))
           (copy (copy-org-timegrid-block block))
           (ads/org-timegrid--underline
            (and (string-match-p org-link-bracket-re raw) t)))
      (setf (org-timegrid-block-title copy)
            (concat (and icon (concat icon " ")) (org-link-display-format raw)))
      (apply fn svg copy args))))

(defun ads/org-timegrid-underline-wrapped (lines)
  "Narrow the underline to the LINES the title actually wrapped to."
  (when (eq ads/org-timegrid--underline t)
    (setq ads/org-timegrid--underline lines))
  lines)

(defun ads/org-timegrid-underline-links (args)
  "Underline drawn text that came from a heading carrying a link."
  (if (and ads/org-timegrid--underline
           (or (eq ads/org-timegrid--underline t)
               (member (nth 1 args) ads/org-timegrid--underline)))
      (append args '(:text-decoration "underline"))
    args))

(defun ads/org-timegrid-color (headline)
  "Colour HEADLINE by its TIMEGRID_COLOR property, else by its tags.
Tags are read the way the agenda reads them, with inheritance, so a
=#+FILETAGS: :meeting:= colours every entry in the file.  The package
reads `org-element-property' `:tags', which is only what is written on
the heading itself, and the buffer is current while it parses."
  (or (when-let* ((name (org-element-property :TIMEGRID_COLOR headline))
                  (name (string-trim name)))
        (if (assq (intern name) org-timegrid-colors) (intern name) name))
      (seq-some (lambda (tag)
                  (cdr (assoc tag org-timegrid-org-tag-color-alist)))
                (if (derived-mode-p 'org-mode)
                    (org-get-tags (org-element-property :begin headline))
                  (org-element-property :tags headline)))))

(defun ads/org-timegrid--title-links (title)
  "Return the bracket links in TITLE as an alist of description to link."
  (let ((pos 0) links)
    (while (string-match org-link-bracket-re title pos)
      (push (cons (or (match-string 2 title) (match-string 1 title))
                  (match-string 0 title))
            links)
      (setq pos (match-end 0)))
    (nreverse links)))

(defun ads/org-timegrid-open-link ()
  "Open a link from the heading behind the block at the cursor.
Reads the links off the block's title rather than visiting the file, so
it does not move point in the Org buffer to answer a question about it."
  (interactive)
  (let* ((block (or (org-timegrid--block-at-cursor)
                    (user-error "Nothing under the cursor")))
         (links (ads/org-timegrid--title-links
                 (or (org-timegrid-block-title block) ""))))
    (pcase links
      ('nil (user-error "No link in this heading"))
      (`(,only) (org-link-open-from-string (cdr only)))
      (_ (org-link-open-from-string
          (cdr (assoc (completing-read "Link: " links nil t) links)))))))

(defun ads/org-timegrid-keep-p (headline timestamp)
  "Keep TIMESTAMP on the calendar unless it is an untimed repeat."
  (and (org-timegrid-org-default-filter headline timestamp)
       (or (org-element-property :hour-start timestamp)
           (not (org-element-property :repeater-type timestamp)))))

(defvar ads/org-timegrid-work-hours '(9 . 19.5)
  "First and last hour of the working day, tinted on the calendar.
Fractional, so 19.5 is half past seven.")

(defun ads/org-timegrid-shade-canvas
    (svg width _height column-width palette _font-family
         start-minute end-minute scale days &optional week-start &rest _)
  "Tint weekends, and the hours outside `ads/org-timegrid-work-hours'.
Positional in the renderer's arguments, so it draws inside
`with-demoted-errors': an upstream signature change should cost the tint
rather than the calendar."
  (with-demoted-errors "org-timegrid shading: %S"
    (let* ((foreground (plist-get palette :foreground))
           (background (plist-get palette :background))
           (left (org-timegrid--label-width))
           (top (org-timegrid--grid-top-inset))
           (bottom (+ top (* (- end-minute start-minute) scale))))
      (when week-start
        (dotimes (day days)
          (when (memq (calendar-day-of-week
                       (calendar-gregorian-from-absolute (+ week-start day)))
                      '(0 6))
            (svg-rectangle svg (+ left (* day column-width)) top
                           column-width (- bottom top)
                           :fill (org-timegrid--blend foreground background 0.10)
                           :fill-opacity 0.55))))
      (pcase-let ((`(,opens . ,closes) ads/org-timegrid-work-hours))
        (dolist (range (list (cons start-minute (min (round (* 60 opens)) end-minute))
                             (cons (max (round (* 60 closes)) start-minute) end-minute)))
          (when (< (car range) (cdr range))
            (svg-rectangle svg left (+ top (* (- (car range) start-minute) scale))
                           (- width left)
                           (* (- (cdr range) (car range)) scale)
                           :fill (org-timegrid--blend foreground background 0.07)
                           :fill-opacity 0.6)))))))

(defun ads/org-timegrid-drop-stateless (&rest _)
  "Kill a calendar buffer that a failed open left without state.
`org-timegrid-open' creates the buffer and turns the mode on before it
loads the week, so a signal in the load leaves `org-timegrid--state' nil
-- and from then on every open takes the revisit branch and dies reading
the week start off it, which is a wedge that outlives whatever caused it.
The minute timer reads the same nil state, so it errors every tick too."
  (when-let* ((buffer (get-buffer org-timegrid-buffer-name))
              ((provided-mode-derived-p
                (buffer-local-value 'major-mode buffer) 'org-timegrid-mode))
              ((null (buffer-local-value 'org-timegrid--state buffer))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun ads/org-timegrid-agenda-guard (fn &rest args)
  "Draw the day strip inside `with-demoted-errors'.
It runs first on `org-agenda-finalize-hook', so a signal here takes
`org-modern-agenda' and the appt rebuild down with it and the agenda
renders as plain text.  The strip is worth less than the rest of it."
  (with-demoted-errors "org-timegrid strip: %S" (apply fn args)))

(use-package org-timegrid
  :vc (:url "https://github.com/Gleek/org-timegrid"
       :rev :newest)
  :defer t
  :init
  (setq org-timegrid-start-hour 8
        org-timegrid-end-hour 22
        org-timegrid-all-day-max-lanes 2
        org-timegrid-highlight-current-day t
        org-timegrid-org-capture-file ads/inbox-file
        org-timegrid-org-auto-save t
        org-timegrid-org-filter-function #'ads/org-timegrid-keep-p
        org-timegrid-org-color-function #'ads/org-timegrid-color
        ;; First mapped tag wins, and meetings are the only split I read a
        ;; calendar for.
        org-timegrid-org-tag-color-alist '(("meeting" . yellow)))
  (evil-set-initial-state 'org-timegrid-mode 'emacs)
  (advice-add 'org-timegrid-week :before #'ads/org-agenda-files-update)
  (advice-add 'org-timegrid-open :before #'ads/org-timegrid-drop-stateless)
  (advice-add 'org-timegrid-org--event :around #'ads/org-timegrid-note-category)
  (advice-add 'org-timegrid--draw-block :around #'ads/org-timegrid-plain-title)
  (advice-add 'org-timegrid--draw-all-day-block :around #'ads/org-timegrid-plain-title)
  (advice-add 'org-timegrid--wrap-title :filter-return #'ads/org-timegrid-underline-wrapped)
  (advice-add 'org-timegrid--draw-text :filter-args #'ads/org-timegrid-underline-links)
  (advice-add 'org-timegrid--draw-timed-background :after #'ads/org-timegrid-shade-canvas))

(use-package org-timegrid-agenda
  :ensure nil
  :after org-agenda
  :demand t
  :init
  (setq org-timegrid-agenda-insert-after nil)
  (advice-add 'org-timegrid-agenda-insert :around #'ads/org-timegrid-agenda-guard)
  :config
  (org-timegrid-agenda-mode 1))

(defun ads/org-timegrid--absolute-minute (time)
  "Return TIME as the absolute minute org-timegrid counts blocks in."
  (let ((decoded (decode-time time)))
    (+ (* 1440 (calendar-absolute-from-gregorian
                (list (decoded-time-month decoded)
                      (decoded-time-day decoded)
                      (decoded-time-year decoded))))
       (* 60 (decoded-time-hour decoded))
       (decoded-time-minute decoded))))

(defun ads/org-timegrid-schedule-task ()
  "Give an existing task a time and show it on the calendar.
Reads the same completion the grid's own RET does, so a title matching
nothing is captured as a new entry, and the range is written by the
package rather than by me."
  (interactive)
  (require 'org-timegrid-org)
  (ads/org-agenda-files-update)
  (let* ((entry (org-timegrid-org-read-entry))
         (title (car entry))
         (marker (cdr entry))
         (start (ads/org-timegrid--absolute-minute
                 (org-read-date t t nil "Starts")))
         (end (+ start (read-number
                        "Minutes: " org-timegrid-default-duration-minutes))))
    (if marker
        (org-timegrid-org--add-range marker start end)
      (org-timegrid-org--create-event title start end))
    ;; Nothing tells an open calendar that a file moved under it.
    (when-let* ((buffer (get-buffer org-timegrid-buffer-name)))
      (with-current-buffer buffer (setq-local org-timegrid--stale t)))
    (org-timegrid-week (floor start 1440))))

(defun ads/org-timegrid-visit ()
  "Open the Org heading behind the block at the cursor."
  (interactive)
  (let* ((block (org-timegrid--block-at-cursor))
         (event (and block (org-timegrid-block-event block)))
         (visitor (and org-timegrid--backend
                       (org-timegrid-backend-visit-function org-timegrid--backend))))
    (unless (and event (functionp visitor))
      (user-error "Nothing under the cursor; o makes something"))
    (funcall visitor event)))

(defun ads/org-timegrid-cursor-hour-forward (&optional count)
  "Move the cursor COUNT hours later."
  (interactive "p")
  (org-timegrid-cursor-forward
   (* (or count 1) (max 1 (/ 60 org-timegrid-cursor-step-minutes)))))

(defun ads/org-timegrid-cursor-hour-backward (&optional count)
  "Move the cursor COUNT hours earlier."
  (interactive "p")
  (ads/org-timegrid-cursor-hour-forward (- (or count 1))))

(defun ads/org-timegrid-move-hour-later (&optional count)
  "Move the selected block COUNT hours later."
  (interactive "p")
  (org-timegrid-move-later
   (* (or count 1) (max 1 (/ 60 org-timegrid-slot-minutes)))))

(defun ads/org-timegrid-move-hour-earlier (&optional count)
  "Move the selected block COUNT hours earlier."
  (interactive "p")
  (ads/org-timegrid-move-hour-later (- (or count 1))))

(defun ads/org-timegrid--move-weeks (count)
  "Shift the selected block COUNT days by writing the range itself.
`org-timegrid--transform-block-range' clamps a timed block to the visible
week, so the built-in move stops at the week's edge instead of carrying
the block over it."
  (org-timegrid--commit-keyboard-edit)
  (let* ((block (org-timegrid--selected-block))
         (event (org-timegrid-block-event block))
         (updater (org-timegrid-backend-update-function org-timegrid--backend)))
    (unless (and event (functionp updater))
      (user-error "This backend cannot move calendar entries"))
    (let ((start (+ (org-timegrid-event-start event) (* count 1440)))
          (end (+ (org-timegrid-event-end event) (* count 1440))))
      (org-timegrid--call-update
       updater event start end nil
       (if (org-timegrid-event-all-day event) 'all-day 'timed))
      (setq-local org-timegrid--stale t)
      (org-timegrid-week (floor start 1440)))))

(defun ads/org-timegrid-move-next-day (&optional count)
  "Move the selected block COUNT days later, over the week's edge if need be."
  (interactive "p")
  (let* ((count (or count 1))
         (target (+ (org-timegrid-block-day (org-timegrid--selected-block)) count)))
    (if (<= 0 target (org-timegrid--last-day-index))
        (org-timegrid-move-next-day count)
      (ads/org-timegrid--move-weeks count))))

(defun ads/org-timegrid-move-previous-day (&optional count)
  "Move the selected block COUNT days earlier, over the week's edge if need be."
  (interactive "p")
  (ads/org-timegrid-move-next-day (- (or count 1))))

(defun ads/org-timegrid-widen-hours (&optional count)
  "Draw COUNT more hours at each end of the day."
  (interactive "p")
  (setq-local org-timegrid-start-hour (max 0 (- org-timegrid-start-hour (or count 1)))
              org-timegrid-end-hour (min 24 (+ org-timegrid-end-hour (or count 1))))
  (org-timegrid-refresh))

(defun ads/org-timegrid-narrow-hours (&optional count)
  "Draw COUNT fewer hours at each end of the day."
  (interactive "p")
  (let ((start (+ org-timegrid-start-hour (or count 1)))
        (end (- org-timegrid-end-hour (or count 1))))
    (when (< (- end start) 2)
      (user-error "The day is as narrow as it goes"))
    (setq-local org-timegrid-start-hour start
                org-timegrid-end-hour end))
  (org-timegrid-refresh))

(defvar ads/org-timegrid-hour-ranges '((8 . 22) (9 . 20) (0 . 24))
  "Canvas hours cycled by `ads/org-timegrid-cycle-hours'.
Whole hours: the grid draws one labelled line per hour, so the working
day rounds out past `ads/org-timegrid-work-hours'.")

(defun ads/org-timegrid-cycle-hours ()
  "Show waking hours, then working hours, then the whole day."
  (interactive)
  (let* ((current (cons org-timegrid-start-hour org-timegrid-end-hour))
         (next (or (cadr (member current ads/org-timegrid-hour-ranges))
                   (car ads/org-timegrid-hour-ranges))))
    (setq-local org-timegrid-start-hour (car next)
                org-timegrid-end-hour (cdr next))
    (org-timegrid-refresh)
    (message "%02d:00 to %02d:00" (car next) (cdr next))))

(defvar ads/org-timegrid-all-day-lanes 2
  "Lanes the all-day rail shows when it is neither open nor shut.")

(defun ads/org-timegrid--all-day-fit-lanes ()
  "Return the lanes that would show every all-day event in the week.
Capped to half the window: the rail is the header line, one SVG that does
not scroll, so a week of thirty would push the grid off the bottom."
  (let ((needed (1+ (seq-max (cons -1 (mapcar #'org-timegrid-block-rail-lane
                                              (org-timegrid--all-day-layout))))))
        (room (max 1 (floor (- (/ (window-body-height nil t) 2)
                               (org-timegrid--rail-top))
                            (org-timegrid--all-day-lane-height)))))
    (min needed room)))

(defun ads/org-timegrid-cycle-all-day ()
  "Open the all-day rail as far as it fits, shut it, then restore it."
  (interactive)
  (let* ((fit (ads/org-timegrid--all-day-fit-lanes))
         (next (cond ((= org-timegrid-all-day-max-lanes 0)
                      ads/org-timegrid-all-day-lanes)
                     ((< org-timegrid-all-day-max-lanes fit) fit)
                     (t 0))))
    (setq-local org-timegrid-all-day-max-lanes next)
    (org-timegrid-refresh)
    (if (= next 0)
        (message "All-day rail shut")
      (message "All-day rail: %d lanes" next))))

(defun ads/org-timegrid-toggle-weekends ()
  "Switch the calendar between the whole week and Monday to Friday.
Both settings are buffer-local: `calendar-week-start-day' is Sunday
everywhere else and stays that way."
  (interactive)
  ;; Anchor on the middle of what is on screen.  `org-timegrid--range-start'
  ;; ends a range shorter than a week *on* the date it is handed, so passing
  ;; the current first day walked the calendar a week backwards per press; the
  ;; midpoint is inside the week both before and after the toggle.
  (let ((anchor (+ (org-timegrid--calendar-state-week-start org-timegrid--state)
                   (/ org-timegrid-days 2))))
    (if (= org-timegrid-days 7)
        (setq-local org-timegrid-days 5
                    calendar-week-start-day 1)
      (setq-local org-timegrid-days 7
                  calendar-week-start-day 0))
    (setq-local org-timegrid--stale t)
    (org-timegrid-week anchor)))

(defun ads/org-timegrid--on-screen-p (block)
  "Return non-nil when BLOCK is actually drawn right now.
A timed block has to overlap the hours on the canvas, and an all-day one
has to be inside the lanes the rail is currently showing."
  (if (org-timegrid-block-all-day-p block)
      (< (or (org-timegrid-block-rail-lane block) 0)
         org-timegrid-all-day-max-lanes)
    (and (< (org-timegrid-block-start block) (* 60 org-timegrid-end-hour))
         (> (org-timegrid-block-end block) (* 60 org-timegrid-start-hour)))))

(defun ads/org-timegrid-goto-block ()
  "Jump the cursor to one of the blocks on screen, chosen by name.
Offers only what is drawn and changes nothing about the view: widen the
canvas with = or 0, or open the rail with z, to reach the rest."
  (interactive)
  (let* ((week-start (org-timegrid--calendar-state-week-start org-timegrid--state))
         (candidates
          (mapcar (lambda (block)
                    (cons (format "%s %s  %s"
                                  (calendar-day-name
                                   (calendar-gregorian-from-absolute
                                    (+ week-start (org-timegrid-block-day block)))
                                   t)
                                  (if (org-timegrid-block-all-day-p block)
                                      "all day"
                                    (org-timegrid--format-minute
                                     (org-timegrid-block-start block)))
                                  (concat
                                   (when-let* ((icon (ads/org-timegrid--block-icon
                                                      block)))
                                     (concat icon " "))
                                   (org-link-display-format
                                    (or (org-timegrid-block-title block) ""))))
                          block))
                  (seq-filter #'ads/org-timegrid--on-screen-p
                              (org-timegrid--calendar-state-blocks
                               org-timegrid--state))))
         (_ (unless candidates (user-error "Nothing on screen to jump to")))
         (block (cdr (assoc (completing-read "Block: " candidates nil t)
                            candidates)))
         (day (org-timegrid-block-day block))
         (minute (org-timegrid-block-start block)))
    (if (org-timegrid-block-all-day-p block)
        (org-timegrid--set-all-day-cursor
         day (or (org-timegrid-block-rail-lane block) 0))
      (org-timegrid--set-cursor day minute 0))
    (setf (org-timegrid--calendar-state-cursor-visible org-timegrid--state) t)
    (org-timegrid--render-dynamic t)
    (org-timegrid--scroll-cursor-into-view)))

(with-eval-after-load 'org-timegrid
  (general-define-key
   :keymaps 'org-timegrid-mode-map
   "h" 'org-timegrid-cursor-backward-day
   "l" 'org-timegrid-cursor-forward-day
   "j" 'org-timegrid-cursor-forward
   "k" 'org-timegrid-cursor-backward
   "C-j" 'ads/org-timegrid-cursor-hour-forward
   "C-k" 'ads/org-timegrid-cursor-hour-backward
   "G" 'org-timegrid-goto-date          ; j was this
   "N" 'org-timegrid-previous-block     ; frees p for a paste
   "/" 'ads/org-timegrid-goto-block
   "v" 'org-timegrid-set-mark-command
   "<escape>" 'org-timegrid-dismiss
   "H" 'ads/org-timegrid-move-previous-day
   "L" 'ads/org-timegrid-move-next-day
   "J" 'org-timegrid-move-later
   "K" 'org-timegrid-move-earlier
   "C-S-j" 'ads/org-timegrid-move-hour-later
   "C-S-k" 'ads/org-timegrid-move-hour-earlier
   "<" 'org-timegrid-grow-start         ; start earlier
   ">" 'org-timegrid-shrink-start       ; start later
   "[" 'org-timegrid-shrink-end         ; end earlier
   "]" 'org-timegrid-grow-end           ; end later
   "C-c C-o" 'ads/org-timegrid-open-link
   "y" 'org-timegrid-copy-selected
   "p" 'org-timegrid-yank
   "o" 'org-timegrid-create-at-cursor
   "a" 'ads/org-timegrid-schedule-task
   "RET" 'ads/org-timegrid-visit
   "=" 'ads/org-timegrid-widen-hours
   "-" 'ads/org-timegrid-narrow-hours
   "0" 'ads/org-timegrid-cycle-hours
   "w" 'ads/org-timegrid-toggle-weekends
   "z" 'ads/org-timegrid-cycle-all-day)
  ;; Emacs state reaches `ads/keyboard-quit-dwim' before the mode map, and it
  ;; answers C-g here by leaving emacs state, which unbinds everything above.
  (general-define-key
   :states 'emacs
   :keymaps 'org-timegrid-mode-map
   "C-g" 'org-timegrid-dismiss))

(ads/leader-keys
  "og" '(org-timegrid-week :wk "timegrid week")
  "oG" '(ads/org-timegrid-schedule-task :wk "timegrid schedule task"))

;;; timegrid.el ends here
