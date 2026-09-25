;;; org-meetings.el --- Meetings and their agenda notifications  -*- lexical-binding: t; -*-
;;; Commentary:
;; org meetings, agenda notifications
;;; Code:

(require 'org-agenda)
(require 'org-element)

(defun ads/collect-meeting-todos ()
  "Collect all TODO entries tagged with :meeting: from org-agenda-files.
Returns a list of plists with :heading, :file, :point, and :scheduled properties."
  (let ((entries '()))
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))
              (tags (org-get-tags))
              (todo-state (org-get-todo-state))
              (scheduled (org-get-scheduled-time (point)))
              (file (buffer-file-name))
              (pos (point)))
         (when (and todo-state
                    (member "meeting" tags)
                    (not (member todo-state org-done-keywords)))
           (push (list :heading heading
                       :file file
                       :point pos
                       :scheduled scheduled)
                 entries))))
     nil
     'agenda)
    (nreverse entries)))

(defun ads/get-unique-meetings (entries)
  "Extract unique meeting headings from ENTRIES.
Returns a sorted list of unique heading strings."
  (let ((headings (mapcar (lambda (entry) (plist-get entry :heading)) entries)))
    (sort (delete-dups headings) #'string<)))

(defun ads/jump-to-first-upcoming (meeting-heading entries)
  "Jump to the first upcoming instance of MEETING-HEADING in ENTRIES.
If scheduled times exist, picks the earliest one from today onward.
Otherwise, picks the first occurrence."
  (let* ((matching-entries (seq-filter
                            (lambda (entry)
                              (string= (plist-get entry :heading) meeting-heading))
                            entries))
         (today-start (encode-time 0 0 0
                                   (nth 3 (decode-time))
                                   (nth 4 (decode-time))
                                   (nth 5 (decode-time))))
         (future-entries (seq-filter
                          (lambda (entry)
                            (let ((sched (plist-get entry :scheduled)))
                              (and sched (not (time-less-p sched today-start)))))
                          matching-entries))
         (sorted-entries (if future-entries
                             (sort future-entries
                                   (lambda (a b)
                                     (time-less-p (plist-get a :scheduled)
                                                  (plist-get b :scheduled))))
                           matching-entries))
         (target (car sorted-entries)))
    (if target
        (progn
          (find-file (plist-get target :file))
          (goto-char (plist-get target :point))
          (org-show-entry)
          (org-reveal)
          (recenter)
          (message "Jumped to: %s" meeting-heading))
      (message "No matching meeting found"))))


(defun ads/jump-to-meeting-todo ()
  "Select a meeting type and jump to its first upcoming TODO.
Collects all TODO items tagged with :meeting:, groups them by heading text,
prompts for selection, then jumps to the first upcoming instance."
  (interactive)
  (let* ((meeting-entries (ads/collect-meeting-todos))
         (unique-meetings (ads/get-unique-meetings meeting-entries))
         (selected-meeting (completing-read "Select meeting: " unique-meetings nil t)))
    (if selected-meeting
        (ads/jump-to-first-upcoming selected-meeting meeting-entries)
      (message "No meeting selected")))
  (org-narrow-to-subtree))

(ads/leader-keys "om" 'ads/jump-to-meeting-todo)

(require 'appt)

(defvar ads/notify-appt--entries (make-hash-table :test 'equal)
  "Appt message -> plist of :file, :point, :category and :meeting behind it.")

(defun ads/notify-appt--entry-text (x)
  "The string `org-agenda-to-appt' will hand to appt for agenda entry X."
  (org-trim (replace-regexp-in-string
             org-link-bracket-re "\\2"
             (or (get-text-property 1 'txt x) ""))))

(defun ads/notify-appt--record (x)
  "Remember where agenda entry X lives, and always take it.
`org-agenda-to-appt' has already dropped anything without a time on it or
already done, which is the whole of the filtering I want."
  (when-let* ((m (get-text-property 1 'org-marker x))
              (file (buffer-file-name (marker-buffer m))))
    (puthash (ads/notify-appt--entry-text x)
             (list :file file
                   :point (marker-position m)
                   :category (get-text-property (1- (length x)) 'org-category x)
                   :meeting (and (member "meeting" (get-text-property 1 'tags x)) t))
             ads/notify-appt--entries))
  t)

(defun ads/notify-appt-refresh (&rest _)
  "Rebuild today's appt list from the timed agenda entries."
  (interactive)
  (clrhash ads/notify-appt--entries)
  (org-agenda-to-appt t #'ads/notify-appt--record))

(defun ads/notify-appt--todo-regexp ()
  "A regexp matching any of my TODO keywords."
  (regexp-opt
   (delete-dups
    (delq nil
          (mapcar (lambda (kw)
                    (unless (equal kw "|")
                      (replace-regexp-in-string "(.*)\\'" "" kw)))
                  (apply #'append
                         (mapcar (lambda (e) (if (symbolp (car e)) (cdr e) e))
                                 org-todo-keywords)))))
   'words))

(defun ads/notify-appt--title (msg)
  "MSG minus its TODO keyword and trailing tags."
  (let ((s (string-trim msg)))
    (when (string-match (concat "\\`" (ads/notify-appt--todo-regexp) "[ \t]+") s)
      (setq s (substring s (match-end 0))))
    (string-trim (replace-regexp-in-string ":[[:alnum:]_@#%:]+:\\'" "" s))))

(defun ads/notify-appt--icon (category)
  "The glyph an agenda CATEGORY starts with, or a calendar if it has none."
  (if (and (stringp category)
           (> (length category) 0)
           (not (string-match-p "[[:alnum:][:space:]]" (substring category 0 1))))
      (substring category 0 1)
    "nf-md-calendar"))

(defun ads/notify-appt--visit (place)
  "Open the org entry PLACE came from."
  (find-file (plist-get place :file))
  (goto-char (plist-get place :point))
  (org-back-to-heading t)
  (org-fold-show-entry)
  (org-fold-show-children))

(defun ads/notify-appt--style (meeting)
  "Colours for an agenda popup.  MEETING earns the filled background.
Resolved per notification, so a theme toggle is picked up."
  (append (list :border (ads/modus-color 'blue)
                :text (ads/modus-color 'fg-main))
          (if meeting
              (list :background (ads/modus-color 'bg-blue-nuanced)
                    :icon-color (ads/modus-color 'blue))
            (list :icon-color (ads/modus-color 'fg-main)))))

(defun ads/notify-appt (minutes _time msg)
  "Show appt reminders through `ads/notify--broadcast'.
Either MINUTES or MSG may be a list when several are due at once."
  (seq-mapn
   (lambda (m s)
     (let ((place (gethash s ads/notify-appt--entries)))
       (apply #'ads/notify--broadcast
              :title (ads/notify-appt--title s)
              :message (if (equal m "0") "Starting now" (format "In %s min" m))
              :icon (ads/notify-appt--icon (plist-get place :category))
              :action (and place (lambda () (ads/notify-appt--visit place)))
              (ads/notify-appt--style (plist-get place :meeting)))))
   (if (listp minutes) minutes (list minutes))
   (if (listp msg) msg (list msg))))

(setq appt-message-warning-time 3
      appt-display-interval 4
      appt-audible nil
      appt-display-mode-line nil
      appt-display-format 'window
      appt-disp-window-function #'ads/notify-appt
      appt-delete-window-function #'ignore)

(appt-activate 1)
(add-hook 'org-agenda-finalize-hook #'ads/notify-appt-refresh)
(run-at-time "00:05" 86400 #'ads/notify-appt-refresh)
(ads/notify-appt-refresh)

;;; org-meetings.el ends here
