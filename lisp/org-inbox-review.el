;;; org-inbox-review.el --- Inbox review workflow  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-inbox-review
;;; Code:

(defvar ads/inbox-review-intervals '(1 3 7 14 30)
  "Days to snooze items. Each snooze picks the next interval.")

(defun ads/inbox-review-get-interval (current-interval)
  "Get next snooze interval based on CURRENT-INTERVAL."
  (let ((intervals ads/inbox-review-intervals))
    (if (or (null current-interval) (zerop current-interval))
        (car intervals)
      (or (cadr (member current-interval intervals))
          (car (last intervals))))))

(defun ads/inbox-review-snooze ()
  "Snooze current item by scheduling it for a future date."
  (interactive)
  (let* ((last-interval (org-entry-get nil "SNOOZE_INTERVAL"))
         (current (if last-interval (string-to-number last-interval) 0))
         (next-interval (ads/inbox-review-get-interval current))
         (date (org-read-date nil nil (format "+%dd" next-interval))))
    (org-entry-put nil "SNOOZE_INTERVAL" (number-to-string next-interval))
    (org-schedule nil date)
    (message "Snoozed for %d days (next snooze: %d days)"
             next-interval
             (ads/inbox-review-get-interval next-interval))))

(defun ads/inbox-review-reset-interval ()
  "Reset snooze interval to start fresh."
  (interactive)
  (org-entry-delete nil "SNOOZE_INTERVAL")
  (message "Snooze interval reset"))

(defun ads/inbox-review-items ()
  "Get inbox items due for review.
Items are due if:
- Unscheduled
- Scheduled for today or past
- Scheduled for future but has no SNOOZE_INTERVAL (was manually scheduled)"
  (let ((today (format-time-string "%Y-%m-%d"))
        (results nil))
    (with-current-buffer (find-file-noselect ads/inbox-file)
      (org-map-entries
       (lambda ()
         (let* ((scheduled (org-get-scheduled-time (point)))
                (sched-str (when scheduled (format-time-string "%Y-%m-%d" scheduled)))
                (snooze-interval (org-entry-get nil "SNOOZE_INTERVAL"))
                (is-past-or-today (and sched-str (not (string> sched-str today))))
                (is-unscheduled (null scheduled))
                (is-unsnoozed-past (and sched-str
                                        (string< sched-str today)
                                        (null snooze-interval))))
           (when (or is-unscheduled
                     is-past-or-today
                     is-unsnoozed-past)
             (push (point-marker) results))))
       t
       nil))
    (nreverse results)))

(defun ads/inbox-review ()
  "Start reviewing inbox items one by one."
  (interactive)
  (let ((items (delq nil (ads/inbox-review-items))))
    (if (null items)
        (message "No inbox items to review!")
      (ads/inbox-review-next items))))

(defvar ads/inbox-review--remaining nil
  "Remaining items in current review session.")

(defun ads/inbox-review-next (items)
  "Review next item in ITEMS list."
  (if (null items)
      (progn
        (widen)
        (message "Inbox review complete!"))
    (setq ads/inbox-review--remaining (cdr items))
    (let ((marker (car items)))
      (switch-to-buffer (marker-buffer marker))
      (widen)
      (goto-char marker)
      (org-narrow-to-subtree)
      (message "[%d remaining] (s)nooze (S)reset (d)one (k)ill (r)efile (a)rchive (e)dit (n)ext (q)uit"
               (length items))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map "s" #'ads/inbox-review--snooze-and-next)
         (define-key map "S" #'ads/inbox-review--reset-snooze-and-next)
         (define-key map "d" #'ads/inbox-review--done-and-next)
         (define-key map "k" #'ads/inbox-review--kill-and-next)
         (define-key map "r" #'ads/inbox-review--refile-and-next)
         (define-key map "a" #'ads/inbox-review--archive-and-next)
         (define-key map "n" #'ads/inbox-review--next)
         (define-key map "q" #'ads/inbox-review--quit)
         map)
       t))))

(defun ads/inbox-review--snooze-and-next ()
  (interactive)
  (ads/inbox-review-snooze)
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--reset-snooze-and-next ()
  (interactive)
  (ads/inbox-review-reset-interval)
  (ads/inbox-review-snooze)
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--done-and-next ()
  (interactive)
  (widen)
  (let ((inbox-buf (current-buffer)))
    (ads/org-archive-done)
    (switch-to-buffer inbox-buf))
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--kill-and-next ()
  (interactive)
  (widen)
  (org-cut-subtree)
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--refile-and-next ()
  (interactive)
  (widen)
  (org-refile)
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--archive-and-next ()
  (interactive)
  (widen)
  (let ((inbox-buf (current-buffer)))
    (ads/org-archive)
    (switch-to-buffer inbox-buf))
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--next ()
  (interactive)
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review-continue ()
  "Continue review from where you left off after editing."
  (interactive)
  (ads/inbox-review-next ads/inbox-review--remaining))

(defun ads/inbox-review--quit ()
  (interactive)
  (widen)
  (message "Inbox review paused"))

(ads/leader-keys "oI" 'ads/inbox-review)

;;; org-inbox-review.el ends here
