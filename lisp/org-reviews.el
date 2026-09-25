;;; org-reviews.el --- Morning, weekly and monthly review templates  -*- lexical-binding: t; -*-
;;; Commentary:
;; good morning review, org weekly review, org monthly review
;;; Code:

(defun good-morning ()
  "Good morning function"
  (interactive)
  (org-roam-dailies-find-today)
   ; don't keep this here for long but for some reason calling daily doesnt make a node
  (org-id-get-create)
  (narrow-to-region (point-max) (point-max))
  (insert
"* Good Morning
** What's on your mind right now?

** What are you thinking about for work today?
"
   )
  (goto-char (point-min))
  ;; Clocking in starts the Toggl timer; the daily's date-shaped title means
  ;; the project comes from `toggl-org-daily-project' rather than the note.
  (org-clock-in)
  (delete-other-windows))

(defun ads/weekly ()
  "start weekly review process"
  (interactive)
  (toggl-start "Weekly" "Admin")
  (browse-url "https://app.ynab.com")
  (org-roam-dailies-find-today)
  (org-roam-tag-add '("weekly"))
  (goto-char (point-max))
  (insert
   (format
"* Weekly Review

- [ ] [[https:calendar.google.com/calendar/u/0/r/week][Schedule Week]]
- [ ] [[https:app.ynab.com][YNAB]]
- [ ] Clear
  - [ ] [[id:a2a7d9b1-18b7-46f1-885c-68b9b87b29d5][Inbox]]
  - [ ] [[id:20128D1C-9D03-4AC6-94E2-4C479F7BAADA][Reading List]]
  - [ ] Downloads
  - [ ] [[https:gmail.com][gmail]]
- [ ] Clean Apartment
- [ ] Week agenda

#+begin_src emacs-lisp
(org-agenda-list)
(org-agenda-week-view)
#+end_src

** Project Statuses

%s

** Goal Progress

%s
"
    (ads/roam-active-projects)
    (ads/monthly-goals)))
 (package-upgrade-all))

(defun ads/monthly-file ()
  "Return path of this month's review"
  (concat org-directory (format-time-string "%Y_%m_monthly_review.org")))

(defun ads/monthly-new ()
  "New monthly review"
  (interactive)
  (find-file (ads/monthly-file))
  (toggl-start "Monthly" "Admin")
  (insert
   (format
"#+title:%s Monthly
#+filetags: :monthly:

* Theme
#Beginning

* Goals
#Beginning

* Books Read
#End
(org-roam-ql-search
'(and (tags \"book\") (properties COMPLETED \"%s\")))

* Time Tracked
#End

* Thoughts had
#End

* Projects

** Active
#Beginning

** Completed
#End

* Reflection
#End

"
    (format-time-string "%B %Y")
    (format-time-string "%Y-%m")))
  (goto-char (point-min))
  (org-id-get-create))

(defun ads/monthly-node ()
  "Roam node for this month's review, nil before I've made one"
  (org-roam-node-from-title-or-alias (format-time-string "%B %Y Monthly")))

(defun ads/monthly-current ()
  "Go to current monthly review"
  (interactive)
  (org-roam-node-visit (ads/monthly-node)))

(defun ads/monthly-goals ()
  "Return the body of the Goals heading in this month's review"
  (let ((node (ads/monthly-node)))
    (if (not node)
        ""
      (with-temp-buffer
        (insert-file-contents (org-roam-node-file node))
        (goto-char (point-min))
        (if (not (re-search-forward "^\\* Goals$" nil t))
            ""
          (string-trim
           (replace-regexp-in-string
            "^#Beginning$" ""
            (buffer-substring-no-properties
             (line-beginning-position 2)
             (if (re-search-forward "^\\* " nil t)
                 (match-beginning 0)
               (point-max))))))))))

;;; org-reviews.el ends here
