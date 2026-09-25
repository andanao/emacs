;;; capture.el --- org-capture templates  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-capture
;;; Code:

(setq ads/inbox-file (concat org-directory "inbox.org")
      org-default-notes-file ads/inbox-file)

(defun ads/inbox ()
  "Open ads/inbox-file"
  (interactive)
  (find-file ads/inbox-file))

(defun ads/quick-note ()
  "take a note using the capture-note template"
  (interactive)
  (org-capture nil "n"))

(ads/leader-keys
  "oc" 'org-capture
  "oi" 'ads/inbox
  "C-a" 'ads/quick-note)

(setq org-capture-templates
      '(
("i" "inbox" plain
(file ads/inbox-file)
"* %^{}
:CREATED: %U
"
:immediate-finish t)
("t" "todo - inbox" plain
(file ads/inbox-file)
"* TODO %^{TASK}
SCHEDULED: %t

"
:immediate-finish t)
("r" "todo - inbox notes" plain
(file ads/inbox-file)
"* TODO %^{TASK}
SCHEDULED: %t

%?
")
("c" "todo - inline" plain
(here)
"
* TODO %^{TASK}
SCHEDULED: %t


"
:empty-lines 1
:immediate-finish t)
("n" "note" plain
(file ads/inbox-file)
"* %^{HEADING}
:CREATED: %U

%?
")
("b" "book" plain
 (file (lambda () (concat org-directory
                          (format-time-string "%Y%m%d%H%M%S-" nil t)
                          (string-replace " " "-" (read-string "filename "))
                          ".org")))
":PROPERTIES:
:ID: %(org-id-new)
:AUTHOR: %^{Author}
:MEDIUM: %^{MEDIUM ||audio|paper|electronic}
:COMPLETED: %^u
:END:
#+title: %^{Title}
#+filetags: :book:

%?"
:jump-to-captured t)
("a" "anki basic" plain
(file ads/anki-file)
"* %^{TITLE}
:PROPERTIES:
:ANKI_NOTE_TYPE: Basic
:CREATED: %U
:END:
** Front
%?
** Back
,%x"
:jump-to-captured t)
("z" "anki cloze" plain
(file ads/anki-file)
"* %^{TITLE}
:PROPERTIES:
:ANKI_NOTE_TYPE: Cloze
:CREATED: %U
:END:
%?
"
:jump-to-captured t)
("x" "acronym" plain
 (file (lambda ()
         (concat
          org-directory
          (format-time-string "%Y%m%d%H%M%S--acronym.org" nil t))))
":PROPERTIES:
:ID: %(org-id-new)
:CREATED: %U
:ROAM_ALIASES: %^{Short}
:END:
#+title: %\\1: %^{Definition}
#+filetags: :acronym:

")
("q" "quote" plain
 (file (lambda ()
         (concat
          org-directory
          (format-time-string "%Y%m%d%H%M%S--quote.org" nil t))))
":PROPERTIES:
:ID: %(org-id-new)
:CREATED: %U
:SOURCE: %^{Source}
:END:
#+title: %^{Description}
#+filetags: :quote:

#+begin_quote
%?
#+end_quote

")
	))

;;; capture.el ends here
