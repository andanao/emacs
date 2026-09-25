;;; org-prettify-symbols.el --- Prettified org keywords  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-prettify-symbols
;;; Code:

(defun ads/org-prettify-symbols ()
  "Set pretty entitie for org mode"
  (setq prettify-symbols-alist
        '(("lambda" . "λ")
          ("CLOSED:" . "󰃯")
          ("SCHEDULED:" . "󰃭")
          ("DEADLINE:" . "󰨱")
          ("PROPERTY:" . "󱌣")
          ("STARTUP:" . "开")
          ("RESULTS:" . "󰘍")
          (":results" . "󰘍")

          (":ID:" . "")
          (":AUTHOR:" . "")
          (":CATEGORY:" . "󰕲")
          (":SOURCE:" . "")
          (":COMPLETED:" . "󱓴")
          (":RECCOMENDER:" . "")
          (":MEDIUM:" . "󱚋")
          (":CREATED:" . "󰃳")
          (":LOGBOOK:" . "")
          (":PROPERTIES:" . "󱌣")
          (":END:" . "")

          (":ARCHIVE_NODE:" . "󱝜")
          (":ARCHIVE_TIME:" . "󱝐")
          (":ARCHIVE_FILE:" . "󱈎")
          (":ARCHIVE_CATEGORY:" . "󱝖")
          (":ARCHIVE_ITAGS:" . "󱝤")

          (":ROAM_ALIASES:" . "󰑕")

          ;; (":ANKI" . "📚")
          ;; ("_DECK:" . "")
          ;; ("_NOTE_TYPE:" . "󱕷")
          ;; ("_NOTE_ID:" . "")
          ;; ("_NOTE_HASH:" . "󱅿")

          ("+filetags:" . "")
          ("#+Author:" . "")
          ("#+options:" . "󰘵")
          (":tangle" . "󱓡")
          (":noweb" . "󰪎")
          (":noweb-ref" . "")
          (":session" . "󱇝")
          (":mkdirp" . "")
          (":comments" . "󰆉")
          ("header-args:" . "󰉴")
          (":header-args" . "󰉴")

          ;; Blocks
          ("#+begin_quote" . "“")
          ("#+end_quote" . "”")
          ("#+begin_example" . "󰅴")
          ("#+end_example" . "")

          ;; Languages
          ("#+begin_src c" . "󰙱")
          ("#+begin_src cpp" . "󰙱")
          ("#+begin_src c++" . "󰙲")
          ("#+begin_src css" . "")
          ("#+begin_src d2" . "")
          ("#+begin_src dockerfile" . "")
          ("#+begin_src emacs-lisp" . "")
          ("#+begin_src html" . "")
          ("#+begin_src java" . "")
          ("#+begin_src javascript" . "")
          ("#+begin_src json" . "J")
          ("#+begin_src latex" . "")
          ("#+begin_src markdown" . "")
          ("#+begin_src python" . "")
          ("#+begin_src rust" . "󱘗")
          ("#+begin_src shell" . "")
          ("#+begin_src bash" . "")
          ("#+begin_src zsh" . "󰰶")
          ("#+begin_src nu" . "󰰒")
          ("#+begin_src sql" . "")
          ("#+begin_src toml" . "T")
          ("#+begin_src yaml" . "Y")

          ("#+end_src" . "»")))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook 'ads/org-prettify-symbols)

;;; org-prettify-symbols.el ends here
