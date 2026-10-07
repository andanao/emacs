;;; appearance.el --- org-modern, org-appear, org-tidy  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-appear, org-modern, org-indent, org-tidy
;;; Code:

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  (org-appear-autokeywords nil)
  :hook
  ;; (org-mode . org-appear-mode)
  (evil-insert-state-exit . (lambda ()
	      (setq org-appear-delay 2)))
  (evil-insert-state-entry-hook .
	    (lambda ()
	      (setq org-appear-delay .3)))
  :config
  (if (eq system-type 'windows-nt)
      (print "org-appear skipped on windows")
      (add-hook 'org-mode-hook 'org-appear-mode))


  )

(use-package org-modern
  :after (org)
  :custom
  (org-modern-fold-stars
   '(("▸ " . "▾ ")
     ;; ("  ▸ " . "  ▾ ")
     ;; ("    ▸ " . "    ▾ ")
     ;; ("      ▸ " . "      ▾ ")
     ;; ("        ▸ " . "        ▾ ")
     ;; ("          ▸ " . "          ▾ ")
     ;; ("            ▸ " . "            ▾ ")
     ;; ("              ▸ " . "              ▾ ")
     ))
  (org-modern-checkbox
      '((?X . " ")
	(?- . " ")
	(?\s . " ")))
  (org-modern-table-vertical 2)
  (org-modern-table-horizontal 0.1)
  (org-modern-block-name nil)
  (org-modern-block-mode nil)
  (org-modern-list '((?- . "•")  (?+ . "◦")))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (defun ads/org-modern-table-thin-hline (fn &rest args)
    "Call FN with ARGS, then shrink the indent prefix on table separator rows.
Without this the full-height `line-prefix' keeps an indented hline a full line
tall, even though org-modern has shrunk the rest of the row."
    (let ((beg (match-beginning 0))
	  (end (match-end 0))
	  (hline (eq ?- (char-after (1+ (match-beginning 1))))))
      (apply fn args)
      (let ((prefix (and hline
			 (numberp org-modern-table-horizontal)
			 (get-text-property beg 'line-prefix))))
	(when (and (stringp prefix) (not (string-empty-p prefix)))
	  (put-text-property
	   beg (min (1+ end) (point-max)) 'line-prefix
	   (propertize " "
		       'display `(space :width (,(string-pixel-width prefix)))
		       'face `(:height ,org-modern-table-horizontal)))))))
  (advice-add 'org-modern--table :around #'ads/org-modern-table-thin-hline))

(use-package org-indent
  :ensure nil
  :custom
  (org-startup-indented t)
  (org-indent-indentation-per-level 2))

(use-package org-tidy
  :ensure t
  :custom
  (org-tidy-properties-style 'invisible)
  (org-tidy-protect-overlay nil)
  :hook  (org-mode . org-tidy-mode)
  )

;; `org-tidy-toggle' trusts a flag that goes stale when `org-tidy-mode'
;; re-tidies on save, so this one asks the buffer instead.
(defun ads/org-tidy-toggle ()
  "Untidy the buffer if any drawer is hidden, otherwise tidy it."
  (interactive)
  (if org-tidy-overlays
      (org-tidy-untidy-buffer)
    (org-tidy-buffer)))
(ads/leader-keys
  :keymaps 'org-mode-map
  "ot" 'ads/org-tidy-toggle
  "oT" 'org-tidy-toggle
  "o C-t" 'org-tidy-mode)

;;; appearance.el ends here
