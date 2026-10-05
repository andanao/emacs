;;; agenda.el --- org-agenda and its task frame  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-agenda, Task Frame, rebuild on theme change
;;; Code:

(require 'org-agenda)
;; (evil-make-overriding-map org-agenda-mode-map)

(setq org-agenda-window-setup 'current-window
      org-agenda-span 'day
      org-agenda-block-separator ""
      org-agenda-restore-windows-after-quit t
      org-agenda-persistent-filter t
      org-agenda-scheduled-leaders '("   " "%2dd")
      org-agenda-skip-scheduled-if-done nil
      org-agenda-sticky t  ; use bury-buffer instead of agenda-quit
      )

(add-hook 'org-agenda-mode-hook
            (lambda () (setq-local mode-line-format nil)))

(setq org-agenda-prefix-format
      '((agenda . "  %-20 c%?-12t% s")
	(todo . "  %-20 c")
	(tags . "  %-20 c")
	(search . "  %-20 c")))

(setopt org-agenda-custom-commands
        '(("u" "Unscheduled TODOs"
           ((todo ""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                 (org-agenda-overriding-header "Unscheduled TODOs")))))
          ("t" "All tasks (no meetings)" ((tags-todo "-meeting")))))

(ads/leader-keys
  "oa" 'org-agenda-list ;; I use this more frequently
  "oA" 'org-agenda)

(general-define-key
 :states '(normal motion emacs)
 :keymaps 'org-agenda-mode-map
 "j" 'org-agenda-next-line
 "k" 'org-agenda-previous-line
 "M-j" 'org-agenda-drag-line-forward
 "M-k" 'org-agenda-drag-line-backward

 "h" 'org-agenda-earlier
 "l" 'org-agenda-later
 "H" 'org-agenda-do-date-earlier
 "L" 'org-agenda-do-date-later

 "S" 'org-agenda-schedule

 "m" 'org-agenda-bulk-toggle
 "M" 'org-agenda-bulk-unmark-all
 "R" 'org-agenda-bulk-mark-regexp
 "x" 'org-agenda-bulk-action

 "a" 'org-agenda-add-note
 "A" 'org-agenda-archive

 "u" 'org-agenda-undo
 ";" 'org-agenda-set-tags

 ;; go show
 "gr" 'org-agenda-redo
 "gR" 'org-agenda-redo-all
 "gc" 'org-agenda-goto-calendar
 "gt" 'org-agenda-show-tags
 "G" '(lambda () (interactive) (goto-line 3))
 "gg" '(lambda () (interactive)
         (forward-line 100)
         (forward-line -1))

 ;; delete
 "dd" 'org-agenda-kill
 "da" 'org-agenda-archive

 ;; filter
 "sc" 'org-agenda-filter-by-category
 "sr" 'org-agenda-filter-by-regexp
 "se" 'org-agenda-filter-by-effort
 "st" 'org-agenda-filter-by-tag
 "s^" 'org-agenda-filter-by-top-headline
 "ss" 'org-agenda-limit-interactively
 "sq" 'org-agenda-filter-remove-all

 "C-w C-h" 'evil-window-left
 "C-w C-j" 'evil-window-down
 "C-w C-k" 'evil-window-up
 "C-w C-l" 'evil-window-right)

(defun ads/task-frame ()
    "Open a dedicated frame showing today's agenda."
    (interactive)
    (let* ((frame (make-frame '((name . "Tasks")
                                (width . 60)
                                (height . 30))))
           (aerospace "/opt/homebrew/bin/aerospace"))
      (select-frame frame)
      (set-frame-parameter frame 'task-frame t)
      (org-agenda-list nil nil 'day)))

  (defun ads/org-agenda-quit-advice (&rest _)
    "Delete frame if it's a task frame."
    (when (frame-parameter nil 'task-frame)
      (delete-frame)))

(advice-add 'org-agenda-quit :after #'ads/org-agenda-quit-advice)

(defvar ads/org-agenda-rebuild-timer nil
  "Idle timer scheduled by `ads/org-agenda-rebuild'.")

(defun ads/org-agenda-rebuild ()
  "Rebuild every agenda buffer, off an idle timer so the theme paints first.
`org-agenda-redo-all' keeps each buffer's filters and line, and only recenters
when a human asked for it, so nothing moves but the colours."
  (when (timerp ads/org-agenda-rebuild-timer)
    (cancel-timer ads/org-agenda-rebuild-timer))
  (setq ads/org-agenda-rebuild-timer
        (run-with-idle-timer
         0.3 nil
         (lambda ()
           (with-demoted-errors "ads/org-agenda-rebuild: %S"
             (org-agenda-redo-all t))))))

(add-hook 'ef-themes-after-load-theme-hook #'ads/org-agenda-rebuild)

;;; agenda.el ends here
