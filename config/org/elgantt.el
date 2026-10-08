;;; elgantt.el --- calendar-grid gantt view of org timestamps  -*- lexical-binding: t; -*-
;;; Commentary:
;; elgantt, a text gantt drawn from the timestamps on org headings;
;; it schedules nothing, so dates are set by hand,
;; its own keys in an Emacs-state buffer
;;; Code:

(use-package elgantt
  :vc (:url "https://github.com/legalnonsense/elgantt" :rev :newest)
  :commands (elgantt-open)
  :init
  ;; elgantt-mode binds plain letters (a, r, p) that normal state would shadow.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'elgantt-mode 'emacs)))

;;; elgantt.el ends here
