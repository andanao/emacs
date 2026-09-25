;;; modeline.el --- doom-modeline and its indicators  -*- lexical-binding: t; -*-
;;; Commentary:
;; display-time-mode, display-battery, doom-modeline, telephone-line
;;; Code:

(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-default-load-average nil)

(defvar ads/battery-status nil
  "Cons of (PERCENTAGE . LINE-POWER) from the most recent battery poll.")

(defun ads/battery-watch (data)
  "Cache the two fields of DATA the mode line reads."
  (setq ads/battery-status
        (cons (string-to-number (or (cdr (assq ?p data)) "0"))
              (cdr (assq ?L data)))))

(add-hook 'battery-update-functions #'ads/battery-watch)

(use-package doom-modeline
  :demand t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-hud t)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-time-icon nil)
  (doom-modeline-modal-use-evil-tag t)
  :config
  (setq
   line-number-mode nil
   column-number-mode nil)
  ;; comint sets `mode-line-process' to ":%s", the process status, so a live
  ;; shell says ":run" forever and only stops saying it when the process dies.
  ;; Print it when it is anything other than that, and print it as a warning,
  ;; because by then it is one.
  (doom-modeline-def-segment ads/process
    (let ((text (string-trim (format-mode-line mode-line-process))))
      (unless (or (string-empty-p text) (member text '(":run" "run")))
        (concat (doom-modeline-spc)
                (propertize text 'face (doom-modeline-face 'doom-modeline-warning))))))
  ;; doom-modeline draws the whole line, so agent-shell's own busy indicator
  ;; never reaches it.  Green and thinking, red and waiting on me, or nothing at
  ;; all - a shell with nothing to say should not be saying it.  The faces are
  ;; agent-shell's own, the same two `ads/agent-shell-mode-line' uses, and that
  ;; poll's `force-mode-line-update' every second is what keeps this current.
  (doom-modeline-def-segment ads/agent
    (when (and (derived-mode-p 'agent-shell-mode) (fboundp 'agent-shell-status))
      (pcase (agent-shell-status)
        ('busy    (concat (doom-modeline-spc)
                          (propertize "" 'face (doom-modeline-face 'agent-shell-success))))
        ('blocked (concat (doom-modeline-spc)
                          (propertize "" 'face (doom-modeline-face 'agent-shell-error)))))))
  ;; Upstream's is a vertical icon plus a percentage that reads 100% nearly
  ;; always.  On AC the level is not news, so say only that; on battery the
  ;; glyph fills to match and the number comes back with it.
  (doom-modeline-def-segment ads/battery
    (when (and (bound-and-true-p display-battery-mode) ads/battery-status)
      (let ((percentage (car ads/battery-status))
            (charging (equal (cdr ads/battery-status) "AC")))
        ;; Plugged in and nearly full is the one state that never needs saying.
        (unless (and charging (>= percentage 90))
          (concat
           (doom-modeline-spc)
           (propertize
            (if charging
                ""
              (concat (cond ((>= percentage 88) "")
                            ((>= percentage 63) "")
                            ((>= percentage 38) "")
                            ((>= percentage 13) "")
                            (t ""))
                      ;; Doubled because the segment is printed by `format-mode-line'.
                      (format " %d%%%%" percentage)))
            'face (doom-modeline-face
                   (cond (charging 'ads/modeline-context)
                         ((< percentage 15) 'doom-modeline-urgent)
                         ((< percentage 30) 'doom-modeline-warning)
                         (t 'ads/modeline-context)))))))))
  ;; Guarded because doom-modeline loads long before toggl, and an unguarded
  ;; call here would be a void-function on every redraw until it does.
  (doom-modeline-def-segment ads/clock
    (when-let* (((fboundp 'ads/toggl-mode-line))
                (clock (ads/toggl-mode-line)))
      (concat (doom-modeline-spc)
              (propertize clock
                          'help-echo "mouse-1 toggl menu, mouse-3 stop"
                          'mouse-face 'mode-line-highlight
                          'local-map toggl-mode-line-keymap))))
  ;; Upstream's `main' without `project-name' and `major-mode'.  `time' and
  ;; `ads/battery' stay so `SPC t c' and `SPC t b' still have somewhere to draw.
  (doom-modeline-def-modeline 'main
    '(eldoc bar window-state workspace-name window-number modals matches follow
      buffer-info ads/agent remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name ads/battery grip irc mu4e gnus
      github debug repl lsp spell minor-modes input-method indent-info
      buffer-encoding ads/clock ads/process vcs check time)))

(use-package telephone-line
  :custom
  (telephone-line-primary-left-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-primary-right-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left)
  :config
(setq telephone-line-height 24)
(setq telephone-line-evil-use-short-tag t)

)

;;; modeline.el ends here
