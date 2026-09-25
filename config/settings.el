;;; settings.el --- General editor settings  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs Settings, replace-match case conversion fix, auto tangle files,
;; auto chmod
;;; Code:

;; General QoL settings
(setopt
 cursor-in-non-selected-windows nil
 large-file-warning-threshold 100000000 ;; 100Mb
 help-window-select t
 bidi-display-reordering 'left-to-right ;; no right to left text like arabic
 bidi-paragraph-direction 'left-to-right
 read-process-output-max (* 4 1024 1024);; 64k
 kill-do-not-save-duplicates t
 custom-file (concat user-emacs-directory "custom.el")
 async-shell-command-display-buffer nil
 delete-trailing-lines nil
 ffmap-machine-p-known 'reject
 window-combination-resize t
 )

(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)           ;; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ;; Iterate through CamelCase words
(put 'downcase-region 'disabled nil)    ;; Enable downcase-region
(put 'upcase-region 'disabled nil)      ;; Enable upcase-region
(put 'narrow-to-region 'disabled nil)   ;; Enable narrow commands

(add-to-list 'display-buffer-alist
  (cons "*Async Shell Command*" (cons #'display-buffer-no-window nil)))

(when (file-exists-p custom-file)
  (load custom-file nil t))

(advice-add 'custom-save-faces :override #'ignore)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook '(lambda () (display-line-numbers-mode 1)))
(setq-default display-line-numbers-widen t) ;; Keep line numbers absolute when narrowed

(defun ads/replace-match-fixedcase-when-empty (args)
  "Force FIXEDCASE for `replace-match' when the replacement is empty.
ARGS is (NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP).  Deleting a
match has no replacement text to convert, so the only thing the case
machinery can still do is upcase the wrong region off stale registers."
  (if (and (equal (car args) "") (null (nth 1 args)))
      (list "" t (nth 2 args) (nth 3 args) (nth 4 args))
    args))

(advice-add 'replace-match :filter-args #'ads/replace-match-fixedcase-when-empty)

(setq org-babel-auto-tangle-file-list
      (list ads/config-file))

(defun org-babel-auto-tangle-files ()
  ;; Automatically tangle files in ~org-babel-auto-tangle-file-list~ when one of them is saved
  (when (member buffer-file-name org-babel-auto-tangle-file-list)
    (org-babel-tangle-file buffer-file-name)))

(add-hook 'org-mode-hook
  (lambda () (add-hook 'after-save-hook 'org-babel-auto-tangle-files)))

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;;; settings.el ends here
