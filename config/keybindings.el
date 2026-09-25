;;; keybindings.el --- general.el and the leader-key map  -*- lexical-binding: t; -*-
;;; Commentary:
;; General.el, eval ~e~, quit ~q~, narrow ~n~, windows, buffers, frames ~j~,
;; kill and restore ~k~, config ~c~, Toggles ~t~, Toggle frame decoration
;;; Code:

(use-package general
  :demand t
  :ensure t
  :config
  (general-override-mode)
  (general-auto-unbind-keys))

(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator emacs)
 :prefix "SPC"
 :global-prefix "C-SPC")

(general-create-definer ads/leader-keys
  :keymaps 'override
  :states '(insert normal hybrid motion visual operator emacs)
  :wk-full-keys nil
  :prefix "SPC"
  :global-prefix "C-SPC")

(defmacro ads/leader-def (key desc &rest body)
  "Define a leader key KEY with description DESC and BODY as the lambda."
  `(ads/leader-keys ,key '((lambda () (interactive) ,@body) :which-key ,desc)))

(defmacro ads/image-key (predicate command)
  "A binding running COMMAND only where PREDICATE says point is on an image.
Elsewhere the filter answers nil, which reads as unbound, so the key
falls through to whatever evil does with it — `+' and `0' are motions I
use far more often than I resize anything.  The same `menu-item' trick
evil-collection uses to scope its permission keys to a live prompt."
  `(list 'menu-item "" nil :filter
         (lambda (&optional _) (when (,predicate) ,command))))

(defun ads/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   ((evil-emacs-state-p)
    (evil-normal-state))
   (t
    (keyboard-quit))))

(general-define-key
 :states '(normal hybrid motion visual operator emacs)
 '"C-g" 'ads/keyboard-quit-dwim)

(ads/leader-keys
  "r" 'replace-regexp
  "C-j" 'jump-to-register)

(defun ads/eval-enclosing-sexp ()
  "Evaluate the innermost sexp containing point."
  (interactive)
  (save-excursion
    (when (nth 3 (syntax-ppss)) (goto-char (nth 8 (syntax-ppss))))
    (up-list 1 t t)
    (call-interactively #'eval-last-sexp)))

(ads/leader-keys
  "e" '(:ignore t :which-key "eval")
  "eb" 'eval-buffer
  "ed" 'eval-defun
  "ee" 'eval-expression
  "er" 'eval-region
  "ep" 'pp-eval-last-sexp
  "ex" 'ads/eval-enclosing-sexp
  "es" 'eval-last-sexp
  )

(ads/leader-keys
  "q" '(:ignore t :which-key "quit")
  "qQ" 'save-buffers-kill-emacs
  "qE" 'kill-emacs
  )

(defun ads/clone-indirect-here ()
  "Clone the current buffer into an indirect buffer in another window."
  (clone-indirect-buffer-other-window nil t))

(defun ads/narrow-section-dwim (&optional indirect)
  "Narrow to the section at point, dispatching on the major mode.
Org narrows to the subtree, markdown to its subtree, prog modes to the
enclosing defun and everything else to the page.  With INDIRECT, narrow
an indirect clone in another window so the base buffer stays widened."
  (interactive "P")
  (when indirect (ads/clone-indirect-here))
  (cond
   ((derived-mode-p 'org-mode)
    (org-narrow-to-subtree)
    (org-fold-show-all))
   ((derived-mode-p 'markdown-ts-mode)
    (outline-mark-subtree)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))
   ((derived-mode-p 'prog-mode) (narrow-to-defun))
   (t (narrow-to-page))))

(defun ads/narrow-to-defun-dwim (&optional indirect)
  "Narrow to the defun at point.
With INDIRECT, narrow an indirect clone in another window."
  (interactive "P")
  (when indirect (ads/clone-indirect-here))
  (narrow-to-defun))

(defun ads/goto-heading ()
  "Jump to a heading or symbol using the best source for the major mode.
Outline headings in org and markdown, imenu symbols everywhere else."
  (if (derived-mode-p 'org-mode 'markdown-ts-mode)
      (consult-outline)
    (condition-case nil
        (consult-imenu)
      (error (consult-outline)))))

(defun ads/outline-dwim (&optional indirect)
  "Widen, jump to a heading or symbol, then narrow to it.
With INDIRECT, narrow an indirect clone in another window."
  (interactive "P")
  (widen)
  (ads/goto-heading)
  (ads/narrow-section-dwim indirect))

(defun ads/outline-goto ()
  "Widen then jump to a heading or symbol without narrowing."
  (interactive)
  (widen)
  (ads/goto-heading))

(ads/leader-keys
  "n" '(:ignore t :which-key "narrow")
  "nn" 'narrow-to-region
  "ns" 'ads/narrow-section-dwim
  "nd" 'ads/narrow-to-defun-dwim
  "ni" 'clone-indirect-buffer-other-window
  "np" 'narrow-to-page
  "nw" 'widen

  "oo" 'ads/outline-dwim
  "of" 'ads/outline-goto
  )

(ads/leader-def "nS" "narrow section indirect" (ads/narrow-section-dwim t))
(ads/leader-def "nD" "narrow defun indirect" (ads/narrow-to-defun-dwim t))
(ads/leader-def "oO" "outline narrow indirect" (ads/outline-dwim t))

(ads/leader-keys
  "j" '(:ignore t :which-key "frames")

  "jQ" 'delete-frame
  "jN" 'tear-off-window
  "jR" 'set-frame-name
  "jr" 'select-frame-by-name

  "j=" 'balance-windows-area
  "j_" 'split-window-vertically

  "jh" 'evil-window-left
  "jj" 'evil-window-down
  "jk" 'evil-window-up
  "jl" 'evil-window-right

  "jH" 'evil-window-move-far-left
  "jJ" 'evil-window-move-very-bottom
  "jK" 'evil-window-move-very-top
  "jL" 'evil-window-move-far-right
  )

(winner-mode)
(ads/leader-keys
   "k" '(:ignore t :wk "kill")
   "kj" 'kill-buffer-and-window
   "kk" 'kill-current-buffer
   "kl" 'delete-window
   "k," 'winner-undo
   "ki" 'winner-redo)

;; bound to suspend-frame by default
(global-unset-key (kbd "C-x C-z"))

(ads/leader-keys "c" '(:ignore t :which-key "config"))

(ads/leader-def "cc" "emacs config" (progn (find-file ads/config-file)
                                          (ads/outline-dwim)))

(ads/leader-def "cI" "load init" (load-file user-init-file))

(ads/leader-def "cC" "emacs config magit"
                (magit-status (file-name-directory ads/config-file)))

(defun ads/add-config-package (package-name)
  "Add a new package section to the config file in alphabetical order.
Prompts for PACKAGE-NAME, finds the correct position in the Packages
section, and inserts a heading with a use-package elisp source block."
  (interactive "sPackage name: ")
  (let ((config-buffer (find-file-noselect ads/config-file)))
    (with-current-buffer config-buffer
      (goto-char (point-min))
      ;; Find the Packages section
      (unless (re-search-forward "^\\* Packages$" nil t)
        (error "Could not find Packages section"))
      (forward-line 1)
      ;; Find the right alphabetical position among ** headings
      (let ((insert-point nil)
            (package-lower (downcase package-name)))
        (while (and (not insert-point)
                    (re-search-forward "^\\*\\* \\([a-zA-Z0-9_-]+\\)" nil t))
          (let ((current-pkg (downcase (match-string 1))))
            (when (string< package-lower current-pkg)
              (setq insert-point (line-beginning-position)))))
        ;; If no position found, insert before the next top-level heading
        (unless insert-point
          (if (re-search-forward "^\\* " nil t)
              (setq insert-point (line-beginning-position))
            (setq insert-point (point-max))))
        (goto-char insert-point)
        (insert (format "\n** %s\n\n#+begin_src emacs-lisp\n(use-package %s)\n#+end_src\n"
                        package-name package-name))))
    (switch-to-buffer config-buffer)
    (goto-char (point-min))
    (re-search-forward (format "^\\*\\* %s$" (regexp-quote package-name)) nil t)
    (message "Added package: %s" package-name)))

(ads/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" 'modus-themes-toggle    ; toggle theme
    "tl" 'toggle-truncate-lines  ; toggle lines
    "tb" 'display-battery-mode   ; toggle battery
    "td" 'toggle-debug-on-error
    "tc" 'display-time-mode      ; toggle clock
    )

(defun ads/toggle-frame-decorations ()
  "Toggle frame decorations (title bar and borders)"
  (interactive)
  (let* ((current-state (frame-parameter nil 'undecorated))
         (new-state (not current-state)))
    (set-frame-parameter nil 'undecorated new-state)
    (message "Frame decorations %s" (if new-state "hidden" "shown"))))
(ads/leader-keys "tw" 'ads/toggle-frame-decorations) ;; Toggle window

;;; keybindings.el ends here
