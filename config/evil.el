;;; evil.el --- Evil and its companions  -*- lexical-binding: t; -*-
;;; Commentary:
;; evil, evil-anzu, evil-collection, evil-surround, undo-tree
;;; Code:

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-want-integration t)
  (evil-want-keybinding  nil)
  (evil-want-C-u-scroll  nil)
  (evil-want-C-i-jump    nil)
  (evil-want-C-w-delete  nil)
  (evil-complete-all-buffers nil)
  :hook
  (after-init . evil-mode)
  (after-save . evil-normal-state)

  :config
  (general-define-key :states 'insert "C-g" 'evil-normal-state)
  (general-define-key "C-;" 'evil-switch-to-windows-last-buffer)

  ;; Use visual line motions even outside of visual-line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; set back normal mouse behaviour
  (define-key evil-motion-state-map [down-mouse-1] nil)
  ;; unbind q for macros
  (define-key evil-normal-state-map (kbd "q") 'nil)
  (define-key evil-normal-state-map (kbd "Q") 'nil)
  (evil-mode))

(general-define-key
  :states '(normal insert)
  "C-w C-h" 'evil-window-left
  "C-w C-j" 'evil-window-down
  "C-w C-k" 'evil-window-up
  "C-w C-l" 'evil-window-right)

(use-package evil-anzu
  :after (evil)
  :config
  (global-anzu-mode))

(use-package evil-collection
  :after (evil)
  :custom
  (evil-collection-calendar-setup-want-org-bindings t)
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(setq undo-limit        (* 8 1024 1024)
      undo-strong-limit (* 16 1024 1024)
      undo-outer-limit  (* 128 1024 1024))

(defvar ads/undo-tree-visualizer-keep-commands
  '(ignore execute-extended-command keyboard-quit ads/keyboard-quit-dwim)
  "Commands that may run in the undo-tree visualizer without closing it.
Anything named for undo-tree, and anything that scrolls, is kept anyway.")

(defun ads/undo-tree-visualizer-accept ()
  "Take the state under point and close the visualizer."
  (interactive)
  (when (bound-and-true-p undo-tree-visualizer-selection-mode)
    (undo-tree-visualizer-set))
  (undo-tree-visualizer-quit))

(defun ads/undo-tree-visualizer-keep-open-p ()
  "Whether `this-command' is one the visualizer should stay open for."
  (or (not (symbolp this-command))      ; its scroll keys are anonymous closures
      (memq this-command ads/undo-tree-visualizer-keep-commands)
      (string-match-p "undo-tree-\\|scroll" (symbol-name this-command))))

(defun ads/undo-tree-visualizer-quit-on-stray-key ()
  "Close the visualizer as soon as a command that isn't part of it runs.
The key is swallowed rather than passed on, so a stray `w' cannot land in
the buffer being visualised."
  (unless (ads/undo-tree-visualizer-keep-open-p)
    (setq this-command #'ignore)
    (with-demoted-errors "undo-tree: %S" (undo-tree-visualizer-quit))))

(defun ads/undo-tree-visualizer-setup ()
  "Make the current visualizer buffer close on any key that isn't its own."
  (add-hook 'pre-command-hook #'ads/undo-tree-visualizer-quit-on-stray-key nil t))

(defvar ads/undo-tree-directory (expand-file-name "tmp/undo-tree/" user-emacs-directory))

(defun ads/undo-tree-quietly (fn &rest args)
  "Call FN with ARGS without echoing into the minibuffer."
  (let ((inhibit-message t)) (apply fn args)))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,ads/undo-tree-directory)))
  :config
  (make-directory ads/undo-tree-directory t)
  (advice-add 'undo-tree-save-history :around #'ads/undo-tree-quietly)
  (global-undo-tree-mode)
  ;; Not `evil-set-undo-system', which sets the two function variables but leaves
  ;; `evil-undo-system' itself nil; the defcustom's setter does both.
  (setopt evil-undo-system 'undo-tree)

  ;; `evil-integration' already wires the visualizer up, by remapping `evil-next-line' and
  ;; friends in both of its keymaps — so `h'/`l'/`RET' work out of the box, but the [[*evil][visual-line
  ;; motions]] on `j'/`k' are different commands and evil doesn't remap those.  Borrow whatever
  ;; evil pointed the line motions at, so each map keeps its own meaning.
  (dolist (map (list undo-tree-visualizer-mode-map
                     undo-tree-visualizer-selection-mode-map))
    (define-key map [remap evil-next-visual-line]
                (lookup-key map [remap evil-next-line]))
    (define-key map [remap evil-previous-visual-line]
                (lookup-key map [remap evil-previous-line])))

  ;; `t' is a plain binding rather than a remap, so motion state's `evil-find-char-to' wins.
  (general-define-key
   :states '(normal motion emacs)
   :keymaps 'undo-tree-visualizer-mode-map
   "t"   'undo-tree-visualizer-toggle-timestamps
   "RET" 'ads/undo-tree-visualizer-accept)

  (add-hook 'undo-tree-visualizer-mode-hook #'ads/undo-tree-visualizer-setup))

(ads/leader-keys "u" '(undo-tree-visualize :wk "undo tree"))

;;; evil.el ends here
