;;; ghostel.el --- ghostel terminal sessions  -*- lexical-binding: t; -*-
;;; Commentary:
;; ghostel, popup terminal, session names, ghostel consult source ~t~
;;; Code:

(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel"
       :lisp-dir "lisp"
       :rev :newest)
  ;; Every command below is autoloaded, so the dynamic module stays unloaded
  ;; until I ask for a terminal.
  :defer t
  :custom
  ;; ghostel is half Zig.  The flake supplies only the elisp, so the native
  ;; module has to live somewhere ghostel can write to - and the package
  ;; directory it defaults to is a read-only /nix/store path.  ghostel already
  ;; keeps its ssh terminfo cache here, so the module joins it: outside both
  ;; the repo and ~/.emacs.d, and so unaffected by cutover.
  (ghostel-module-directory
   (expand-file-name "ghostel" (or (getenv "XDG_CACHE_HOME")
                                   (expand-file-name ".cache" "~")))))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :custom
  (evil-ghostel-initial-state 'normal))

(defvar ghostel-buffer-name)

(defconst ads/ghostel-popup-stem "ghostel-popup"
  "Name prefix the popup keeps through every rename.")

(defconst ads/ghostel-popup-name (concat "*" ads/ghostel-popup-stem "*")
  "Buffer name the always-on terminal is created under.
Also its identity to `ghostel', which the later rename does not disturb.")

(defconst ads/ghostel-popup-regexp
  (concat "\\`\\*?" (regexp-quote ads/ghostel-popup-stem))
  "Matches the popup as created and as renamed.
The leading star is optional because only the creation name carries one.")

(defun ads/ghostel-popup-buffer ()
  "The popup terminal, whatever `ads/ghostel-buffer-name' has renamed it to."
  (car (match-buffers ads/ghostel-popup-regexp)))

(defvar ads/ghostel-popup-height 0.3
  "Fraction of the frame the popup takes, kept for the rest of the session.")

(defun ads/ghostel--popup-fraction (window)
  "How much of its frame WINDOW takes up, as a fraction."
  (/ (float (window-total-height window))
     (window-total-height (frame-root-window window))))

(defun ads/ghostel-popup ()
  "Toggle the always-on terminal across the bottom of the frame.
Hides it when point is already there, jumps to it when it is showing
elsewhere, and starts it the first time."
  (interactive)
  (let* ((buffer (ads/ghostel-popup-buffer))
         (window (and buffer (get-buffer-window buffer))))
    (cond
     ((eq window (selected-window))
      (setq ads/ghostel-popup-height (ads/ghostel--popup-fraction window))
      (delete-window window))
     (window (select-window window))
     (t (let ((ghostel-buffer-name ads/ghostel-popup-name))
          (ghostel))))))

(defun ads/ghostel--popup-fit (window)
  "Size WINDOW to `ads/ghostel-popup-height'."
  (let ((lines (round (* ads/ghostel-popup-height
                         (window-total-height (frame-root-window window))))))
    (window-resize window (- lines (window-total-height window)) nil 'preserved)))

(defun ads/ghostel-popup-resize (delta)
  "Grow the popup by DELTA of the frame, and keep that size."
  (setq ads/ghostel-popup-height
        (min 0.9 (max 0.1 (+ ads/ghostel-popup-height delta))))
  (when-let* ((buffer (ads/ghostel-popup-buffer))
              (window (get-buffer-window buffer)))
    (ads/ghostel--popup-fit window)))

(defun ads/ghostel-popup-taller ()
  "Give the popup another tenth of the frame."
  (interactive)
  (ads/ghostel-popup-resize 0.1))

(defun ads/ghostel-popup-shorter ()
  "Take a tenth of the frame back from the popup."
  (interactive)
  (ads/ghostel-popup-resize -0.1))

(defun ads/ghostel-new ()
  "Start a terminal alongside the ones already running."
  (interactive)
  (ghostel '(4)))

;; `ghostel' pops to its buffer asking for the same window; the alist outranks
;; that, which is the whole reason this lands in a side window.  Matching the
;; prefix rather than the whole name keeps the rule working once the buffer
;; has been renamed to carry its directory.
(add-to-list 'display-buffer-alist
             `(,ads/ghostel-popup-regexp
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . ads/ghostel--popup-fit)
               ;; Survive a resize by hand.
               (preserve-size . (nil . t))))

(ads/leader-keys
  ";" 'ads/ghostel-popup
  "s" '(:ignore t :which-key "shell")
  "ss" 'ghostel
  "sn" 'ads/ghostel-new
  "sp" 'ghostel-project
  "sb" 'ads/consult-ghostel
  "sj" 'ghostel-next
  "sk" 'ghostel-previous
  "s=" 'ads/ghostel-popup-taller
  "s-" 'ads/ghostel-popup-shorter)

(defvar ghostel-identity)
(defvar ghostel-title)
(defvar ghostel-buffer-name-function)
(declare-function ghostel--rename-managed "ghostel" (new-name))

(defvar-local ads/ghostel--named-for nil
  "`default-directory' the current buffer name was built from.")

(defun ads/ghostel--short-path (directory)
  "Return DIRECTORY as ROOT/FIRST/.../LAST when it sits inside a git worktree.
Outside one, return the abbreviated path whole - there is no root to
elide against."
  (let* ((directory (directory-file-name (expand-file-name directory)))
         (root (locate-dominating-file directory ".git")))
    (if (not root)
        (abbreviate-file-name directory)
      (let* ((root (directory-file-name (expand-file-name root)))
             (relative (file-relative-name directory root))
             (parts (unless (equal relative ".") (split-string relative "/" t))))
        (string-join (cons (file-name-nondirectory root)
                           (if (> (length parts) 2)
                               (list (car parts) "..." (car (last parts)))
                             parts))
                     "/")))))

(defun ads/ghostel--where ()
  "Describe `default-directory' for this terminal's buffer name.
Remote directories read HOST:DIR and are never probed for a git root."
  (let ((host (file-remote-p default-directory 'host)))
    (if host
        (format "%s:%s" host
                (directory-file-name (file-remote-p default-directory 'localname)))
      (ads/ghostel--short-path default-directory))))

(defun ads/ghostel--name-stem ()
  "The prefix this buffer's name keeps, or nil when it is not mine to rename."
  (when (eq (alist-get 'kind ghostel-identity) 'term)
    (if (string-match-p ads/ghostel-popup-regexp (buffer-name))
        ads/ghostel-popup-stem
      "ghostel")))

(defun ads/ghostel-buffer-name (_title)
  "Name this terminal for the host and directory it is sitting in.
A `ghostel-buffer-name-function'.  Returns nil to decline, and the
unchanged name when the directory has not moved, so the git lookup stays
off the every-prompt path.  TITLE is ignored: nothing here emits one."
  (when-let* ((stem (ads/ghostel--name-stem)))
    (if (equal default-directory ads/ghostel--named-for)
        (buffer-name)
      (setq ads/ghostel--named-for default-directory)
      (format "%s: %s" stem (ads/ghostel--where)))))

(defun ads/ghostel-name-at-birth ()
  "Name a terminal as it spawns, before any OSC 7 could have arrived."
  (ghostel--rename-managed (ads/ghostel-buffer-name ghostel-title)))

(setq ghostel-buffer-name-function #'ads/ghostel-buffer-name)
(add-hook 'ghostel-pre-spawn-hook #'ads/ghostel-name-at-birth)

(with-eval-after-load 'consult
  (defvar ads/consult-source-ghostel
    (list :name     "Terminal"
          :narrow   ?t
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :annotate (lambda (candidate)
                      (when-let* ((buffer (get-buffer candidate)))
                        (abbreviate-file-name
                         (buffer-local-value 'default-directory buffer))))
          :items    (lambda ()
                      (mapcar #'buffer-name
                              (match-buffers `(and (derived-mode . ghostel-mode)
                                                   (not ,ads/ghostel-popup-regexp))))))
    "Open `ghostel' terminals, for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'ads/consult-source-ghostel)

  (defun ads/consult-ghostel ()
    "Pick a `ghostel' terminal, with preview."
    (interactive)
    (consult-buffer (list ads/consult-source-ghostel))))

;;; ghostel.el ends here
