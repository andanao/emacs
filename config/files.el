;;; files.el --- Files, projects, history and shell helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;; async, auto-revert, bookmark+, dwim-shell-commands, log files,
;; no-littering, nov (epub), pdf-tools, projectile, recentf, rg (ripgrep),
;; save-hist, sudo-edit, tramp
;;; Code:

(use-package async
  :config
  (async-bytecomp-package-mode 1))

(setopt revert-without-query '(".*"))
(use-package autorevert
  :ensure nil                           ; built in
  :custom
  ;; kqueue says the moment a file changes, so this is faster than the 0.1s
  ;; interval it replaces *and* free, where that one was stat-ing every one of a
  ;; hundred-odd buffers ten times a second.  The interval is now only the
  ;; fallback for files notification cannot watch.
  (auto-revert-avoid-polling t)
  (auto-revert-interval 5 "Fallback only; notifications do the work")
  ;; [[*agent-shell][agent-shell]] appends to its transcript continuously, and a buffer visiting
  ;; one announced every single revert into the echo area while I was typing
  ;; somewhere else.  A revert I did not ask about is not news.
  (auto-revert-verbose nil)
  :config
  (if (eq system-type 'windows-nt)
      (global-auto-revert-mode nil)
      (global-auto-revert-mode t)))

(use-package bookmark+
  :vc (:url "https://github.com/emacsmirror/bookmark-plus"
       :branch "master"))
(customize-set-variable 'bookmark-default-file '"~/.emacs.d/bookmarks")
(customize-set-variable 'bmkp-last-bookmark-file '"~/.emacs.d/bookmarks")
(customize-set-variable 'bmkp-last-as-first-bookmark-file 'nil)

;; [[*no-littering][no-littering]] loads later and re-`setq's these out from under the
;; settings above, so set them again once it is up.  `bookmark-save' writes
;; `bmkp-current-bookmark-file', which is how saving ended up aimed at a
;; year-old file under var/bmkp/.  It is a plain `defvar', hence the `setq'.
(with-eval-after-load 'no-littering
  (customize-set-variable 'bookmark-default-file '"~/.emacs.d/bookmarks")
  (customize-set-variable 'bmkp-last-bookmark-file '"~/.emacs.d/bookmarks")
  (setq bmkp-current-bookmark-file (expand-file-name "~/.emacs.d/bookmarks")))

;; `bookmark-write-file' prints each record with `pp'.  Since Emacs 29 `pp'
;; prints straight into the buffer via `pp-fill', which is not narrowed to the
;; object: breaking a long line it steps over the enclosing alist's own closing
;; paren.  Every later record then lands outside the alist and the next save
;; dies with "Invalid bookmark-file".  It only bites the first record of a save.
(require 'pp)

(defun ads/pp-fill-narrowed (object-or-beg &optional end)
  "Like `pp-fill', but never reflow text outside the object being printed.
Called with one argument, print it at point inside a narrowing so
`pp-fill' cannot pull a following close paren onto the object's last line."
  (if end
      (pp-fill object-or-beg end)
    (let ((print-escape-newlines pp-escape-newlines)
          (print-quoted t)
          (start (point)))
      (prin1 object-or-beg (current-buffer))
      (save-restriction
        (narrow-to-region start (point))
        (pp-fill (point-min) (point-max))
        (goto-char (point-max))))))

(define-advice bookmark-write-file (:around (fn &rest args) ads/narrowed-pp)
  "Keep `pp' inside the record it is printing.  See `ads/pp-fill-narrowed'."
  (let ((pp-default-function #'ads/pp-fill-narrowed))
    (apply fn args)))

;; Only needed for files written before the `pp' advice above went in.
(defun ads/repair-bookmark-file (file)
  "Move FILE's stray alist-closing paren back to the end of the file.
Back FILE up to FILE.broken first.  Do nothing if FILE already parses
as a single alist."
  (interactive (list (read-file-name "Bookmark file: " nil bmkp-current-bookmark-file t)))
  (let ((file   (expand-file-name file))
        (stamp  ";;; -*- End Of Bookmark File Format Version Stamp -*-\n"))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (search-forward stamp)
      (forward-sexp 1)
      (if (save-excursion (skip-chars-forward " \t\n") (eobp))
          (message "%s is already well formed" file)
        (copy-file file (concat file ".broken") t)
        (delete-char -1)                ; drop the premature ")"
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert ")\n")
        (goto-char (point-min))         ; verify before writing anything out
        (search-forward stamp)
        (let ((n  (length (read (current-buffer)))))
          (unless (save-excursion (skip-chars-forward " \t\n") (eobp))
            (error "Repair failed: still junk after the alist"))
          (write-region (point-min) (point-max) file)
          (message "Repaired %s: %d bookmarks (backup at %s.broken)" file n file))))))

(define-advice ffap-read-file-or-url (:filter-args (args) ads/bmkp-url-from-clipboard)
  "Offer a URL on the clipboard as the URL to bookmark."
  (if (eq this-command 'bmkp-url-target-set)
      (let ((clip (ignore-errors (string-trim (current-kill 0 t)))))
        (if (and clip (not (string-match-p "[[:space:]]" clip)) (ffap-url-p clip))
            (list (car args) clip)
          args))
    args))

(use-package dwim-shell-command)

(require 'ansi-color)

(defun ads/log-colors ()
  "Turn the ANSI escapes in this buffer into the colours they name."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))
    ;; [[*auto-revert][auto-revert]] will not touch a modified buffer, and stripping the escapes is
    ;; what modified it, so without this a log still being written stops following.
    (set-buffer-modified-p nil)))

(define-derived-mode ads/log-mode fundamental-mode "Log"
  "Major mode for reading captured terminal output."
  (visual-fill-column-mode -1)
  (setq-local truncate-lines t)
  ;; Reverting preserves the mode, so the colours have to be reapplied by hand
  ;; every time the file grows.
  (add-hook 'after-revert-hook #'ads/log-colors nil t)
  (ads/log-colors))

(add-to-list 'auto-mode-alist '("\\.log\\'" . ads/log-mode))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(use-package no-littering)

(use-package nov
  :custom
  (nov-text-width 80)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t) ;; sharp on retina
  (pdf-view-resize-factor 1.1)
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install t)
  (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

(defun ads/pdf-view-refresh-theme ()
  "Re-render every open PDF against the current theme's colours."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'pdf-view-mode)
                 pdf-view-themed-minor-mode)
        (pdf-view-refresh-themed-buffer t)))))

(with-eval-after-load 'pdf-tools
  (add-hook 'ef-themes-after-load-theme-hook #'ads/pdf-view-refresh-theme))

(defvar ads/pdf-view-keys-set nil)

(defun ads/pdf-view-keys ()
  "Layer my bindings on top of evil-collection's `pdf-view-mode-map'."
  (unless ads/pdf-view-keys-set
    (setq ads/pdf-view-keys-set t)
    (general-define-key
     :states '(normal visual motion) :keymaps 'pdf-view-mode-map
     "J" 'pdf-view-next-page-command
     "K" 'pdf-view-previous-page-command
     "zt" 'pdf-view-themed-minor-mode
     "a" '(:ignore t :wk "annotate")
     "ah" 'pdf-annot-add-highlight-markup-annotation
     "au" 'pdf-annot-add-underline-markup-annotation
     "as" 'pdf-annot-add-strikeout-markup-annotation
     "at" 'pdf-annot-add-text-annotation
     "ad" 'pdf-annot-delete
     "al" 'pdf-annot-list-annotations)))

(add-hook 'pdf-view-mode-hook #'ads/pdf-view-keys)

(use-package projectile
  :custom
  (projectile-sort-order 'recently-active)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  (add-to-list 'projectile-globally-ignored-directories "*target")
  (add-to-list 'projectile-globally-ignored-directories "*venv"))

(ads/leader-keys
   "p" '(:ignore t :wk "projects")
   "pf" 'projectile-find-file-dwim
   "pp" 'consult-project-buffer
   "pP" 'projectile-switch-project
   "ps" 'projectile-switch-project
   "pj" 'projectile-next-project-buffer
   "pg" 'projectile-ripgrep
   "pk" 'projectile-previous-project-buffer)

(use-package recentf
  :ensure nil                           ; built in
  :custom
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files")
  :config
  (recentf-mode)
  )

(use-package rg

  :config
  (rg-enable-default-bindings)
  (rg-enable-menu))

(use-package savehist
  :ensure nil                           ; built in
  :config
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring kill-ring)

  (add-hook 'savehist-save-hook
            (lambda ()
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring)))))
  )

(use-package sudo-edit)

(require 'tramp)
(setopt remote-file-name-access-timeout 300)

;; SPC s r / SPC s R: a terminal or dired on a Host from ~/.ssh/config,
;; wildcard patterns left out.  Both start in the remote home directory.
(defun ads/ssh-hosts ()
  "Hosts named in ~/.ssh/config, without the wildcard patterns."
  (seq-uniq
   (seq-remove (lambda (host) (string-match-p "[*?!]" host))
               (delq nil (mapcar #'cadr (tramp-parse-sconfig "~/.ssh/config"))))))

(defun ads/read-ssh-host ()
  "Ask for a host from ~/.ssh/config."
  (completing-read "Host: " (ads/ssh-hosts)))

(defun ads/remote-ghostel (host)
  "Start a terminal in HOST's home directory."
  (interactive (list (ads/read-ssh-host)))
  (let ((default-directory (format "/ssh:%s:~/" host)))
    (ghostel '(4))))

(defun ads/remote-dired (host)
  "Open HOST's home directory in dired."
  (interactive (list (ads/read-ssh-host)))
  (dired (format "/ssh:%s:~/" host)))

(ads/leader-keys
  "sr" 'ads/remote-ghostel
  "sR" 'ads/remote-dired)

;;; files.el ends here
