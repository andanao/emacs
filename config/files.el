;;; files.el --- Files, projects, history and shell helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;; async, auto-revert, bookmark+, dwim-shell-commands, no-littering,
;; nov (epub), pdf-tools, projectile, recentf, rg (ripgrep), save-hist,
;; sudo-edit, tramp
;;; Code:

(use-package async
  :config
  (async-bytecomp-package-mode 1))

(setopt revert-without-query '(".*"))
(use-package autorevert
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

(use-package dwim-shell-command)

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
  (add-hook 'modus-themes-after-load-theme-hook #'ads/pdf-view-refresh-theme))

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

;;; files.el ends here
