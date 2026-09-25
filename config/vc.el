;;; vc.el --- Magit, ediff and git links  -*- lexical-binding: t; -*-
;;; Commentary:
;; ediff, git-link, magit, magit-pre-commit
;;; Code:

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ;; keep in one frame
  (ediff-split-window-function 'split-window-horizontally) ;; side-by-side
  (ediff-merge-split-window-function 'split-window-horizontally))

(use-package git-link
  :custom
  (git-link-use-single-line-number nil)
  :config
  (ads/leader-keys
    "gf" 'git-link
    "gF" 'git-link-dispatch))

;; TODO: remove once MELPA's llama ships `any'/`all' (magit HEAD needs them)
(require 'llama nil t)
(unless (fboundp 'any)
  (defun any (pred seq)
    "Compat shim for llama's `any': non-nil if PRED holds for some elt of SEQ."
    (seq-some pred seq)))
(unless (fboundp 'all)
  (defun all (pred seq)
    "Compat shim for llama's `all': non-nil if PRED holds for every elt of SEQ."
    (seq-every-p pred seq)))

(use-package magit
  :config
  (transient-bind-q-to-quit)
  (setopt
   magit-diff-refine-hunk 'all
   magit-format-file-function #'magit-format-file-nerd-icons)
  (defun ads/git-main ()
    "Checkout main"
    (interactive)
    (magit-checkout "main"))
  (defun ads/git-lazy ()
    (interactive)
    (save-buffer)
    (magit-file-stage)
    (magit-commit-create))
  (defun ads/git-amend ()
    (interactive)
    (save-buffer)
    (magit-file-stage)
    (magit-commit-amend "--no-edit"))

  (ads/leader-keys
   "g" '(:ignore t :wk "git")
   "C-g" 'magit-dispatch
   "gg" 'magit-status
   "gk" 'magit-commit
   "gl" 'ads/git-lazy
   "gm" 'ads/git-main
   "go" 'ads/git-amend
   "gp" 'magit-push
   "gP" 'vc-push
   "gs" 'magit-file-stage
   "gS" 'magit-stage
   "gu" 'magit-file-unstage
   "gU" 'magit-unstage))

(use-package magit-pre-commit
  :vc (:url  "https://github.com/DamianB-BitFlipper/magit-pre-commit.el":rev :newest)
  :after magit)

;;; vc.el ends here
