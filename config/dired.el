;;; dired.el --- Dired and Dirvish  -*- lexical-binding: t; -*-
;;; Commentary:
;; Dired, Dirvish
;;; Code:

(require 'dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-kill-when-opening-new-dired-buffer t
      delete-by-moving-to-trash t)
(general-define-key
 :states '(normal motion emacs)
 :keymaps 'dired-mode-map
 "h" 'dired-up-directory
 "l" 'dired-find-file)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(nerd-icons subtree-state collapse file-size vc-state))
  :config
  (defvar ads/dirvish-widths (make-hash-table :test #'equal)
    "Session id to the width of the window its columns were built for.")

  (defvar ads/dirvish-scale 1.0
    "Window width over frame width, bound while a layout is being built.")

  (defun ads/dirvish-in-window (fn dv)
    "Build DV's layout in the selected window rather than the whole frame.
Drops the `delete-other-windows', keeping only this session's own panes,
and measures the window the session started in for `ads/dirvish-scale'."
    (dolist (window (window-list))
      (when (and (not (eq window (selected-window)))
                 (memq (window-buffer window) (dv-special-buffers dv)))
        (delete-window window)))
    (let (dead)
      (maphash (lambda (id _) (unless (gethash id dirvish--sessions)
                                (push id dead)))
               ads/dirvish-widths)
      (dolist (id dead) (remhash id ads/dirvish-widths)))
    ;; the window is its own width again only before the panes go in, so the
    ;; first build of a session is the one every rebuild has to measure
    (let* ((width (or (gethash (dv-id dv) ads/dirvish-widths)
                      (puthash (dv-id dv) (window-width) ads/dirvish-widths)))
           (ads/dirvish-scale (/ (float width) (frame-width))))
      (cl-letf (((symbol-function 'delete-other-windows) #'ignore))
        (funcall fn dv))))

  (defun ads/dirvish-pane-width (fn buffer alist)
    "Read the pane widths in ALIST as fractions of the window, not the frame."
    (funcall fn buffer
             (if-let* ((width (cdr (assq 'window-width alist))))
                 (cons (cons 'window-width (* ads/dirvish-scale width)) alist)
               alist)))

  (defun ads/dirvish-columns (&optional window)
    "Give a session that arrived through dired the three columns.
Hung off the same window change dirvish sets itself up on, rather than
`dirvish-setup-hook', which waits on a whole `emacs -Q' subprocess to
come back with the directory metadata before it runs."
    (with-selected-window (if (window-live-p window) window (selected-window))
      (when-let* ((dv (dirvish-curr))
                  ((eq (dv-type dv) 'default))
                  ((not (dv-curr-layout dv)))
                  ((window-live-p (dv-root-window dv))))
        (dirvish-layout-toggle))))

  (dirvish-define-preview ads-dir (file preview-window dv)
    "Preview a directory as a listing rather than as `ls' output.
Reading it happens here rather than in a subprocess, so anything huge is
left to dirvish's own dispatcher instead of hanging the cursor on it."
    (when (and (file-accessible-directory-p file)
               (not (length> (directory-files file nil nil t 2000) 1999)))
      (remhash file (dv-parent-hash dv)) ; the listing is a cache for parents
      (let ((buffer (dirvish--create-parent-buffer dv file file 'preview)))
        (set-window-buffer preview-window buffer)
        (dirvish--render-attrs preview-window 'never) ; draws the icons
        `(buffer . ,buffer))))

  ;; reloading this config into a running emacs leaves the old hook behind,
  ;; and a second set of icons with it
  (remove-hook 'dired-mode-hook 'nerd-icons-dired-mode)

  (advice-add 'dirvish--build-layout :around 'ads/dirvish-in-window)
  (advice-add 'dirvish--display-buffer :around 'ads/dirvish-pane-width)
  (advice-add 'dirvish-winbuf-change-h :after 'ads/dirvish-columns)
  (add-to-list 'dirvish-preview-dispatchers 'ads-dir)
  (ads/leader-keys "oD" '(dirvish :wk "dirvish here"))
  (general-define-key
   [remap dired] 'dirvish
   [remap dired-jump] 'dirvish)
  (general-define-key
   :states '(normal motion emacs)
   :keymaps 'dirvish-mode-map
   "<tab>" 'dirvish-subtree-toggle
   "M-t" 'dirvish-layout-toggle
   "q" 'dirvish-quit))

;;; dired.el ends here
