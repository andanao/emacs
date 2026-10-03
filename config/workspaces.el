;;; workspaces.el --- Workspaces, their switcher and per-project setup  -*- lexical-binding: t; -*-
;;; Commentary:
;; workspaces, switching, with preview ~hh~, this workspace's buffers ~w~,
;; a workspace per project, keybindings ~h~
;;; Code:

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  :config
  (tab-bar-mode 1))

(defun ads/workspace-names ()
  "The name of every workspace on this frame, in tab order."
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

(defun ads/workspace-current ()
  "The name of the workspace I am in."
  (alist-get 'name (assq 'current-tab (frame-parameter nil 'tabs))))

(defun ads/workspace-index ()
  "The position of the workspace I am in."
  (seq-position (tab-bar-tabs) 'current-tab
                (lambda (tab key) (eq (car tab) key))))

(defun ads/workspace-select (name)
  "Switch to the workspace called NAME."
  (when-let* ((index (seq-position (ads/workspace-names) name)))
    (tab-bar-select-tab (1+ index))))

(defun ads/workspace-buffers (&optional index)
  "The live buffers of the workspace at INDEX, or of the one I am in."
  (let* ((tab (and index (nth index (tab-bar-tabs))))
         (buffers (if (or (null tab) (eq (car tab) 'current-tab))
                      (frame-parameter nil 'buffer-list)
                    (alist-get 'wc-bl tab))))
    (seq-filter #'buffer-live-p buffers)))

(defvar ads/workspace-history nil
  "Completion history for `ads/workspace-switch'.")

(defun ads/workspace--candidates ()
  "Alist of display name to position, most recently left first, mine last.
Mine last because a picker that opens on the layout already in front of
me previews nothing and looks broken; tab-bar stamps a workspace with the
time I left it, which is the order I want the rest in."
  (let* ((tabs (seq-map-indexed #'cons (tab-bar-tabs)))
         (mine (seq-find (lambda (entry) (eq (car (car entry)) 'current-tab)) tabs))
         (rest (seq-sort-by (lambda (entry) (or (alist-get 'time (car entry)) 0)) #'>
                            (delq mine tabs)))
         (seen (make-hash-table :test #'equal)))
    (mapcar (lambda (entry)
              (let* ((name (or (alist-get 'name (car entry)) "?"))
                     (nth (puthash name (1+ (gethash name seen 0)) seen)))
                (cons (if (> nth 1) (format "%s<%d>" name nth) name) (cdr entry))))
            (append rest (and mine (list mine))))))

(defun ads/workspace--shell (index &optional waiting)
  "The agent shell in the workspace at INDEX worth reporting on.
Whichever one is WAITING on me, otherwise the one I used last."
  (when (fboundp 'agent-shell-buffers)
    (let ((shells (seq-intersection (ads/workspace-buffers index) (agent-shell-buffers))))
      (or (car (seq-intersection shells waiting))
          (car shells)))))

(defun ads/workspace--window-buffers (state)
  "The buffer names in the `window-state' STATE, in window order.
Walking both halves of every cons, because a window state is full of
dotted pairs like =(pixel-width . 4)= that no list function will touch."
  (cond
   ((not (consp state)) nil)
   ((and (eq (car state) 'buffer) (stringp (cadr state))) (list (cadr state)))
   (t (append (ads/workspace--window-buffers (car state))
              (ads/workspace--window-buffers (cdr state))))))

(defun ads/workspace-windows (index)
  "What is on screen in the workspace at INDEX, in window order.
A workspace I am not in is a saved `window-state', so read the buffer
names straight out of it rather than restoring anything to look."
  (let ((tab (nth index (tab-bar-tabs))))
    (if (eq (car tab) 'current-tab)
        (mapcar (lambda (window) (buffer-name (window-buffer window)))
                (window-list nil 'nomini))
      (delete-dups (ads/workspace--window-buffers (alist-get 'ws tab))))))

(defun ads/workspace--annotate (candidates)
  "Annotation function for the workspaces in CANDIDATES.
Which sessions want me is settled once, when the picker opens: asking
every agent shell its status again for every candidate on every
keystroke is both slow and a way for the list to shift under me."
  (let ((waiting (and (fboundp 'ads/agent-shell-waiting) (ads/agent-shell-waiting))))
    (lambda (name)
      (when-let* ((index (alist-get name candidates nil nil #'equal))
                  (buffers (ads/workspace-buffers index)))
        (consult--annotate-align
         name
         ;; Decoration is never worth an error: one unhappy shell would
         ;; otherwise take out the whole list.
         (with-demoted-errors "Workspace annotation: %S"
           (concat
            (or (when-let* ((shell (ads/workspace--shell index waiting)))
                  (ads/agent-shell-status-string shell))
                (propertize (abbreviate-file-name
                             (buffer-local-value 'default-directory (car buffers)))
                            'face 'completions-annotations))
            (propertize (concat "   " (string-join (ads/workspace-windows index) "  "))
                        'face 'shadow))))))))

(defun ads/workspace--state (candidates origin)
  "Preview the workspace in CANDIDATES the cursor is on, ORIGIN on quit.
No NAME means reset, which is ORIGIN; a NAME I cannot place is the
completion UI mid-thought and worth nothing but sitting still."
  (lambda (action name)
    (when (memq action '(preview return))
      (when-let* ((index (if name
                             (alist-get name candidates nil nil #'equal)
                           origin)))
        ;; A preview runs from `post-command-hook', and an error thrown there
        ;; gets the hook function removed — one layout Emacs is unhappy to
        ;; restore would silently kill preview for the rest of the session,
        ;; which is exactly what flaky previewing looks like.  Say so in
        ;; *Messages* and carry on.
        (with-demoted-errors "Workspace preview: %S"
          (tab-bar-select-tab (1+ index))
          ;; Emacs skips redisplay while there is input pending, so holding the
          ;; movement key down swaps the whole frame without painting any of
          ;; it.  A preview that cannot be seen is not one.
          (redisplay t))))))

(defun ads/workspace--restore-times (times origin)
  "Undo the visits preview logged, counting only ORIGIN as one I left.
TIMES is the time each workspace was last left, by position."
  (seq-do-indexed
   (lambda (tab index)
     (when (alist-get 'time tab)
       (setf (alist-get 'time tab)
             (if (= index origin)
                 (float-time)
               (or (nth index times) (alist-get 'time tab))))))
   (tab-bar-tabs)))

(defun ads/workspace-switch ()
  "Pick a workspace, previewing each layout on the way past it.
Workspaces belong to a frame, so a second frame starts with none of
them; say that rather than offering a list of one that previews nothing."
  (interactive)
  (when (length< (tab-bar-tabs) 2)
    (user-error "Only one workspace on this frame%s"
                (if-let* ((elsewhere (seq-filter
                                      (lambda (frame)
                                        (and (not (eq frame (selected-frame)))
                                             (length> (tab-bar-tabs frame) 1)))
                                      (frame-list))))
                    (format "; %s has the rest"
                            (mapconcat (lambda (frame)
                                         (format "%s" (frame-parameter frame 'name)))
                                       elsewhere ", "))
                  "")))
  (let* ((candidates (ads/workspace--candidates))
         (origin (ads/workspace-index))
         (times (mapcar (lambda (tab) (alist-get 'time tab)) (tab-bar-tabs)))
         (read-minibuffer-restore-windows nil))
    (unwind-protect
        (consult--read (mapcar #'car candidates)
                       :prompt "Workspace: "
                       :category 'ads/workspace
                       :history 'ads/workspace-history
                       :require-match t
                       :sort nil
                       :annotate (ads/workspace--annotate candidates)
                       :state (ads/workspace--state candidates origin))
      (ads/workspace--restore-times times origin))))

(with-eval-after-load 'consult
  (defvar ads/consult-source-workspace
    (list :name     "Workspace"
          :narrow   ?w
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :hidden   t
          :items    (lambda ()
                      (consult--buffer-query :sort 'visibility
                                             :buffer-list (ads/workspace-buffers)
                                             :as #'buffer-name)))
    "Buffers belonging to the current workspace, for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'ads/consult-source-workspace 'append)

  (defun ads/consult-workspace-buffer ()
    "Pick a buffer from this workspace, with preview."
    (interactive)
    (consult-buffer (list ads/consult-source-workspace))))

(defun ads/workspace-project-name (&optional root)
  "The workspace name for the project at ROOT, or for the current project."
  (when-let* ((root (or root (projectile-project-root))))
    (file-name-nondirectory (directory-file-name root))))

(defun ads/workspace-fresh-p ()
  "Non-nil while this frame is still on its unnamed starting workspace."
  (let ((tabs (tab-bar-tabs)))
    (and (= (length tabs) 1)
         (not (alist-get 'explicit-name (car tabs))))))

(defun ads/workspace-new (name)
  "Start a new workspace called NAME on a clean window.
A new tab inherits the buffer list of the one I made it from, which is
the one thing a workspace should not inherit, so empty it."
  (interactive (list (read-string "New workspace: " (ads/workspace-project-name))))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (set-frame-parameter nil 'buffer-list (list (current-buffer)))
  (set-frame-parameter nil 'buried-buffer-list nil))

(defun ads/workspace-switch-project (switch project &rest args)
  "Run SWITCH for PROJECT in a workspace of its own.
Advice around `projectile-switch-project-by-name'."
  (let ((name (ads/workspace-project-name project)))
    (cond
     ((equal name (ads/workspace-current))
      (apply switch project args))
     ((member name (ads/workspace-names))
      (ads/workspace-select name))
     (t
      (if (ads/workspace-fresh-p)
          (tab-bar-rename-tab name)
        (ads/workspace-new name))
      (apply switch project args)))))

(advice-add 'projectile-switch-project-by-name :around #'ads/workspace-switch-project)

(ads/leader-keys
  "h" '(:ignore t :wk "workspaces")
  "hh" 'ads/workspace-switch
  "hb" 'ads/consult-workspace-buffer
  "hn" 'ads/workspace-new
  "hr" 'tab-bar-rename-tab
  "hd" 'tab-bar-close-tab
  "hu" 'tab-bar-undo-close-tab
  "hj" 'tab-bar-switch-to-next-tab
  "hk" 'tab-bar-switch-to-prev-tab
  "hl" 'tab-bar-switch-to-recent-tab)

;;; workspaces.el ends here
