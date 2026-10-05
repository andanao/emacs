;;; agent-shell.el --- agent-shell sessions, notifications and resume  -*- lexical-binding: t; -*-
;;; Commentary:
;; agent-shell, waiting sessions, notify when a session finishes,
;; agent-shell consult source ~a~, switch-buffer preview,
;; last night's shells, global session resume,
;; kickoff with the first prompt
;;; Code:

(defvar ads/agent-shell-sans-stack
  '("Optima"                      ; macOS only, and Linotype's - cannot be pinned
    "Libertinus Sans"             ; the flake's, and the nearest libre thing to it
    "Source Sans 3"
    "Cantarell"
    "sans-serif")
  "Families to render the conversation in, best first.
The first one installed wins, so this reads as Optima on a Mac and as
whatever the flake supplies everywhere else.  Resolved per buffer rather
than once: `font-family-list' is empty until there is a frame, which on a
daemon means every answer before the first client is wrong.")

(defun ads/agent-shell-sans ()
  "First family in `ads/agent-shell-sans-stack' this machine has."
  (let ((have (font-family-list)))
    (or (seq-find (lambda (f) (member f have)) ads/agent-shell-sans-stack)
        "sans-serif")))

(defvar-local ads/agent-shell--remap nil
  "Face-remap cookies owned by this buffer, so re-running cannot leak them.")

(defun ads/agent-shell-prose-font ()
  "Render the conversation proportionally, leaving code and tables fixed.
Buffer-local remaps rather than `custom-set-faces': they resolve against
whatever theme is current, so a toggle needs no re-application, and they
leave the faces themselves alone for every other mode that borrows them."
  (mapc #'face-remap-remove-relative ads/agent-shell--remap)
  (setq ads/agent-shell--remap nil)
  (let ((family (ads/agent-shell-sans)))
    (setq-local buffer-face-mode-face (list :family family))
    (buffer-face-mode 1)
    (setq ads/agent-shell--remap
          (cons
           ;; Inherits nothing upstream, so it would follow the body into a
           ;; proportional face and the columns would stop lining up.  The
           ;; header, border and zebra faces all inherit it.
           (face-remap-add-relative 'agent-shell-markdown-table 'fixed-pitch)
           ;; Headings inherit org-level-N, which `ef-themes-mixed-fonts'
           ;; makes variable-pitch - EtBembo, the serif.  Swap the family
           ;; and keep the size and weight `ef-themes-headings' set.
           (mapcar (lambda (n)
                     (face-remap-add-relative
                      (intern (format "agent-shell-markdown-header-%d" n))
                      (list :family family)))
                   (number-sequence 1 6))))))

(add-hook 'agent-shell-mode-hook #'ads/agent-shell-prose-font)

(defun ads/agent-shell-send-buffer ()
  "Send the whole buffer to the project's agent shell."
  (interactive)
  (save-mark-and-excursion
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (agent-shell-send-region)))

(defun ads/agent-shell--button-action ()
  "Command the button under point binds to RET, if there is one.
agent-shell hangs button actions off a `keymap' text property rather
than `button-1', so there is nothing for `push-button' to find."
  (when-let* ((map (get-char-property (point) 'keymap))
              (action (lookup-key map (kbd "RET")))
              ((commandp action)))
    action))

(defun ads/agent-shell-expand-item ()
  "Expand, collapse or follow the agent-shell item at point.
Falls through to the next item when point is on nothing expandable, so
TAB still walks the buffer."
  (interactive)
  (if-let* ((action (ads/agent-shell--button-action)))
      (call-interactively action)
    (agent-shell-next-item)))

(defun ads/agent-shell-open-link ()
  "Open the link at point, in a browser or in Emacs.
Every rendered link hangs its action off the same `keymap' property -
markdown links, bare URLs the renderer picked up, and `foo.el:12' file
references alike - so defer to that rather than deciding here.  Half of
what `agent-shell-markdown-url' holds is a file reference, not a URL,
and `browse-url' on `readme.org:214' goes nowhere good.

`thing-at-point' is the fallback, for text the renderer left alone
inside a code block or in the prompt."
  (interactive)
  (if-let* ((action (ads/agent-shell--button-action)))
      (call-interactively action)
    (if-let* ((url (thing-at-point 'url t)))
        (browse-url url)
      (user-error "No link at point"))))

(defun ads/agent-shell--on-image-p ()
  "Non-nil when point is on a rendered image."
  (and (derived-mode-p 'agent-shell-mode)
       (agent-shell-markdown--image-position-at-point)))

(defvar ads/agent-shell--scaling nil
  "Non-nil while an explicit image scale command is running.")

(defun ads/agent-shell-grow-small-image (orig start end max-width window-width)
  "Let `+' enlarge an image narrower than the width it is clamped to.
agent-shell sizes images with `:max-width', which only ever shrinks, so
a screenshot already under the clamp never grows however far `+' pushes
it - a 368px capture sits at 368 while the clamp climbs past it
unnoticed.  Where the target is wider than the file, size with `:width'
as well, which does scale up; `:max-width' rides along so agent-shell's
own stepping still reads the width back and carries on.

Only while scaling by hand.  This is also the path that first renders an
image and that re-renders on a window resize, and blowing every small
screenshot up to the clamp there would be its own bug - `0' resets to
the natural size precisely because the clamp isn't reached."
  (funcall orig start end max-width window-width)
  (when-let* ((ads/agent-shell--scaling)
              (image (get-text-property start 'display))
              ((eq (car-safe image) 'image))
              (file (image-property image :file))
              ((> max-width (car (image-size (create-image file) t))))
              ((not (eql max-width (car (image-size image t))))))
    (let ((grown (create-image file nil nil
                               :width max-width
                               :max-width max-width
                               :max-height (image-property image :max-height))))
      (image-flush grown)
      (put-text-property start end 'display grown))))

(advice-add 'agent-shell-markdown--resize-image
            :around #'ads/agent-shell-grow-small-image)

(defun ads/agent-shell-image-scale-increase ()
  "Widen the image at point, past its own size if that's where it is.
Straight to the markdown command: `agent-shell-image-scale-increase'
self-inserts a `+' at the last prompt, where an image I've just
attached lives."
  (interactive)
  (let ((ads/agent-shell--scaling t))
    (agent-shell-markdown-image-scale-increase)))

(defun ads/agent-shell-image-scale-decrease ()
  "Narrow the image at point, holding it above its own size if it's there."
  (interactive)
  (let ((ads/agent-shell--scaling t))
    (agent-shell-markdown-image-scale-decrease)))

(defun ads/agent-shell-evil-keys (&optional mode &rest _)
  "Apply the agent-shell normal-state bindings.
Run from `evil-collection-setup-hook' so these land after evil-collection's
own agent-shell module, which binds the same map."
  (when (or (null mode) (eq mode 'agent-shell))
    (general-define-key
     :states '(normal) :keymaps 'agent-shell-mode-map
     (kbd "RET") 'agent-shell-submit
     (kbd "<tab>") 'ads/agent-shell-expand-item
     ;; evil-collection spends gx on interrupt, which already has C-c C-c and
     ;; SPC a Q.  Take it back for what gx means in vim.  gy is its session id.
     "gx" 'ads/agent-shell-open-link
     "gY" 'agent-shell-copy-link-url-at-point
     "C-j" 'agent-shell-next-item
     "C-k" 'agent-shell-previous-item
     "+" (ads/image-key ads/agent-shell--on-image-p 'ads/agent-shell-image-scale-increase)
     "-" (ads/image-key ads/agent-shell--on-image-p 'ads/agent-shell-image-scale-decrease)
     "0" (ads/image-key ads/agent-shell--on-image-p 'agent-shell-markdown-image-scale-reset))))

(use-package agent-shell
  :ensure t
  ;; Nothing here defers, so this loads where it sits — alphabetically ahead of
  ;; evil, whose `evil-define-key' the config below needs.
  :after evil
  :custom
  ;; Named per project: "claude-agent @ emacs".  Fixes "which session is which".
  (agent-shell-buffer-name-format 'kebab-case)
  ;; Resume vs new is a prompt at startup, not a separate command.  The order
  ;; agent-shell ships is the right one: a new shell first, sessions under it.
  (agent-shell-session-strategy 'prompt)
  ;; Replay the conversation on restore.  The default `minimal' resumes
  ;; without one, so the agent has the context and I can't read any of it.
  (agent-shell-session-restore-verbosity 'full)
  ;; Keep the conversation visible when the agent opens a file.
  (agent-shell-file-display-action '(display-buffer-pop-up-window))
  (agent-shell-display-action '(display-buffer-same-window))
  (agent-shell-thought-process-expand-by-default nil)
  (agent-shell-tool-use-expand-by-default nil)
  ;; Two screens of ASCII art and a sponsor plug before every conversation.
  (agent-shell-show-welcome-message nil)
  :config
  ;; RET sends in normal state, inserts a newline in insert state.
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  ;; evil-collection ships an agent-shell module that binds this same map, and
  ;; it always gets the last word - re-apply from its own after-setup hook.
  (add-hook 'evil-collection-setup-hook #'ads/agent-shell-evil-keys)
  (define-key agent-shell-mode-map (kbd "C-x C-s") #'agent-shell-show-usage)
  ;; The buffer is always "modified" and never savable; red is a lie here.
  (add-hook 'agent-shell-mode-hook
            (lambda ()
              (setq-local doom-modeline-highlight-modified-buffer-name nil)))
  ;; Diff buffers are read-only review UI, not evil buffers.
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "agent-shell-diff" (buffer-name))
                (evil-emacs-state))))
  (ads/leader-keys
    "a" '(:ignore t :which-key "ai")
    "aa" 'agent-shell
    "ai" 'agent-shell
    "aA" 'agent-shell-toggle
    "a?" 'agent-shell-help-menu
    "aM" 'agent-shell-cycle-session-mode
    "am" 'agent-shell-set-session-mode
    "av" 'agent-shell-set-session-model
    "aT" 'agent-shell-set-session-thought-level
    "an" 'agent-shell-new-shell
    "aw" 'agent-shell-new-worktree-shell
    "aR" 'agent-shell-resume-session
    "ag" 'agent-shell-prompt-steer
    ;; "ac" is the [[*review comments][review]] transient; compose is a key inside it.
    "aq" 'agent-shell-interrupt
    "aX" 'agent-shell-restart
    "ar" 'agent-shell-send-region
    "ab" 'ads/agent-shell-send-buffer
    "af" 'agent-shell-send-file
    "ap" 'agent-shell-send-dwim
    "ad" 'agent-shell-send-clipboard-image
    "al" 'agent-shell-prompt-queue
    "as" 'agent-shell-switch-buffer
    "a@" 'agent-shell-insert-file
    "ay" 'agent-shell-copy-last-output
    "at" 'agent-shell-other-buffer))

(defvar-local ads/agent-shell--finished nil
  "Non-nil when this shell finished a turn I haven't looked at yet.")

(defvar ads/agent-shell--last-status nil
  "Alist of (BUFFER . STATUS) from the previous poll, for edge detection.")

(defvar ads/agent-shell--timer nil)

(defun ads/agent-shell--poll ()
  "Refresh the finished-turn flag on every live agent shell."
  (let (statuses)
    (dolist (buffer (agent-shell-buffers))
      (when (buffer-live-p buffer)
        (let ((status (agent-shell-status :shell-buffer buffer))
              (previous (alist-get buffer ads/agent-shell--last-status)))
          (push (cons buffer status) statuses)
          (with-current-buffer buffer
            (cond
             ;; Displayed anywhere means seen; nothing left to flag.
             ((get-buffer-window buffer t) (setq ads/agent-shell--finished nil))
             ((and (eq status 'ready) (eq previous 'busy))
              (setq ads/agent-shell--finished t)))))))
    (setq ads/agent-shell--last-status statuses))
  (force-mode-line-update t))

(defun ads/agent-shell-waiting ()
  "Agent shells wanting input, in recency order."
  (seq-filter (lambda (buffer)
                (and (buffer-live-p buffer)
                     (or (eq (agent-shell-status :shell-buffer buffer) 'blocked)
                         (buffer-local-value 'ads/agent-shell--finished buffer))))
              (agent-shell-buffers)))

(defun ads/agent-shell-next (&optional all)
  "Switch to the next agent shell waiting on me.
With prefix ALL, or when none are waiting, cycle through every shell."
  (interactive "P")
  (let* ((pool (or (and (not all) (ads/agent-shell-waiting))
                   (agent-shell-buffers)
                   (user-error "No agent shells")))
         (rest (cdr (memq (current-buffer) pool))))
    (pop-to-buffer (or (car rest) (car pool)))))

(defun ads/agent-shell-mode-line ()
  "Mode line tag counting shells that want me, blocked ones first."
  (when-let* ((waiting (ads/agent-shell-waiting)))
    (let ((blocked (seq-count (lambda (buffer)
                                (eq (agent-shell-status :shell-buffer buffer) 'blocked))
                              waiting)))
      (propertize (format " %s%d" (if (> blocked 0) "⚠" "✓") (length waiting))
                  'face (if (> blocked 0) 'agent-shell-error 'agent-shell-success)
                  'help-echo (mapconcat #'buffer-name waiting ", ")))))

(with-eval-after-load 'agent-shell
  (add-to-list 'global-mode-string '(:eval (ads/agent-shell-mode-line)) t)
  (unless ads/agent-shell--timer
    (setq ads/agent-shell--timer (run-with-timer 1 1 #'ads/agent-shell--poll)))
  (ads/leader-keys "a SPC" 'ads/agent-shell-next))

(defun ads/agent-shell--session-name (buffer)
  "Which session BUFFER is, minus the agent name the icon already carries."
  (let ((name (replace-regexp-in-string
               "\\`\\*\\|\\*\\(<[0-9]+>\\)?\\'" "\\1" (buffer-name buffer))))
    (if (string-match " @ \\(.+\\)\\'" name)
        (match-string 1 name)
      name)))

(defun ads/agent-shell-knockknock--on-turn-complete (event)
  "Say which session finished, and stay up until I dismiss it."
  (unless (agent-shell-knockknock--shell-visible-p (current-buffer))
    (let ((shell (current-buffer)))
      (ads/notify--broadcast
       :title (ads/agent-shell--session-name shell)
       :message (if (equal (map-nested-elt event '(:data :stop-reason)) "end_turn")
                    "Finished"
                  "Stopped")
       :icon-file (agent-shell-knockknock--icon-file shell)
       :tint t
       :action (lambda () (agent-shell-knockknock--switch-to-shell shell)))
      (agent-shell-knockknock--install-ret-binding shell))))

(defun ads/agent-shell-knockknock--on-permission-request (event)
  "Say which session is asking, and what for."
  (unless (agent-shell-knockknock--shell-visible-p (current-buffer))
    (let ((shell (current-buffer))
          (tool-call (map-nested-elt event '(:data :tool-call))))
      (ads/notify--broadcast
       :title (ads/agent-shell--session-name shell)
       :message (format "%s %s"
                        (capitalize (or (map-elt tool-call :kind) "permission"))
                        (agent-shell-knockknock--format-permission-message tool-call))
       :icon-file (agent-shell-knockknock--icon-file shell)
       :tint t
       :action (lambda () (agent-shell-knockknock--switch-to-shell shell)))
      (agent-shell-knockknock--install-ret-binding shell))))

(use-package agent-shell-knockknock
  :vc (:url "https://github.com/xenodium/agent-shell-knockknock" :rev :newest)
  :after (agent-shell knockknock)
  :custom
  (agent-shell-knockknock-duration 10 "Only how long RET jumps for; the popup outlives it")
  :config
  (advice-add 'agent-shell-knockknock--on-turn-complete
              :override #'ads/agent-shell-knockknock--on-turn-complete)
  (advice-add 'agent-shell-knockknock--on-permission-request
              :override #'ads/agent-shell-knockknock--on-permission-request))

(with-eval-after-load 'agent-shell
  (ads/leader-keys "aN" '(agent-shell-knockknock-mode :wk "notify when done")))

(with-eval-after-load 'consult
  (defun ads/agent-shell-status-string (buffer)
    "Coloured status and session title for the agent shell in BUFFER."
    (when-let* ((status (agent-shell-status :shell-buffer buffer)))
      (concat
       (propertize (format "%-8s" status)
                   'face (pcase status
                           ('busy 'agent-shell-warning)
                           ('blocked 'agent-shell-error)
                           (_ 'agent-shell-success)))
       (when-let* ((title (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                                          '(:session :title))))
         (propertize (car (split-string (string-trim title) "\n"))
                     'face 'agent-shell-session-title)))))

  (defun ads/agent-shell-annotate (candidate)
    "Status and session title for CANDIDATE, an agent shell buffer name."
    (when-let* ((buffer (get-buffer candidate))
                (status (ads/agent-shell-status-string buffer)))
      (concat " " status)))

  (defvar ads/consult-source-agent-shell
    (list :name     "Agent shell"
          :narrow   ?a
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :annotate #'ads/agent-shell-annotate
          :items    (lambda () (mapcar #'buffer-name (agent-shell-buffers))))
    "Open `agent-shell' sessions, for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'ads/consult-source-agent-shell))

(with-eval-after-load 'consult
  (defun ads/agent-shell--read-preview (prompt collection predicate require-match buffers)
    "Read from agent-shell's COLLECTION with preview, PROMPT and REQUIRE-MATCH.
BUFFERS is the buffer list COLLECTION was built from, in the same order.
PREDICATE filters candidates as it would in `completing-read'."
    (let ((entries (seq-mapn #'cons (all-completions "" collection predicate) buffers))
          (preview (consult--buffer-preview)))
      (consult--read collection
                     :prompt prompt
                     :predicate predicate
                     :require-match require-match
                     ;; nil keeps agent-shell's recency order; consult must not
                     ;; add an :annotate here or it displaces the icon affixation.
                     :sort nil
                     :category 'buffer
                     :state (lambda (action candidate)
                              (funcall preview action
                                       (when-let* ((buffer (alist-get candidate entries
                                                                      nil nil #'equal)))
                                         (buffer-name buffer)))))))

  (defun ads/agent-shell-read-with-preview (original &rest args)
    "Give ORIGINAL `agent-shell--read-shell-buffer' preview, passing ARGS along."
    (let* ((buffers (or (plist-get args :buffers) (agent-shell-buffers)))
           (waiting (ads/agent-shell-waiting))
           (ordered (append (seq-intersection buffers waiting)
                            (seq-difference buffers waiting)))
           (plain (symbol-function 'completing-read)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &optional predicate require-match &rest _)
                   ;; The swap is dynamic, so it is still in force inside
                   ;; `consult--read', which calls `completing-read' itself.  Put
                   ;; the real one back first or the two recurse until the stack
                   ;; gives out.
                   (cl-letf (((symbol-function 'completing-read) plain))
                     (ads/agent-shell--read-preview prompt collection predicate
                                                    require-match ordered)))))
        (apply original (plist-put (copy-sequence args) :buffers ordered)))))

  (advice-add 'agent-shell--read-shell-buffer
              :around #'ads/agent-shell-read-with-preview))

(defvar ads/agent-shell-session-file
  (expand-file-name "tmp/agent-shell-sessions.eld" user-emacs-directory)
  "Where the agent shells open at exit are recorded.")

(defun ads/agent-shell--live-sessions ()
  "Every open agent shell as (CWD AGENT SESSION-ID), most recent first.
A shell still negotiating has no session id yet, and nothing to resume."
  (seq-keep
   (lambda (buffer)
     (let ((state (buffer-local-value 'agent-shell--state buffer)))
       (when-let* ((id (map-nested-elt state '(:session :id)))
                   (agent (map-nested-elt state '(:agent-config :identifier))))
         (list (buffer-local-value 'default-directory buffer) agent id))))
   (agent-shell-buffers)))

(defun ads/agent-shell-save-sessions ()
  "Record the open agent shells for `ads/agent-shell-resume-last'.
Runs from `kill-emacs-hook', where an error would hold up the exit."
  (with-demoted-errors "Saving agent shells: %S"
    (when-let* ((sessions (ads/agent-shell--live-sessions)))
      (make-directory (file-name-directory ads/agent-shell-session-file) t)
      (with-temp-file ads/agent-shell-session-file
        (prin1 sessions (current-buffer))))))

(defun ads/agent-shell-resume-last ()
  "Reopen every agent shell that was open when Emacs last exited.
Each one resumes by session id, so its conversation comes back with it.
Shells already open are left alone, as are directories that have since
gone away - a worktree I finished with isn't somewhere to pick up."
  (interactive)
  (unless (file-exists-p ads/agent-shell-session-file)
    (user-error "No shells recorded in %s" ads/agent-shell-session-file))
  (let ((sessions (with-temp-buffer
                    (insert-file-contents ads/agent-shell-session-file)
                    (read (current-buffer))))
        (live (mapcar (lambda (session) (nth 2 session))
                      (ads/agent-shell--live-sessions)))
        (resumed 0))
    (dolist (session (reverse sessions))
      (pcase-let ((`(,cwd ,agent ,id) session))
        (when (and (file-directory-p cwd)
                   (not (member id live)))
          (let ((default-directory cwd))
            (agent-shell--start
             :config (or (agent-shell--resolve-config-designator agent)
                         (user-error "No configuration for agent `%s'" agent))
             :session-id id
             :new-session t
             :no-focus t))
          (setq resumed (1+ resumed)))))
    (message "Resumed %d of %d shell%s" resumed (length sessions)
             (if (= (length sessions) 1) "" "s"))))

(with-eval-after-load 'agent-shell
  (add-hook 'kill-emacs-hook #'ads/agent-shell-save-sessions)
  (ads/leader-keys "aL" 'ads/agent-shell-resume-last))

(defvar ads/agent-shell-transcript-directory "~/.claude/projects/"
  "Where Claude Code keeps its transcripts, a directory per cwd.")

(defvar ads/agent-shell-transcript-ignore '("/\\.cache/" "\\`/\\'")
  "Regexps matching cwds no shell of mine ever sat in.")

(defvar ads/agent-shell-transcript-window 16384
  "How much of a transcript to read from either end.")

(defvar ads/agent-shell-transcript-count 25
  "How many sessions `ads/agent-shell-resume-global-session' offers.")

(defun ads/agent-shell--transcript-records (file beg end)
  "The JSON records of FILE between byte BEG and END, in order.
A byte range starts and ends mid-line, and those two lines don't parse
and drop out, which is all the trimming they need."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file nil beg end))
    (seq-keep (lambda (line)
                (ignore-errors (json-parse-string line :object-type 'alist)))
              (split-string (buffer-string) "\n" t))))

(defun ads/agent-shell--transcript-tail (file)
  "The last records of FILE, newest first.
A window off the end rather than the file, which runs to tens of
megabytes on a long conversation.  A single record longer than the
window leaves nothing parseable, so widen once before giving up."
  (let ((size (file-attribute-size (file-attributes file))))
    (nreverse
     (or (ads/agent-shell--transcript-records
          file (max 0 (- size ads/agent-shell-transcript-window)) size)
         (ads/agent-shell--transcript-records
          file (max 0 (- size (* 64 ads/agent-shell-transcript-window))) size)))))

(defun ads/agent-shell--transcript-title (file)
  "The first thing I said in FILE.
Claude Code titles nothing, so the opening prompt is the title - minus
the tool results and reminders the harness sends under that same `user'
type, which all arrive as a tag."
  (seq-some
   (lambda (record)
     (and (equal "user" (alist-get 'type record))
          (not (alist-get 'isMeta record))
          (when-let* ((content (map-nested-elt record '(message content)))
                      (text (if (stringp content)
                                content
                              (mapconcat (lambda (block) (or (alist-get 'text block) ""))
                                         content " ")))
                      (text (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))
                      ((not (string-empty-p text)))
                      ((not (string-prefix-p "<" text))))
            text)))
   (ads/agent-shell--transcript-records
    file 0 (min (file-attribute-size (file-attributes file))
                ads/agent-shell-transcript-window))))

(defun ads/agent-shell--transcript-session (file)
  "Read FILE into a session, shaped the way `session/list' returns them.
Nil for the cwds nothing of mine sat in and for directories that have
since gone away, on the same grounds `ads/agent-shell-resume-last' skips
them: a worktree I finished with isn't somewhere to pick up."
  (when-let* ((tail (ads/agent-shell--transcript-tail file))
              (updated (seq-some (lambda (record) (alist-get 'timestamp record)) tail))
              (cwd (seq-some (lambda (record) (alist-get 'cwd record)) tail))
              ((not (seq-some (lambda (regexp) (string-match-p regexp cwd))
                              ads/agent-shell-transcript-ignore)))
              ((file-directory-p cwd)))
    (list (cons 'sessionId (file-name-base file))
          (cons 'cwd cwd)
          (cons 'title (or (ads/agent-shell--transcript-title file) "Untitled"))
          (cons 'updatedAt updated))))

(defun ads/agent-shell--global-sessions ()
  "Every session in the transcript store worth resuming, newest first."
  (seq-take (agent-shell--sort-sessions-by-recency
             (seq-keep #'ads/agent-shell--transcript-session
                       (file-expand-wildcards
                        (expand-file-name "*/*.jsonl"
                                          ads/agent-shell-transcript-directory)
                        t)))
            ads/agent-shell-transcript-count))

(defun ads/agent-shell--shell-on-session (id)
  "The shell already open on session ID, if there is one."
  (seq-find (lambda (buffer)
              (equal id (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                                        '(:session :id))))
            (agent-shell-buffers)))

(defun ads/agent-shell-resume-global-session ()
  "Resume a recent session, whichever directory it was running in.
agent-shell's own picker asks the agent for one cwd's sessions, which is
no help when what I've lost is which directories had shells in them.
Read Claude Code's transcripts instead - same columns, every project."
  (interactive)
  (let ((sessions (ads/agent-shell--global-sessions)))
    (unless sessions
      (user-error "No sessions under %s" ads/agent-shell-transcript-directory))
    (let* ((max-widths
            (mapcar (lambda (column)
                      (cons column
                            (apply #'max (mapcar
                                          (lambda (session)
                                            (length (agent-shell--session-column-value
                                                     column session)))
                                          sessions))))
                    (agent-shell--session-selection-columns)))
           (choices (mapcar (lambda (session)
                              (cons (agent-shell--session-choice-label
                                     :acp-session session :max-widths max-widths)
                                    session))
                            sessions))
           (selection (completing-read
                       "Resume session: "
                       (lambda (string predicate action)
                         (if (eq action 'metadata)
                             '(metadata (display-sort-function . identity))
                           (complete-with-action action choices string predicate)))
                       nil t))
           (session (alist-get selection choices nil nil #'equal))
           (id (alist-get 'sessionId session)))
      (if-let* ((buffer (ads/agent-shell--shell-on-session id)))
          (pop-to-buffer buffer)
        (let ((default-directory (alist-get 'cwd session)))
          (agent-shell-resume-session id))))))

(with-eval-after-load 'agent-shell
  (ads/leader-keys "aH" 'ads/agent-shell-resume-global-session))

(defun ads/agent-shell--cwd (directory)
  "Where agent-shell would anchor a session opened in DIRECTORY.
`agent-shell-cwd-function' answers from the buffer it is called in, and
on this machine that can name a different worktree entirely - DIRECTORY
is the instruction, so project detection answers from it alone."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (with-temp-buffer
      (setq default-directory directory)
      (let ((agent-shell-cwd-function nil))
        (agent-shell-cwd)))))

(defun ads/agent-shell-kickoff (prompt &optional directory)
  "Open an agent shell in DIRECTORY and send it PROMPT.
DIRECTORY defaults to `default-directory'.  An agent shell already
running there gets the prompt instead of a second one being started."
  (let* ((cwd (ads/agent-shell--cwd (or directory default-directory)))
         ;; Read off each buffer rather than switching into one: a shell's cwd
         ;; is its `default-directory', and nothing here may rewrite it.
         (shell (or (seq-find (lambda (buffer)
                                (file-equal-p (buffer-local-value 'default-directory buffer)
                                              cwd))
                              (agent-shell-buffers))
                    (with-temp-buffer
                      (setq default-directory cwd)
                      (let ((agent-shell-cwd-function nil))
                        (agent-shell--start
                         :config (or (agent-shell--auto-preferred-config)
                                     (agent-shell-select-config :prompt "Start new agent: ")
                                     (error "No agent config found"))
                         :no-focus t
                         :new-session t
                         :session-strategy 'new))))))
    (agent-shell--display-buffer shell)
    ;; From inside the shell, so the queue resolves to this one rather than to
    ;; the first shell in whatever project point was in.
    (with-current-buffer shell
      (agent-shell-prompt-queue prompt))
    shell))

;;; agent-shell.el ends here
