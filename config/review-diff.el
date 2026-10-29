;;; review-diff.el --- The review in magit: commit diffs, comments, an index  -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded once magit is.  Comments hang off the diff lines in a commit's
;; magit buffer (or the work tree's); the index buffer lists the branch's
;; commits with their comments and is the one place to come back to.
;;; Code:

(require 'magit)
(require 'magit-section)
(require 'transient)

(define-fringe-bitmap 'ads/review-marker
  [#b00111000
   #b01111100
   #b11111110
   #b11111110
   #b11111110
   #b01111100
   #b00111000]
  nil nil 'center)

(defvar-local ads/review--root-cache nil)

(defun ads/review--root ()
  "The repository the review at point belongs to.
Cached per buffer: the note-taking hook asks on every command, and asking git
that often is felt."
  (or ads/review--root-cache
      (setq ads/review--root-cache
            (if-let* ((top (magit-toplevel)))
                (file-truename top)
              (user-error "Not in a git repository")))))

;;;; Reading a diff buffer

(defun ads/review--buffer-key ()
  "What this magit buffer shows: a commit hash, \"worktree\", or nil.
Status, and the staged, unstaged and HEAD diffs, all show the work tree: a
comment on an unstaged hunk and one on a staged hunk are both on the
uncommitted change."
  (cond (magit-buffer-revision-oid)
        ((derived-mode-p 'magit-status-mode) "worktree")
        ((and (derived-mode-p 'magit-diff-mode)
              (or (memq magit-buffer-diff-type '(unstaged staged))
                  (and (eq magit-buffer-diff-type 'committed)
                       (equal magit-buffer-diff-range "HEAD"))))
         "worktree")))

(defun ads/review--selection ()
  "The diff lines to comment on: the region, or this line.
On a hunk's heading with no region, the whole hunk.
Returns (FILE LINE DIFF), clamped to one hunk."
  (let* ((beg (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (point)))
         (section (magit-section-at beg)))
    (unless (and section (magit-hunk-section-p section))
      (user-error "Put point on a diff line"))
    (let* ((whole (and (not (use-region-p)) (< beg (oref section content))))
           (first (max (save-excursion (goto-char beg) (line-beginning-position))
                       (oref section content)))
           (last (save-excursion
                   (goto-char (if whole
                                  (1- (oref section end))
                                (min end (1- (oref section end)))))
                   (when (and (not whole) (> end beg) (bolp)) (forward-line -1))
                   (line-end-position)))
           (diff (buffer-substring-no-properties first (max first last))))
      ;; An empty quote would match everywhere, and `ads/review--locate' would
      ;; have nothing to find.
      (when (string-empty-p diff)
        (user-error "Nothing to comment on here"))
      (list (oref (oref section parent) value)
            (save-excursion (goto-char first) (magit-diff-hunk-line section nil))
            diff))))

(defun ads/review--file-sections (section file)
  "Every file section for FILE at or under SECTION.
More than one in a status buffer, where a file can be both staged and unstaged."
  (if (eq (oref section type) 'file)
      (and (equal (oref section value) file) (list section))
    (seq-mapcat (lambda (child) (ads/review--file-sections child file))
                (oref section children))))

(defun ads/review--locate (comment)
  "Buffer (BEG . END) of COMMENT's diff lines in this buffer, or nil."
  (when-let* ((root magit-root-section)
              (diff (ads/review-comment-diff comment))
              ((not (string-empty-p diff))))
    (seq-some
     (lambda (file)
       (save-excursion
         (goto-char (oref file start))
         (let (found)
           (while (and (not found)
                       (search-forward diff (oref file end) t))
             (when (and (save-excursion (goto-char (match-beginning 0)) (bolp))
                        (eolp))
               (setq found (cons (match-beginning 0) (match-end 0)))))
           found)))
     (ads/review--file-sections root (ads/review-comment-file comment)))))

;;;; Drawing comments

(defun ads/review--stepping (command direction)
  "A replacement for COMMAND that never gets stuck on a comment.
The comment hangs off its last line as display rows that all map to the
same buffer position, so vertical motion spends a press on each row before
it moves on.  On that line, step to the neighbouring buffer line instead;
anywhere else in the comment, COMMAND as usual.  DIRECTION is 1 or -1."
  (lambda ()
    (interactive)
    (if-let* ((comment (ads/review--comment-at-point))
              (ov (seq-find (lambda (ov) (eq comment (overlay-get ov 'ads/review-comment)))
                            (overlays-in (line-beginning-position)
                                         (min (point-max) (1+ (line-end-position))))))
              ((= (line-end-position) (overlay-end ov))))
        (let ((count (prefix-numeric-value current-prefix-arg))
              (column (current-column)))
          (forward-line direction)
          (move-to-column column)
          (when (> count 1)
            (let ((current-prefix-arg (1- count)))
              (call-interactively command))))
      (call-interactively command))))

(defvar ads/review-comment-map
  (let ((map (make-sparse-keymap)))
    (dolist (command '(next-line evil-next-line evil-next-visual-line))
      (define-key map (vector 'remap command) (ads/review--stepping command 1)))
    (dolist (command '(previous-line evil-previous-line evil-previous-visual-line))
      (define-key map (vector 'remap command) (ads/review--stepping command -1)))
    (define-key map (kbd "RET") #'ads/review-edit)
    (define-key map "d" #'ads/review-delete)
    (define-key map "h" #'ads/review-hold)
    (define-key map "o" #'ads/review-visit)
    (define-key map "s" #'ads/review-send-one)
    (define-key map "n" #'ads/review-next)
    (define-key map "p" #'ads/review-previous)
    (define-key map "?" #'ads/review-transient)
    map)
  "Keys that act on a comment while point is on the lines it covers.
An overlay's keymap comes before evil's state maps, so these win only
there and leave the same keys alone everywhere else in the diff.")

(defun ads/review--after-string (number comment)
  "COMMENT, drawn as NUMBER, ready to hang under its diff lines."
  (let* ((held (ads/review-comment-held comment))
         (rule (propertize "▏ " 'face 'ads/review-index))
         (tag (propertize (if held "held " (format "%d  " number)) 'face 'ads/review-index))
         (face (if held '(ads/review-held ads/review-text) 'ads/review-text)))
    (concat "\n"
            (cl-loop for line in (split-string (ads/review-comment-text comment) "\n")
                     for lead = tag then (make-string (length tag) ?\s)
                     collect (concat rule lead (propertize line 'face face))
                     into drawn
                     finally return (string-join drawn "\n")))))

(defun ads/review--clear-buffer ()
  "Remove every comment overlay from this buffer."
  (remove-overlays (point-min) (point-max) 'ads/review t))

(defun ads/review--render-buffer ()
  "Redraw this buffer's comments, numbered as the prompt will number them."
  (ads/review--clear-buffer)
  ;; Runs on every magit refresh: no git for a repository with nothing pending,
  ;; and an error here must not take the refresh down with it.
  (when-let* ((key (ads/review--buffer-key))
              (root (ignore-errors (ads/review--root)))
              ((ads/review--pending root)))
    (let ((number 0))
      (dolist (comment (ads/review--sorted root))
        (unless (ads/review-comment-held comment) (cl-incf number))
        (when-let* (((equal key (ads/review-comment-commit comment)))
                    (bounds (ads/review--locate comment))
                    (ov (make-overlay (car bounds) (cdr bounds) nil t nil)))
          (overlay-put ov 'ads/review t)
          (overlay-put ov 'ads/review-comment comment)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'face 'ads/review-region)
          (overlay-put ov 'keymap ads/review-comment-map)
          (overlay-put ov 'before-string
                       (propertize " " 'display
                                   '(left-fringe ads/review-marker ads/review-index)))
          (overlay-put ov 'after-string (ads/review--after-string number comment)))))))

(defun ads/review--comment-at-point ()
  "The comment on this line, if there is one.
Scans the line: an overlay ending at a line's end does not cover the
position the cursor rests on there."
  (seq-some (lambda (ov) (overlay-get ov 'ads/review-comment))
            (overlays-in (line-beginning-position)
                         (min (point-max) (1+ (line-end-position))))))

(defun ads/review--current ()
  "The comment under point, in a diff buffer or in the index."
  (or (ads/review--comment-at-point)
      (when (derived-mode-p 'ads/review-index-mode)
        (magit-section-value-if 'ads-comment))
      (user-error "No comment here")))

;;;; Commands on a comment

(defun ads/review-comment ()
  "Comment on the region, or on the diff line at point."
  (interactive)
  (let ((key (or (ads/review--buffer-key)
                 (user-error "Open a commit, or the working tree, from the review index")))
        (root (ads/review--root)))
    (if-let* ((existing (and (not (use-region-p)) (ads/review--comment-at-point))))
        (ads/review-edit existing)
      (pcase-let ((`(,file ,line ,diff) (ads/review--selection)))
        (deactivate-mark)
        (let ((comment (ads/review--comment :root root :commit key :file file
                                            :line line :diff diff)))
          (ads/review--open-editor
           (format "%s:%s" file line) nil
           (lambda (text)
             (setf (ads/review-comment-text comment) text)
             (ads/review--add comment))))))))

(defun ads/review-edit (&optional comment)
  "Edit COMMENT, or the one at point."
  (interactive)
  (let ((comment (or comment (ads/review--current))))
    (ads/review--open-editor
     (format "%s:%s" (ads/review-comment-file comment) (ads/review-comment-line comment))
     (ads/review-comment-text comment)
     (lambda (text)
       (setf (ads/review-comment-text comment) text)
       (run-hooks 'ads/review-changed-hook)))))

(defun ads/review-delete ()
  "Delete the comment at point."
  (interactive)
  (ads/review--forget (list (ads/review--current)))
  (message "%d left" (length (ads/review--pending (ads/review--root)))))

(defun ads/review-hold ()
  "Hold the comment at point out of the next submit, or release it."
  (interactive)
  (let ((comment (ads/review--current)))
    (setf (ads/review-comment-held comment) (not (ads/review-comment-held comment)))
    (run-hooks 'ads/review-changed-hook)))

(defun ads/review-visit ()
  "Visit the work tree file at the commented line."
  (interactive)
  (ads/review--current)
  (call-interactively #'magit-diff-visit-worktree-file))

(defun ads/review-send-one ()
  "Send only the comment at point."
  (interactive)
  (ads/review--send (list (ads/review--current)) (ads/review--root)))

(defun ads/review-abandon ()
  "Throw the whole review away."
  (interactive)
  (let ((pending (ads/review--pending (ads/review--root))))
    (when (and pending
               (yes-or-no-p (format "Discard %s? " (ads/review--plural (length pending) "comment"))))
      (ads/review--forget pending))))

;;;; Moving

(defun ads/review--open (key)
  "Show the diff for KEY, a commit hash or \"worktree\"."
  (if (equal key "worktree")
      (magit-diff-working-tree "HEAD")
    (magit-show-commit key)))

(defun ads/review--goto (comment)
  "Show COMMENT's diff and put point on it."
  (ads/review--open (ads/review-comment-commit comment))
  (with-current-buffer (magit-get-mode-buffer
                        (if (equal (ads/review-comment-commit comment) "worktree")
                            'magit-diff-mode
                          'magit-revision-mode))
    (ads/review--render-buffer)
    (when-let* ((ov (seq-find (lambda (ov) (eq comment (overlay-get ov 'ads/review-comment)))
                              (overlays-in (point-min) (point-max)))))
      (dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window (overlay-start ov)))
      (goto-char (overlay-start ov)))))

(defun ads/review-next (&optional backward)
  "Go to the next comment in the review, wrapping at the end.
With BACKWARD, go to the previous one."
  (interactive)
  (let* ((all (or (ads/review--sorted (ads/review--root)) (user-error "No comments")))
         (pool (if backward (reverse all) all))
         (here (or (ignore-errors (ads/review--current)) nil))
         (target (or (cadr (memq here pool)) (car pool))))
    (ads/review--goto target)
    (when-let* ((window (get-buffer-window (current-buffer))))
      (select-window window))))

(defun ads/review-previous ()
  "Go to the previous comment in the review."
  (interactive)
  (ads/review-next t))

;;;; Submitting

(defun ads/review-submit ()
  "Send every comment that is not held, grouped by commit."
  (interactive)
  (let ((root (ads/review--root)))
    (ads/review--send
     (seq-remove #'ads/review-comment-held (ads/review--sorted root))
     root)))

(defun ads/review--section-key ()
  "The commit the index or diff buffer at point is about."
  (or (magit-section-value-if 'ads-commit)
      (ads/review--buffer-key)
      (when-let* ((c (ignore-errors (ads/review--current))))
        (ads/review-comment-commit c))
      (user-error "No commit here")))

(defun ads/review-submit-commit ()
  "Send the comments on this commit only and keep the rest."
  (interactive)
  (let ((root (ads/review--root))
        (key (ads/review--section-key)))
    (ads/review--send
     (seq-filter (lambda (c) (and (equal key (ads/review-comment-commit c))
                                  (not (ads/review-comment-held c))))
                 (ads/review--sorted root))
     root)))

;;;; The minor mode on magit's diff buffers

(defvar ads/review--last nil
  "Alist of (ROOT . BUFFER): the review buffer I was last in.")

(defvar ads/review--last-diff nil
  "Alist of (ROOT . BUFFER): the diff buffer I was last in.")

(defun ads/review--note-buffer ()
  "Remember this buffer as where the review was left."
  (when-let* ((root (ignore-errors (ads/review--root))))
    (setf (alist-get root ads/review--last nil nil #'equal) (current-buffer))
    (unless (derived-mode-p 'ads/review-index-mode)
      (setf (alist-get root ads/review--last-diff nil nil #'equal) (current-buffer)))))

(defvar ads/review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") #'ads/review-comment)
    map))

(define-minor-mode ads/review-mode
  "Comment on a commit's diff lines.  Lives on magit's status, diff and revision buffers."
  :keymap ads/review-mode-map
  (if ads/review-mode
      (progn (add-hook 'magit-refresh-buffer-hook #'ads/review--render-buffer nil t)
             (add-hook 'post-command-hook #'ads/review--note-buffer nil t)
             (ads/review--render-buffer))
    (remove-hook 'magit-refresh-buffer-hook #'ads/review--render-buffer t)
    (remove-hook 'post-command-hook #'ads/review--note-buffer t)
    (ads/review--clear-buffer)))

(add-hook 'magit-diff-mode-hook #'ads/review-mode)
(add-hook 'magit-status-mode-hook #'ads/review-mode)

;;;; The index

(defvar-local ads/review--index-root nil)

(defvar ads/review-index-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'ads/review-index-visit)
    (define-key map "e" #'ads/review-edit)
    (define-key map "d" #'ads/review-delete)
    (define-key map "h" #'ads/review-hold)
    (define-key map "s" #'ads/review-submit)
    (define-key map "S" #'ads/review-submit-commit)
    (define-key map "g" #'ads/review-index-refresh)
    (define-key map "?" #'ads/review-transient)
    map))

(define-derived-mode ads/review-index-mode magit-section-mode "Review"
  "The branch's commits, and the comments waiting on each.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'ads/review-index-mode 'motion)
  (dolist (state '(motion normal))
    (evil-define-key* state ads/review-index-mode-map
      (kbd "RET") #'ads/review-index-visit
      "e" #'ads/review-edit
      "d" #'ads/review-delete
      "h" #'ads/review-hold
      "s" #'ads/review-submit
      "S" #'ads/review-submit-commit
      "g" #'ads/review-index-refresh
      "?" #'ads/review-transient)))

(defun ads/review--index-insert-comments (comments)
  "One line per comment in COMMENTS."
  (dolist (comment comments)
    (magit-insert-section (ads-comment comment)
      (insert (propertize (format "  %s:%s  " (ads/review-comment-file comment)
                                  (or (ads/review-comment-line comment) "?"))
                          'face 'magit-filename)
              (propertize (car (split-string (ads/review-comment-text comment) "\n"))
                          'face (if (ads/review-comment-held comment)
                                    'ads/review-held 'ads/review-text))
              (if (ads/review-comment-held comment) " (held)" "")
              "\n"))))

(defun ads/review--index-insert-group (key title comments)
  "A section for KEY, headed TITLE, listing COMMENTS."
  (magit-insert-section (ads-commit key)
    (magit-insert-heading
      (propertize title 'face 'magit-section-secondary-heading)
      (if comments
          (propertize (format "  %s" (ads/review--plural (length comments) "comment"))
                      'face 'ads/review-index)
        ""))
    (ads/review--index-insert-comments comments)))

(defun ads/review-index-refresh ()
  "Redraw the index from git and the pending comments."
  (interactive)
  (let* ((root ads/review--index-root)
         (inhibit-read-only t)
         (commits (ads/review--commits root))
         (sorted (ads/review--sorted root))
         (known (append (mapcar #'car commits) '("worktree"))))
    (erase-buffer)
    (magit-insert-section (review)
      (magit-insert-heading
        (format "Review of %s, not yet pushed to %s   %s"
                (file-name-nondirectory (directory-file-name root))
                (if (ads/review--has-remote-p root) ads/review-remote "a remote (none found)")
                (ads/review--plural (length sorted) "comment")))
      (dolist (commit commits)
        (ads/review--index-insert-group
         (car commit)
         (format "%s %s" (substring (car commit) 0 8) (cdr commit))
         (seq-filter (lambda (c) (equal (car commit) (ads/review-comment-commit c))) sorted)))
      (ads/review--index-insert-group
       "worktree" "Uncommitted changes"
       (seq-filter (lambda (c) (equal "worktree" (ads/review-comment-commit c))) sorted))
      (when-let* ((orphans (seq-remove (lambda (c) (member (ads/review-comment-commit c) known))
                                       sorted)))
        (ads/review--index-insert-group
         "orphaned" "No longer on the branch" orphans)))))

(defun ads/review-index ()
  "Show the review index for this repository."
  (interactive)
  (let* ((root (ads/review--root))
         (buffer (get-buffer-create
                  (format "*review: %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ads/review-index-mode)
        (ads/review-index-mode)
        (add-hook 'post-command-hook #'ads/review--note-buffer nil t))
      (setq ads/review--index-root root
            default-directory root)
      (ads/review-index-refresh))
    (pop-to-buffer-same-window buffer)
    (setf (alist-get root ads/review--last nil nil #'equal) buffer)))

(defun ads/review-index-visit ()
  "Open the commit, or the comment, at point."
  (interactive)
  (cond ((magit-section-value-if 'ads-comment)
         (ads/review--goto (magit-section-value-if 'ads-comment)))
        ((magit-section-value-if 'ads-commit)
         (let ((key (magit-section-value-if 'ads-commit)))
           (unless (equal key "orphaned") (ads/review--open key))))
        (t (user-error "Nothing to open here"))))

(defun ads/review--redraw ()
  "Redraw every review buffer after a change."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond (ads/review-mode (ads/review--render-buffer))
            ((derived-mode-p 'ads/review-index-mode)
             (let ((line (line-number-at-pos)))
               (ads/review-index-refresh)
               (goto-char (point-min))
               (forward-line (1- line))))))))

(add-hook 'ads/review-changed-hook #'ads/review--redraw)

;;;; Getting back

(defun ads/review ()
  "Open the review, or go back to where I left it.
Inside a review buffer this swaps between the index and the last diff."
  (interactive)
  (let* ((root (ads/review--root))
         (last (alist-get root ads/review--last nil nil #'equal))
         (diff (alist-get root ads/review--last-diff nil nil #'equal)))
    (cond ((derived-mode-p 'ads/review-index-mode)
           (if (buffer-live-p diff)
               (pop-to-buffer-same-window diff)
             (user-error "No diff to go back to")))
          (ads/review-mode (ads/review-index))
          ((buffer-live-p last) (pop-to-buffer-same-window last))
          (t (ads/review-index)))))

(defun ads/review--heading ()
  "Transient title, carrying the count so I do not have to ask."
  (format "Review — %s pending"
          (if-let* ((root (ignore-errors (ads/review--root))))
              (length (ads/review--pending root))
            0)))

(transient-define-prefix ads/review-transient ()
  "Review the agent's commits the way I would review a pull request."
  [:description ads/review--heading
   ["Draft"
    ("c" "comment on region" ads/review-comment)
    ("e" "edit" ads/review-edit)
    ("d" "delete" ads/review-delete)
    ("h" "hold / release" ads/review-hold)
    ("a" "abandon review" ads/review-abandon)]
   ["Move"
    ("n" "next" ads/review-next :transient t)
    ("p" "previous" ads/review-previous :transient t)
    ("i" "index" ads/review-index)
    ("o" "visit file" ads/review-visit)]
   ["Send"
    ("s" "submit review" ads/review-submit)
    ("C" "submit this commit" ads/review-submit-commit)
    ("1" "send this comment" ads/review-send-one)
    ("g" "compose a free prompt" agent-shell-prompt-compose)]])

;; Buffers magit made before this file loaded (or reloaded) missed the hook.
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'magit-diff-mode 'magit-status-mode)
      (ads/review-mode 1))))

;;; review-diff.el ends here
