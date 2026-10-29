;;; review.el --- Review the agent's commits like a pull request  -*- lexical-binding: t; -*-
;;; Commentary:
;; Comments are drafted against diff lines in magit (see review-diff.el), kept
;; here per repository, grouped by commit into one prompt and sent to the
;; agent shell.  This file is the part that does not need magit: state, the
;; comment editor, the prompt, sending.
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup ads/review nil
  "Draft review comments against commit diffs, then send them as one prompt."
  :group 'tools)

(defcustom ads/review-preamble
  "Code review, one section per commit.  Implement each comment below.

Where a comment is a question rather than a change, answer it and leave the
code alone.  Where you think I am wrong, say so and leave the code alone - I
will decide.  Do not touch anything I have not commented on.

Each comment quotes the diff hunk it was written on.  A commit hash that is
no longer on the branch was amended or rebased; find it by its subject."
  "What the agent is told to do with a batch of comments."
  :type 'string
  :group 'ads/review)

(defcustom ads/review-remote "origin"
  "Remote whose commits count as already reviewed.
Whatever has reached it is ready for someone else to review, so only the
commits on HEAD that it does not have are mine to review."
  :type 'string
  :group 'ads/review)

(defcustom ads/review-snippet-max-lines 40
  "Longest diff quote sent verbatim before the middle is elided."
  :type 'integer
  :group 'ads/review)

(defcustom ads/review-editor-height 8
  "Height, in lines, of the comment editor window."
  :type 'integer
  :group 'ads/review)

(defvar ads/review-changed-hook nil
  "Run after a comment is added, edited, held, deleted or sent.")

(defvar ads/review-submitted-hook nil
  "Run with the list of comments that just went to the agent.")

(defface ads/review-region '((t :inherit secondary-selection :extend t))
  "Diff lines carrying a review comment."
  :group 'ads/review)

(defface ads/review-text '((t :inherit (italic font-lock-doc-face)))
  "The comment itself, hanging under the lines it belongs to."
  :group 'ads/review)

(defface ads/review-index '((t :inherit (bold font-lock-keyword-face)))
  "The comment's number, and the rule drawn down its left edge."
  :group 'ads/review)

(defface ads/review-held '((t :inherit shadow))
  "A comment held back from the next submit."
  :group 'ads/review)

(cl-defstruct (ads/review-comment (:constructor ads/review--comment))
  root commit file line diff text held)

(defvar ads/review--comments nil
  "Every comment drafted and not yet sent, across all repositories.")

;;;; Git, without magit

(defun ads/review--git (root &rest args)
  "Lines of `git ARGS' run in ROOT, or nil when it fails."
  (let ((default-directory root))
    (ignore-errors (apply #'process-lines "git" args))))

(defun ads/review--has-remote-p (root)
  "Whether ROOT has `ads/review-remote' to measure against."
  (member ads/review-remote (ads/review--git root "remote")))

(defun ads/review--commits (root)
  "The commits in ROOT not yet pushed to the remote, oldest first, as (HASH . SUBJECT).
With no such remote, nothing is known to be pushed and the answer is empty."
  (when (ads/review--has-remote-p root)
    (mapcar (lambda (line)
              (let ((parts (split-string line "\0")))
                (cons (car parts) (cadr parts))))
            (ads/review--git root "log" "--reverse" "--format=%H%x00%s"
                             "HEAD" "--not" (format "--remotes=%s" ads/review-remote)))))

(defun ads/review--subject (root hash)
  "Subject of HASH in ROOT, or nil when the commit is gone."
  (car (ads/review--git root "show" "-s" "--format=%s" hash)))

;;;; State

(defun ads/review--pending (root)
  "The comments waiting in ROOT."
  (seq-filter (lambda (c) (equal root (ads/review-comment-root c)))
              ads/review--comments))

(defun ads/review--order (root)
  "Commit keys in the order the review reads them: the branch, then the work tree."
  (append (mapcar #'car (ads/review--commits root)) '("worktree")))

(defun ads/review--sorted (root)
  "ROOT's comments by commit, then file, then line."
  (let ((order (ads/review--order root)))
    (cl-flet ((rank (c) (or (seq-position order (ads/review-comment-commit c)) (length order))))
      (sort (copy-sequence (ads/review--pending root))
            (lambda (a b)
              (cond ((/= (rank a) (rank b)) (< (rank a) (rank b)))
                    ((not (equal (ads/review-comment-file a) (ads/review-comment-file b)))
                     (string< (ads/review-comment-file a) (ads/review-comment-file b)))
                    (t (< (or (ads/review-comment-line a) 0)
                          (or (ads/review-comment-line b) 0)))))))))

(defun ads/review--add (comment)
  "Keep COMMENT in the review."
  (cl-pushnew comment ads/review--comments)
  (run-hooks 'ads/review-changed-hook))

(defun ads/review--forget (comments)
  "Drop COMMENTS from the review."
  (setq ads/review--comments (seq-difference ads/review--comments comments))
  (run-hooks 'ads/review-changed-hook))

(defun ads/review--plural (n word)
  "N WORD, pluralised."
  (format "%d %s%s" n word (if (= n 1) "" "s")))

;;;; The editor

(defvar-local ads/review--edit-title nil)
(defvar-local ads/review--edit-callback nil)
(defvar-local ads/review--edit-origin nil
  "(WINDOW . MARKER) to hand focus back to when the editor closes.")

(defconst ads/review--edit-buffer "*review comment*")

(defvar ads/review-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ads/review-edit-finish)
    (define-key map (kbd "C-c C-k") #'ads/review-edit-abort)
    map))

(define-derived-mode ads/review-edit-mode text-mode "Comment"
  "Write one review comment."
  (visual-line-mode 1)
  (setq-local header-line-format
              '(:eval (format " %s   C-c C-c save   C-c C-k cancel"
                              (or ads/review--edit-title "comment")))))

(defun ads/review--open-editor (title text callback)
  "Edit TEXT in a window under the current one; call CALLBACK with the result.
TITLE heads the editor.  The window is an ordinary one split off the window
I was reading in, so it cannot get lost behind another frame, and closing it
puts me back on the line I came from."
  (let ((origin (cons (selected-window) (point-marker)))
        (buffer (get-buffer-create ads/review--edit-buffer)))
    (with-current-buffer buffer
      (ads/review-edit-mode)
      (erase-buffer)
      (insert (or text ""))
      (setq ads/review--edit-title title
            ads/review--edit-callback callback
            ads/review--edit-origin origin))
    (select-window
     (display-buffer buffer `((display-buffer-below-selected)
                              (window-height . ,ads/review-editor-height))))
    (goto-char (point-max))
    (when (fboundp 'evil-insert-state) (evil-insert-state))))

(defun ads/review--close-editor ()
  "Take the editor down and put me back where I was reading."
  (let ((origin ads/review--edit-origin)
        (buffer (current-buffer)))
    (if-let* ((window (get-buffer-window buffer)))
        (quit-window t window)
      (kill-buffer buffer))
    (when-let* ((marker (cdr origin))
                ((buffer-live-p (marker-buffer marker))))
      (if (and (window-live-p (car origin))
               (eq (window-buffer (car origin)) (marker-buffer marker)))
          (select-window (car origin))
        (pop-to-buffer-same-window (marker-buffer marker)))
      (goto-char marker))))

(defun ads/review-edit-finish ()
  "Accept what I wrote."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (callback ads/review--edit-callback))
    (ads/review--close-editor)
    (unless (string-empty-p text)
      (funcall callback text))))

(defun ads/review-edit-abort ()
  "Throw the edit away."
  (interactive)
  (ads/review--close-editor))

;;;; The prompt

(defun ads/review--relative (file root)
  "FILE relative to ROOT.
Both sides go through `file-truename' first: a raw `file-relative-name'
compares strings, so =/private/var/...= against =/var/...= - one directory on
this machine - comes back as =../= nonsense the agent cannot open."
  (if root
      (file-relative-name (file-truename file) (file-truename root))
    file))

(defun ads/review--path (comment root)
  "COMMENT's file as the agent working in ROOT can open it.
The file is stored relative to the repository the comment was made in, so it
is resolved against that, not against whichever buffer sends the review."
  (ads/review--relative (expand-file-name (ads/review-comment-file comment)
                                          (ads/review-comment-root comment))
                        root))

(defun ads/review--snippet (text)
  "TEXT, the middle dropped when it runs long.
The heading carries the file and line, so the agent can read the rest; the
quote only pins the comment to the change I was looking at."
  (let* ((lines (split-string text "\n"))
         (keep (/ ads/review-snippet-max-lines 2)))
    (if (<= (length lines) ads/review-snippet-max-lines)
        text
      (string-join (append (seq-take lines keep)
                           (list (format "... %d lines elided ..."
                                         (- (length lines) (* 2 keep))))
                           (last lines keep))
                   "\n"))))

(defun ads/review--quote (text)
  "TEXT as a markdown blockquote."
  (mapconcat (lambda (line) (concat "> " line)) (split-string text "\n") "\n"))

(defun ads/review--group-title (root key commits)
  "Heading for the comments on KEY, one of COMMITS in ROOT or not."
  (cond ((equal key "worktree") "Uncommitted changes")
        ((assoc key commits)
         (format "%s %s" (substring key 0 (min 8 (length key))) (cdr (assoc key commits))))
        (t (format "%s %s (no longer on the branch)"
                   (substring key 0 (min 8 (length key)))
                   (or (ads/review--subject root key) "unknown subject")))))

(defun ads/review--prompt (comments root)
  "COMMENTS as one review grouped by commit, with paths relative to ROOT."
  (let* ((commits (ads/review--commits root))
         (held (seq-uniq (mapcar (lambda (c) (ads/review--path c root))
                                 (seq-difference (ads/review--pending root) comments))))
         (keys (seq-uniq (mapcar #'ads/review-comment-commit comments)))
         (index 0))
    (concat
     ads/review-preamble
     (when held
       (format "\n\nI am still reviewing %s - leave %s alone until I send comments on %s."
               (string-join held ", ")
               (if (cdr held) "those files" "that file")
               (if (cdr held) "them" "it")))
     "\n\n"
     (cl-loop for key in keys
              concat (format "## %s\n\n" (ads/review--group-title root key commits))
              concat (cl-loop for c in (seq-filter (lambda (c) (equal key (ads/review-comment-commit c)))
                                                   comments)
                              concat (format "### %d  %s:%s\n\n```diff\n%s\n```\n\n%s\n\n"
                                             (cl-incf index)
                                             (ads/review--path c root)
                                             (or (ads/review-comment-line c) "?")
                                             (ads/review--snippet (ads/review-comment-diff c))
                                             (ads/review--quote (ads/review-comment-text c))))))))

;;;; Sending

(defun ads/review--shell ()
  "The agent shell this review belongs to."
  (or (agent-shell--shell-buffer)
      (user-error "No agent shell for this project")))

(defun ads/review--agent-root (shell)
  "The directory SHELL's agent sees, so paths in the prompt are ones it can use."
  (with-current-buffer shell (agent-shell-cwd)))

(defun ads/review--send (comments root)
  "Send COMMENTS, all of them in ROOT, to the agent as one prompt."
  (unless comments (user-error "No comments to send"))
  (let* ((shell (ads/review--shell))
         (prompt (ads/review--prompt comments (or (ads/review--agent-root shell) root)))
         (count (length comments)))
    (if (with-current-buffer shell (shell-maker-busy))
        (with-current-buffer shell (agent-shell-prompt-queue prompt))
      (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer shell))
    (ads/review--forget comments)
    (run-hook-with-args 'ads/review-submitted-hook comments)
    (let ((left (length (ads/review--pending root))))
      (message "Sent %s%s" (ads/review--plural count "comment")
               (if (> left 0) (format ", %d held" left) "")))))

;; The magit side loads on first use, and magit with it.
;; Beside this file rather than under `ads/config-directory': a copy loaded
;; from elsewhere would otherwise go looking in the live tree for its other half.
(let ((diff-file (expand-file-name "review-diff" (file-name-directory
                                                  (file-truename (or load-file-name buffer-file-name))))))
  (dolist (command '(ads/review ads/review-index ads/review-comment ads/review-transient))
    (autoload command diff-file nil t)))

(ads/leader-keys
  "ac" '(ads/review :wk "review")
  "aC" '(ads/review-transient :wk "review menu"))

;;; review.el ends here
