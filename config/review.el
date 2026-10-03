;;; review.el --- Drafting review comments and sending them as one prompt  -*- lexical-binding: t; -*-
;;; Commentary:
;; review comments, entries, drawing them, writing one, the commands,
;; sending the review, the transient
;;; Code:

(require 'transient)

(defgroup ads/review nil
  "Draft review comments against a buffer, then send them as one prompt."
  :group 'tools)

(defcustom ads/review-preamble
  "Code review.  Implement each comment below.

Where a comment is a question rather than a change, answer it and leave the
code alone.  Where you think I am wrong, say so and leave the code alone - I
will decide.  Do not touch anything I have not commented on."
  "What the agent is told to do with a batch of comments."
  :type 'string
  :group 'ads/review)

(defcustom ads/review-snippet-max-lines 40
  "Longest code quote sent verbatim before the middle is elided."
  :type 'integer
  :group 'ads/review)

(defcustom ads/review-use-child-frame t
  "Whether the comment editor opens in a child frame at the region.
When nil, or on a terminal, it opens in a window below the current one."
  :type 'boolean
  :group 'ads/review)

(defcustom ads/review-editor-size '(72 . 5)
  "Minimum (WIDTH . HEIGHT), in characters, of the comment editor."
  :type '(cons integer integer)
  :group 'ads/review)

(defvar ads/review-submitted-hook nil
  "Run with the list of entries that just went to the agent.")

(defface ads/review-region '((t :inherit secondary-selection :extend t))
  "A region carrying a review comment."
  :group 'ads/review)

(defface ads/review-text '((t :inherit (italic font-lock-doc-face)))
  "The comment itself, hanging under the region it belongs to."
  :group 'ads/review)

(defface ads/review-index '((t :inherit (bold font-lock-keyword-face)))
  "The comment's number, and the rule drawn down its left edge."
  :group 'ads/review)

(define-fringe-bitmap 'ads/review-marker
  [#b00111000
   #b01111100
   #b11111110
   #b11111110
   #b11111110
   #b01111100
   #b00111000]
  nil nil 'center)

(cl-defstruct (ads/review-entry (:constructor ads/review--entry))
  overlay file line-start line-end snippet text)

(defvar ads/review--entries nil
  "Every comment drafted and not yet sent.")

(defun ads/review--sync (entry)
  "Re-read ENTRY's line range and code from its overlay.
A no-op once the overlay's buffer is gone, which leaves the last values
read standing as the snapshot."
  (when-let* ((ov (ads/review-entry-overlay entry))
              (buffer (overlay-buffer ov)))
    (with-current-buffer buffer
      (let ((beg (overlay-start ov))
            (end (overlay-end ov)))
        (setf (ads/review-entry-line-start entry) (line-number-at-pos beg t)
              (ads/review-entry-line-end entry) (line-number-at-pos end t)
              (ads/review-entry-snippet entry) (buffer-substring-no-properties beg end)))))
  entry)

(defun ads/review--sorted ()
  "Every entry in the order a reviewer reads them: by file, then by line."
  (mapc #'ads/review--sync ads/review--entries)
  (sort (copy-sequence ads/review--entries)
        (lambda (a b)
          (let ((fa (ads/review-entry-file a))
                (fb (ads/review-entry-file b)))
            (if (equal fa fb)
                (< (ads/review-entry-line-start a) (ads/review-entry-line-start b))
              (string< fa fb))))))

(defun ads/review--forget (entry)
  "Drop ENTRY and unhang its overlay."
  (when-let* ((ov (ads/review-entry-overlay entry)))
    (delete-overlay ov))
  (setq ads/review--entries (delq entry ads/review--entries)))

(defun ads/review--entry-at-point ()
  "The comment on this line, if there is one.
Scans the line rather than point because an overlay snapped to the end of
a line does not cover the position the cursor rests on there."
  (seq-some (lambda (ov) (overlay-get ov 'ads/review-entry))
            (overlays-in (line-beginning-position)
                         (min (point-max) (1+ (line-end-position))))))

(defun ads/review--after-string (index entry)
  "ENTRY's comment, drawn as INDEX, ready to hang off its overlay."
  (let* ((ov (ads/review-entry-overlay entry))
         (pad (propertize
               (make-string (with-current-buffer (overlay-buffer ov)
                              (save-excursion
                                (goto-char (overlay-start ov))
                                (current-indentation)))
                            ?\s)
               'face 'default))
         (rule (propertize "▏ " 'face 'ads/review-index))
         (lines (split-string (ads/review-entry-text entry) "\n")))
    (concat "\n"
            (cl-loop for line in lines
                     for tag = (propertize (format "%d  " index) 'face 'ads/review-index)
                     then (make-string (length tag) ?\s)
                     collect (concat pad rule tag
                                     (propertize line 'face 'ads/review-text))
                     into drawn
                     finally return (string-join drawn "\n")))))

(defun ads/review--render ()
  "Redraw every comment, numbered as the prompt will number them."
  (let ((index 0))
    (dolist (entry (ads/review--sorted))
      (cl-incf index)
      (when-let* ((ov (ads/review-entry-overlay entry))
                  ((overlay-buffer ov)))
        (overlay-put ov 'ads/review-entry entry)
        (overlay-put ov 'face 'ads/review-region)
        (overlay-put ov 'before-string
                     (propertize " " 'display
                                 '(left-fringe ads/review-marker ads/review-index)))
        (overlay-put ov 'after-string (ads/review--after-string index entry))))))

(defun ads/review--line-pos (line &optional end)
  "Buffer position at the start of LINE, or at its END."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (if end (line-end-position) (line-beginning-position))))

(defun ads/review--restore ()
  "Re-hang this file's comments when it comes back into a buffer."
  (when buffer-file-name
    (let ((file (file-truename buffer-file-name))
          (restored nil))
      (dolist (entry ads/review--entries)
        (when (and (equal file (ads/review-entry-file entry))
                   (not (overlay-buffer (ads/review-entry-overlay entry))))
          (setf (ads/review-entry-overlay entry)
                (make-overlay (ads/review--line-pos (ads/review-entry-line-start entry))
                              (ads/review--line-pos (ads/review-entry-line-end entry) t)))
          (setq restored t)))
      (when restored (ads/review--render)))))

(add-hook 'find-file-hook #'ads/review--restore)

(defvar ads/review--edit-entry nil
  "The entry the comment editor is open on.")

(defvar ads/review--edit-return nil
  "The frame to hand focus back to when the editor closes.")

(defconst ads/review--edit-buffer " *review comment*")

(defvar ads/review-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ads/review-edit-finish)
    (define-key map (kbd "C-c C-k") #'ads/review-edit-abort)
    map))

(define-derived-mode ads/review-edit-mode text-mode "Comment"
  "Write one review comment."
  ;; A leading space in the buffer name keeps it out of the buffer list and
  ;; also turns undo off; I want the former, not the latter.
  (buffer-enable-undo)
  (visual-line-mode 1)
  (setq-local header-line-format
              '(:eval (when ads/review--edit-entry
                        (format " %s:%d-%d   C-c C-c send   C-c C-k cancel"
                                (file-name-nondirectory
                                 (ads/review-entry-file ads/review--edit-entry))
                                (ads/review-entry-line-start ads/review--edit-entry)
                                (ads/review-entry-line-end ads/review--edit-entry))))))

(defun ads/review--child-frame-p ()
  "Whether the editor should open in a child frame here."
  (and ads/review-use-child-frame
       (display-graphic-p)
       (require 'posframe nil t)))

(defun ads/review--bounds ()
  "The whole-line region to comment on: the active region, or this line."
  (let ((beg (if (use-region-p) (region-beginning) (point)))
        (end (if (use-region-p) (region-end) (point))))
    (cons (save-excursion (goto-char beg) (line-beginning-position))
          (save-excursion
            (goto-char end)
            (when (and (> end beg) (bolp)) (forward-line -1))
            (line-end-position)))))

(defun ads/review--edit (entry)
  "Open the comment editor on ENTRY, at the region it covers."
  (let ((buffer (get-buffer-create ads/review--edit-buffer)))
    (setq ads/review--edit-entry entry
          ads/review--edit-return (selected-frame))
    (with-current-buffer buffer
      (ads/review-edit-mode)
      (erase-buffer)
      (insert (or (ads/review-entry-text entry) "")))
    (if (ads/review--child-frame-p)
        (progn
          (posframe-show buffer
                         :position (overlay-end (ads/review-entry-overlay entry))
                         :poshandler #'posframe-poshandler-point-bottom-left-corner
                         :min-width (car ads/review-editor-size)
                         :min-height (cdr ads/review-editor-size)
                         :internal-border-width 2
                         :internal-border-color (face-foreground 'ads/review-index nil t)
                         :accept-focus t
                         :hidehandler nil)
          (when-let* ((child (buffer-local-value 'posframe--frame buffer)))
            (select-frame-set-input-focus child)))
      (pop-to-buffer buffer '((display-buffer-below-selected)
                              (window-height . 8))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (fboundp 'evil-insert-state) (evil-insert-state)))))

(defun ads/review--close-editor ()
  "Take the editor down and put me back on the code I was commenting on.
Returning to the source explicitly rather than leaving it to `quit-window':
in the child-frame case there is no window to quit, and in both cases the
next thing I do is keep reading."
  (let ((source (when-let* ((entry ads/review--edit-entry))
                  (overlay-buffer (ads/review-entry-overlay entry))))
        (point (when-let* ((entry ads/review--edit-entry))
                 (overlay-start (ads/review-entry-overlay entry)))))
    (when-let* ((buffer (get-buffer ads/review--edit-buffer)))
      (if (ads/review--child-frame-p)
          (posframe-delete buffer)
        (when-let* ((window (get-buffer-window buffer)))
          (quit-window nil window))))
    (setq ads/review--edit-entry nil)
    (when (frame-live-p ads/review--edit-return)
      (select-frame-set-input-focus ads/review--edit-return))
    (when (buffer-live-p source)
      (pop-to-buffer-same-window source)
      (goto-char point))))

(defun ads/review-edit-finish ()
  "Accept what I wrote and hang it on the region."
  (interactive)
  ;; Read both out before closing: `posframe-delete' kills this buffer.
  (let ((entry ads/review--edit-entry)
        (text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p text)
        (ads/review-edit-abort)
      (setf (ads/review-entry-text entry) text)
      (ads/review--sync entry)
      (cl-pushnew entry ads/review--entries)
      (ads/review--close-editor)
      (ads/review--render)
      (message "%d comment%s pending"
               (length ads/review--entries)
               (if (= 1 (length ads/review--entries)) "" "s")))))

(defun ads/review-edit-abort ()
  "Throw the comment away.  A region that never got one loses its overlay."
  (interactive)
  (let ((entry ads/review--edit-entry))
    (ads/review--close-editor)
    (unless (memq entry ads/review--entries)
      (delete-overlay (ads/review-entry-overlay entry)))))

(defun ads/review-comment ()
  "Comment on the region, or on the line at point."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (if-let* ((existing (and (not (use-region-p)) (ads/review--entry-at-point))))
      (ads/review--edit existing)
    (let ((bounds (ads/review--bounds)))
      (deactivate-mark)
      ;; Truename, so a file reached through a symlink is the same file as one
      ;; reached directly - that identity is what `ads/review-submit-file' and
      ;; `ads/review--restore' match on.
      (ads/review--edit (ads/review--entry
                         :file (file-truename buffer-file-name)
                         :overlay (make-overlay (car bounds) (cdr bounds)))))))

(defun ads/review-delete ()
  "Delete the comment on this line."
  (interactive)
  (if-let* ((entry (ads/review--entry-at-point)))
      (progn (ads/review--forget entry)
             (ads/review--render)
             (message "%d left" (length ads/review--entries)))
    (user-error "No comment here")))

(defun ads/review--goto (entry)
  "Show ENTRY, reopening its file if the buffer went away."
  (let ((ov (ads/review-entry-overlay entry)))
    (if (overlay-buffer ov)
        (progn (pop-to-buffer-same-window (overlay-buffer ov))
               (goto-char (overlay-start ov)))
      (find-file (ads/review-entry-file entry))
      (goto-char (ads/review--line-pos (ads/review-entry-line-start entry))))))

(defun ads/review-next (&optional backward)
  "Go to the next comment in the review, wrapping at the end.
With BACKWARD, go to the previous one."
  (interactive)
  (let* ((entries (or (ads/review--sorted) (user-error "No comments")))
         (pool (if backward (reverse entries) entries))
         (rest (cdr (memq (ads/review--entry-at-point) pool))))
    (ads/review--goto (or (car rest) (car pool)))))

(defun ads/review-previous ()
  "Go to the previous comment in the review."
  (interactive)
  (ads/review-next t))

(defun ads/review--label (index entry root)
  "One line describing ENTRY as comment INDEX, for the picker."
  (format "%2d  %s:%d  %s" index
          (ads/review--relative (ads/review-entry-file entry) root)
          (ads/review-entry-line-start entry)
          (car (split-string (ads/review-entry-text entry) "\n"))))

(defun ads/review-list ()
  "Pick a comment and go to it."
  (interactive)
  (let* ((entries (or (ads/review--sorted) (user-error "No comments")))
         (root (or (ads/review--root) default-directory))
         (choices (cl-loop for entry in entries
                           for index from 1
                           collect (cons (ads/review--label index entry root) entry))))
    (ads/review--goto
     (cdr (assoc (completing-read "Comment: " choices nil t) choices)))))

(defun ads/review-abandon ()
  "Throw the whole review away."
  (interactive)
  (when (and ads/review--entries
             (yes-or-no-p (format "Discard %d comment%s? "
                                  (length ads/review--entries)
                                  (if (= 1 (length ads/review--entries)) "" "s"))))
    (mapc #'ads/review--forget (copy-sequence ads/review--entries))))

(defun ads/review--shell ()
  "The agent shell this review belongs to."
  (or (agent-shell--shell-buffer)
      (user-error "No agent shell for this project")))

(defun ads/review--root (&optional shell)
  "The directory the agent sees, so paths in the prompt are ones it can use."
  (when-let* ((shell (or shell (agent-shell--shell-buffer :no-error t :no-create t))))
    (with-current-buffer shell (agent-shell-cwd))))

(defun ads/review--relative (file root)
  "FILE relative to ROOT, with symlinks resolved on both sides first.
A raw `file-relative-name' compares the strings, so a buffer visiting
=/private/var/...= and an agent cwd of =/var/...= - the same directory on
this machine - come back as =../= nonsense the agent cannot open."
  (if root
      (file-relative-name (file-truename file) (file-truename root))
    file))

(defun ads/review--language (entry)
  "The fence tag for ENTRY, from the major mode of the buffer it came from."
  (if-let* ((ov (ads/review-entry-overlay entry))
            (buffer (overlay-buffer ov)))
      (replace-regexp-in-string "\\(-ts\\)?-mode\\'" ""
                                (symbol-name (buffer-local-value 'major-mode buffer)))
    ""))

(defun ads/review--snippet (entry)
  "ENTRY's code, with the middle dropped when it runs long.
The heading carries the line range, so the agent can read the rest from the
file; what the quote is for is pinning the comment to code that has not moved."
  (let* ((text (ads/review-entry-snippet entry))
         (lines (split-string text "\n"))
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

(defun ads/review--holding (going-out root)
  "Files still carrying comments once GOING-OUT has been sent, under ROOT."
  (mapcar (lambda (file) (ads/review--relative file root))
          (seq-uniq (mapcar #'ads/review-entry-file
                            (seq-difference ads/review--entries going-out)))))

(defun ads/review--prompt (entries root)
  "ENTRIES as one review, with paths relative to ROOT."
  (let ((holding (ads/review--holding entries root)))
    (concat
     ads/review-preamble
     (when holding
       (format "\n\nI am still reviewing %s - leave %s alone until I send comments on %s."
               (string-join holding ", ")
               (if (cdr holding) "those files" "that file")
               (if (cdr holding) "them" "it")))
     "\n\n"
     (cl-loop for entry in entries
              for index from 1
              concat (format "### %d  %s:%d-%d\n\n```%s\n%s\n```\n\n%s\n\n"
                             index
                             (ads/review--relative (ads/review-entry-file entry) root)
                             (ads/review-entry-line-start entry)
                             (ads/review-entry-line-end entry)
                             (ads/review--language entry)
                             (ads/review--snippet entry)
                             (ads/review--quote (ads/review-entry-text entry)))))))

(defun ads/review-submit (&optional this-file)
  "Send the review to the agent as one prompt.
With THIS-FILE, send only the comments on the current file and keep the
rest."
  (interactive "P")
  (unless ads/review--entries (user-error "No comments to send"))
  (let* ((all (ads/review--sorted))
         (entries (if this-file
                      (let ((file (file-truename (or buffer-file-name ""))))
                        (seq-filter (lambda (e) (equal file (ads/review-entry-file e))) all))
                    all))
         (shell (ads/review--shell))
         (root (ads/review--root shell))
         ;; Built before anything is forgotten: the held list is the difference
         ;; between what is pending and what is going out.
         (prompt (ads/review--prompt entries root))
         (count (length entries)))
    (unless entries (user-error "No comments on this file"))
    (if (with-current-buffer shell (shell-maker-busy))
        (with-current-buffer shell (agent-shell-prompt-queue prompt))
      (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer shell))
    (mapc #'ads/review--forget entries)
    (ads/review--render)
    (run-hook-with-args 'ads/review-submitted-hook entries)
    (message "Sent %d comment%s%s" count (if (= 1 count) "" "s")
             (if ads/review--entries
                 (format ", %d held" (length ads/review--entries))
               ""))))

(defun ads/review-submit-file ()
  "Send only this file's comments and keep the rest."
  (interactive)
  (ads/review-submit t))

(defun ads/review--heading ()
  "Transient title, carrying the count so I do not have to ask."
  (format "Review — %d pending" (length ads/review--entries)))

(transient-define-prefix ads/review-transient ()
  "Review the agent's code the way I would review a pull request."
  [:description ads/review--heading
   ["Draft"
    ("c" "comment on region" ads/review-comment)
    ("d" "delete this one" ads/review-delete)
    ("a" "abandon review" ads/review-abandon)]
   ["Move"
    ("n" "next" ads/review-next :transient t)
    ("p" "previous" ads/review-previous :transient t)
    ("l" "list" ads/review-list)]
   ["Send"
    ("s" "submit review" ads/review-submit)
    ("f" "submit this file only" ads/review-submit-file)
    ("C" "compose a free prompt" agent-shell-prompt-compose)]])

(ads/leader-keys "ac" '(ads/review-transient :wk "review"))

;;; review.el ends here
