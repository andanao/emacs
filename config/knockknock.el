;;; knockknock.el --- knockknock notifications  -*- lexical-binding: t; -*-
;;; Commentary:
;; knockknock
;;; Code:

(require 'cl-lib)

(defconst ads/notify--inset 16 "Gap between a popup and the frame edge.")
(defconst ads/notify--gap 8 "Gap between stacked popups.")

(cl-defstruct (ads/notify-entry (:constructor ads/notify--entry))
  id args action style buffers)

(defvar ads/notify--stack nil
  "Live notifications, oldest first.")

(defvar ads/notify--counter 0
  "Source of notification ids.")

(defvar ads/notify--y nil
  "Top edge of the popup being shown, bound by `ads/notify--show'.")

(defun ads/notify--frames ()
  "Real visible graphical frames - not posframe's own children."
  (seq-filter (lambda (f)
                (and (display-graphic-p f)
                     (frame-visible-p f)
                     (not (frame-parameter f 'parent-frame))))
              (frame-list)))

(defun ads/notify--frame-id (frame)
  "A stable short id for FRAME, made on first sight."
  (or (frame-parameter frame 'ads/notify-id)
      (let ((id (format "%04x" (random 65536))))
        (set-frame-parameter frame 'ads/notify-id id)
        id)))

(defun ads/notify--poshandler (info)
  "Right edge of the parent frame at `ads/notify--y', clamped to the monitor."
  (let* ((parent (plist-get info :parent-frame))
         (width (plist-get info :posframe-width))
         (x (max 0 (- (plist-get info :parent-frame-width) width ads/notify--inset)))
         (area (frame-monitor-workarea parent))
         (overhang (- (+ (car (frame-edges parent 'absolute)) x width ads/notify--inset)
                      (+ (nth 0 area) (nth 2 area)))))
    (cons (max 0 (if (> overhang 0) (- x overhang) x))
          (or ads/notify--y ads/notify--inset))))

(defvar ads/notify--bg nil
  "Background to paint into the SVG, bound by `ads/notify--show'.")

(defun ads/notify--svg-background (svg)
  "Fill the whole canvas of SVG with `ads/notify--bg'.
knockknock's SVG is icon and text over nothing, so every surface the image
does not cover shows through: the fringes, which take the `fringe' face
rather than the frame's background-color, and the part of a character cell
`fit-frame-to-buffer' rounds up to.  The rect goes in first, so it sits
behind everything else.  A no-op for every other SVG, since nothing else
binds the variable."
  (when (and ads/notify--bg (dom-attr svg 'width))
    (svg-rectangle svg 0 0 (dom-attr svg 'width) (dom-attr svg 'height)
                   :fill ads/notify--bg))
  svg)

(advice-add 'svg-create :filter-return #'ads/notify--svg-background)

(defun ads/notify--cache-key-colours (key)
  "Put the colours baked into the image onto knockknock's cache KEY.
Upstream keys on (title message icon icon-file) alone, so two sources
sharing text and icon would serve each other's colours."
  (append key (list (face-foreground 'knockknock-title-face nil t)
                    (face-foreground 'knockknock-message-face nil t)
                    (face-foreground 'knockknock-icon-face nil t)
                    ads/notify--bg)))

(advice-add 'knockknock--cache-key :filter-return #'ads/notify--cache-key-colours)

(defun ads/notify--literal-icon (orig name)
  "Pass a single-character NAME straight through; ORIG resolves real names.
My agenda categories start with the nerd-font glyph itself rather than an
icon name, and upstream would try to look the glyph up and find nothing."
  (if (and (stringp name) (= (length name) 1))
      name
    (funcall orig name)))

(advice-add 'knockknock--get-icon :around #'ads/notify--literal-icon)

(defvar ads/notify-icon-dir (expand-file-name "knockknock-icons/" user-emacs-directory)
  "Where recoloured copies of the agent icons live.")

(defun ads/notify--tint-icon (file colour)
  "A copy of FILE flat-filled with COLOUR, generated once and cached on disk.
The agent logos are monochrome glyphs on transparency, shipped in a light
and a dark variant, and neither is the theme's foreground.  `svg-embed'
puts a PNG in untinted, so the recolour has to happen before knockknock
ever sees the file."
  (if (not (and file colour (executable-find "magick")))
      file
    (make-directory ads/notify-icon-dir t)
    (let ((out (expand-file-name
                (format "%s-%s.png" (file-name-base file)
                        (string-remove-prefix "#" colour))
                ads/notify-icon-dir)))
      (unless (file-exists-p out)
        (call-process "magick" nil nil nil file
                      "-channel" "RGB" "-fill" colour "-colorize" "100" out))
      (if (file-exists-p out) out file))))

(defun ads/notify--text-px (text px bold)
  "Roughly how wide TEXT is when the SVG draws it at PX pixels.
Whichever is larger of `string-pixel-width' in Emacs' own font, scaled to
PX through `:height' in tenths of a point, and a generous count per
character.  librsvg's font is not Emacs' and there is no asking it what it
will do, so over-padding is the safe error; the eight-per-character guess
this replaces under-padded and cut the title off.  The floor also covers a
frame with no font metrics to measure against."
  (if (or (null text) (string-empty-p text))
      0
    (max (ceiling (* 1.1 (string-pixel-width
                          (propertize text 'face `(:height ,(round (* px 7.5))
                                                   :weight ,(if bold 'bold 'normal))))))
         (* (length text) (if bold 10 7)))))

(defun ads/notify--svg-width (orig title message icon &optional icon-file progress)
  "Widen the canvas to fit TITLE before ORIG draws it.
knockknock sizes the canvas at eight pixels per title character, which is
short for bold 16px and cuts a long heading off at the right edge.
`knockknock-svg-min-width' is the only lever that reaches the calculation
from outside, so measure the text and raise it - capped at the max width,
which the clamp inside would not do for the minimum."
  (let* ((needed (+ knockknock-svg-icon-size knockknock-svg-padding
                    knockknock-left-padding knockknock-right-padding
                    (max (ads/notify--text-px title 16 t)
                         (ads/notify--text-px message 12 nil))))
         (knockknock-svg-min-width (max knockknock-svg-min-width
                                        (min knockknock-svg-max-width needed))))
    (funcall orig title message icon icon-file progress)))

(advice-add 'knockknock--format-buffer-svg :around #'ads/notify--svg-width)

(defun ads/notify--call-with-faces (faces thunk)
  "Call THUNK with FACES applied, then put them back.
FACES is an alist of (FACE . COLOUR).  The SVG builder reads these globally
with `face-foreground', which no buffer-local remapping reaches, and the
render is synchronous, so setting and restoring around it is safe."
  (let ((saved (mapcar (lambda (cell)
                         (cons (car cell) (face-attribute (car cell) :foreground)))
                       faces)))
    (unwind-protect
        (progn
          (pcase-dolist (`(,face . ,colour) faces)
            (when (stringp colour) (set-face-attribute face nil :foreground colour)))
          (funcall thunk))
      (pcase-dolist (`(,face . ,colour) saved)
        (set-face-attribute face nil :foreground colour)))))

(defun ads/notify--sync-border (&rest _)
  "Point the border at the current theme's `success' green."
  (setq knockknock-border-color (face-foreground 'success nil t)))

(use-package knockknock
  :vc (:url "https://github.com/xenodium/knockknock" :rev :newest)
  :custom
  (knockknock-default-duration 86400 "A day is never; I dismiss these myself")
  (knockknock-border-width 2 "Wide enough that the colour on it reads")
  (knockknock-svg-max-width 700 "Room for a long heading now the width is measured")
  (knockknock-poshandler #'ads/notify--poshandler)
  :custom-face
  (knockknock-title-face ((t (:inherit (bold success) :height 1.3))))
  (knockknock-icon-face ((t (:inherit success))))
  :config
  (ads/notify--sync-border)
  (add-hook 'enable-theme-functions #'ads/notify--sync-border))

(defun ads/notify--child (name)
  "The posframe child frame showing buffer NAME, if it is live."
  (when-let* ((buf (get-buffer name))
              (child (buffer-local-value 'posframe--frame buf)))
    (and (frame-live-p child) child)))

(defconst ads/notify--style-keys '(:border :background :text :icon-color :tint)
  "Keys `ads/notify--broadcast' takes for itself rather than passing on.")

(defun ads/notify--tinted-args (args style)
  "ARGS with its :icon-file recoloured, when STYLE asks for it.
A :tint of t means the theme's foreground, which is the point: the shipped
logo is white and vanishes the moment I am in a light theme.  Resolved here
rather than at notify time so a theme toggle is picked up on the next
relayout, and free of the SVG cache because the colour is in the filename."
  (let ((tint (plist-get style :tint))
        (file (plist-get args :icon-file)))
    (if (not (and tint file))
        args
      (plist-put (copy-sequence args) :icon-file
                 (ads/notify--tint-icon
                  file (if (stringp tint) tint (face-foreground 'default nil t)))))))

(defun ads/notify--show (entry frame y)
  "Render ENTRY on FRAME with its top edge at Y.  Return the next free Y."
  (let* ((name (format "*knockknock %s %s*"
                       (ads/notify--frame-id frame) (ads/notify-entry-id entry)))
         (style (ads/notify-entry-style entry))
         (args (ads/notify--tinted-args (ads/notify-entry-args entry) style))
         (ads/notify--y y)
         ;; Painted into the image and used for the frame, so the cell
         ;; fit-frame-to-buffer rounds up to matches instead of showing black.
         (ads/notify--bg (or (plist-get style :background)
                             (face-background 'default nil t)))
         (knockknock-background-color ads/notify--bg)
         (knockknock-border-color (or (plist-get style :border)
                                      knockknock-border-color))
         (knockknock-left-fringe 0)
         (knockknock-right-fringe 0))
    (with-selected-frame frame
      (let ((knockknock--buffer name))
        (ads/notify--call-with-faces
         `((knockknock-title-face   . ,(plist-get style :text))
           (knockknock-message-face . ,(plist-get style :text))
           (knockknock-icon-face    . ,(plist-get style :icon-color)))
         (lambda () (apply #'knockknock--notify-internal args)))))
    (setf (alist-get frame (ads/notify-entry-buffers entry)) name)
    (+ y ads/notify--gap
       (if-let* ((child (ads/notify--child name))) (frame-pixel-height child) 0))))

(defun ads/notify--relayout ()
  "Redraw the whole stack on every frame, oldest at the top."
  (dolist (frame (ads/notify--frames))
    (let ((y ads/notify--inset))
      (dolist (entry ads/notify--stack)
        (setq y (ads/notify--show entry frame y))))))

(defun ads/notify--without (plist key)
  "PLIST without KEY and its value."
  (cl-loop for (k v) on plist by #'cddr unless (eq k key) append (list k v)))

(defun ads/notify--broadcast (&rest args)
  "Show ARGS on every frame, stacked under the notifications already up.
ARGS is a `knockknock-notify' plist plus an optional :action, the closure
`ads/notify-visit' calls to go to whatever sent this, and the optional
style keys in `ads/notify--style-keys'."
  (knockknock--ensure-initialized)
  (let ((plain args)
        (style nil))
    (dolist (key (cons :action ads/notify--style-keys))
      (when (plist-member plain key)
        (unless (eq key :action)
          (setq style (plist-put style key (plist-get plain key))))
        (setq plain (ads/notify--without plain key))))
    (setq ads/notify--stack
          (append ads/notify--stack
                  (list (ads/notify--entry
                         :id (cl-incf ads/notify--counter)
                         :args plain
                         :action (plist-get args :action)
                         :style style)))))
  (ads/notify--relayout))

(defun ads/notify--retheme (&rest _)
  "Redraw any live popups so their colours follow a theme toggle."
  (when ads/notify--stack (ads/notify--relayout)))

(add-hook 'enable-theme-functions #'ads/notify--retheme)

(defun ads/notify (title message icon &optional action)
  "Put MESSAGE on screen under TITLE, with ICON, until I dismiss it.
ACTION, if given, is what `ads/notify-visit' calls to go to the source."
  (ads/notify--broadcast :title title :message message :icon icon :action action))

(defun ads/notify--delete (entry)
  "Take ENTRY off every frame and out of the stack, without redrawing."
  (dolist (cell (ads/notify-entry-buffers entry))
    (when-let* ((buf (get-buffer (cdr cell))))
      (ignore-errors (posframe-delete buf))))
  (setq ads/notify--stack (delq entry ads/notify--stack)))

(defun ads/notify-close ()
  "Dismiss the lot.  Unbound on purpose; for a popup that outlived its entry."
  (interactive)
  (mapc #'ads/notify--delete (copy-sequence ads/notify--stack))
  (setq ads/notify--stack nil)
  (dolist (b (buffer-list))
    (when (string-prefix-p "*knockknock" (buffer-name b))
      (ignore-errors (posframe-delete b)))))

(defun ads/notify--pick (choose prompt)
  "The oldest live notification, or one picked by hand when CHOOSE is non-nil.
Oldest, not newest: the stack is drawn oldest-first, so this always takes the
popup nearest the corner and the rest close up towards it.
PROMPT is used for the completing read."
  (when ads/notify--stack
    (if (and choose (cdr ads/notify--stack))
        (let ((by-title (mapcar (lambda (e)
                                  (cons (plist-get (ads/notify-entry-args e) :title) e))
                                ads/notify--stack)))
          (cdr (assoc (completing-read prompt by-title nil t) by-title)))
      (car ads/notify--stack))))

(defun ads/notify-dismiss (&optional choose)
  "Dismiss the oldest notification.  One press, one popup.
With a prefix argument CHOOSE, pick which one."
  (interactive "P")
  (if-let* ((entry (ads/notify--pick choose "Dismiss: ")))
      (progn (ads/notify--delete entry)
             (ads/notify--relayout))
    (message "No notifications")))

(defun ads/notify-visit (&optional choose)
  "Go to the source of the oldest notification and dismiss it.
With a prefix argument CHOOSE, pick which one."
  (interactive "P")
  (if-let* ((entry (ads/notify--pick choose "Visit: ")))
      (let ((action (ads/notify-entry-action entry)))
        (ads/notify--delete entry)
        (ads/notify--relayout)
        (if action
            (funcall action)
          (message "No source recorded for %s"
                   (plist-get (ads/notify-entry-args entry) :title))))
    (message "No notifications")))

(ads/leader-keys
  "x" '(ads/notify-dismiss :wk "dismiss notification")
  "X" '(ads/notify-visit :wk "go to notification source"))

;;; knockknock.el ends here
