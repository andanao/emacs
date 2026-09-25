;;; d2.el --- d2 diagrams and their previews  -*- lexical-binding: t; -*-
;;; Commentary:
;; d2-mode, making d2 blocks behave like latex previews,
;; editing blocks with =C-c '=, block defaults,
;; regenerate d2 diagrams on theme change
;;; Code:

(use-package d2-mode
  :mode "\\.d2\\'"
  :commands (org-babel-execute:d2)
  :custom
  (d2-output-format ".svg"))

(defvar ads/d2-theme-light 0   "D2 theme id used under a light Emacs theme.")
(defvar ads/d2-theme-dark  200 "D2 theme id used under a dark Emacs theme.")
(defvar ads/d2-pad 8 "Pixels of padding around a rendered d2 diagram.")
(defvar ads/d2-ascii-mode "extended"
  "d2 =--ascii-mode=: \"extended\" for box-drawing glyphs, \"standard\" for pure ASCII.")

(defun ads/dark-background-p ()
  "Non-nil when the `default' face background is a dark colour."
  (let ((rgb (color-values (face-background 'default nil t))))
    (and rgb (< (/ (apply #'+ rgb) 3.0) 32768))))

(defun ads/org-babel-execute:d2--ascii (body)
  "Render BODY to a box-drawing text diagram and return it as a string."
  (let ((temp-file (org-babel-temp-file "d2-")))
    (with-temp-file temp-file (insert body))
    (with-temp-buffer
      ;; stderr discarded: d2 logs its success banner there.
      (let ((status (call-process d2-location nil (list t nil) nil
                                  temp-file "-"
                                  "--stdout-format" "ascii"
                                  (format "--ascii-mode=%s" ads/d2-ascii-mode))))
        (unless (eq status 0)
          (error "d2 exited %s" status))
        (string-trim-right (buffer-string))))))

(defun ads/org-babel-execute:d2 (body params)
  "Render BODY to the =:file= named in PARAMS with the theme-appropriate d2 theme.
Replaces `org-babel-execute:d2': reports failure by exit status rather
than by stderr being non-empty, and renders on a transparent background.
With a non-file =:results=, returns a text diagram instead of writing an SVG."
  (if (not (member "file" (cdr (assq :result-params params))))
      (ads/org-babel-execute:d2--ascii body)
  (let* ((out-file (or (cdr (assq :file params))
                       (error "d2 block needs a #+name: line (or an explicit :file)")))
         (temp-file (org-babel-temp-file "d2-"))
         (theme (if (ads/dark-background-p) ads/d2-theme-dark ads/d2-theme-light))
         (args (append (list temp-file
                             (org-babel-process-file-name out-file)
                             (format "--theme=%d" theme)
                             (format "--pad=%d" ads/d2-pad))
                       d2-flags)))
    (when (file-name-directory out-file)
      (make-directory (file-name-directory out-file) t))
    (with-temp-file temp-file
      (insert body)
      (unless (string-match-p "^[ \t]*style\\.fill[ \t]*:" body)
        (unless (bolp) (insert "\n"))
        (insert "style.fill: transparent\n")))
    (with-current-buffer (get-buffer-create "*d2 output*")
      (erase-buffer)
      (let ((status (apply #'call-process d2-location nil t nil args)))
        (unless (eq status 0)
          (error "d2 exited %s: %s" status (string-trim (buffer-string))))))
    nil)))

(with-eval-after-load 'd2-mode
  (advice-add 'org-babel-execute:d2 :override #'ads/org-babel-execute:d2))

(defun ads/d2-compile-fileless (orig &rest args)
  "Compile the buffer instead of ORIG's file when there is no file.
ARGS are passed through untouched when the buffer is visiting one."
  (if buffer-file-name (apply orig args) (d2-compile-buffer)))

(with-eval-after-load 'd2-mode
  (advice-add 'd2-compile :around #'ads/d2-compile-fileless))

(with-eval-after-load 'd2-mode
  (setq org-babel-default-header-args:d2
        '((:results . "file")
          (:exports . "results")
          (:file-ext . "svg")
          (:output-dir . "img"))))

(defun ads/org-refresh-d2-images ()
  "Regenerate the image behind every d2 src block in the buffer.
Rewrites the files only; the buffer text is untouched."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-babel-map-src-blocks nil
      (when (equal lang "d2")
        (save-excursion
          (goto-char beg-block)
          (let ((info (org-babel-get-src-block-info t)))
            (ads/org-babel-execute:d2 (nth 1 info) (nth 2 info))))))
    (org-link-preview-refresh)))

(defun ads/org-refresh-d2-visible ()
  "Run `ads/org-refresh-d2-images' in every visible org buffer, off an idle timer.
Deferred for the same reason the LaTeX refresh is: the theme should
finish painting before Emacs goes off and shells out to `d2'."
  (run-with-idle-timer
   0.3 nil
   (lambda ()
     (dolist (window (window-list))
       (with-current-buffer (window-buffer window)
         (ads/org-refresh-d2-images))))))

(add-hook 'modus-themes-after-load-theme-hook #'ads/org-refresh-d2-visible)

;;; d2.el ends here
