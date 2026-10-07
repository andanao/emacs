;;; org-block-brackets.el --- SVG brackets down the side of org blocks  -*- lexical-binding: t; -*-
;;; Commentary:
;; A corner, a bar and a corner beside every #+begin_ block, drawn as SVG in the
;; row's `line-prefix'.  Unlike org-modern-indent it does not draw over
;; org-indent's prefix, so it works before the first heading too.
;;
;; A `line-spacing' text property can only add to the buffer's spacing, never
;; remove it, so the bar could not cover the gap between rows.  The mode sets
;; `line-spacing' to 0 in the buffer and puts the global value back as a text
;; property on every row outside a block.  Block rows then match the segment's
;; height exactly.
;;; Code:

(defvar ads/org-block-brackets--cache (make-hash-table :test 'equal))
(defvar-local ads/org-block-brackets--timer nil)

(defun ads/org-block-brackets--color ()
  (let ((c (face-foreground 'org-meta-line nil t)))
    (if (and (stringp c) (not (string-prefix-p "unspecified" c))) c "#888888")))

(defun ads/org-block-brackets--spacing ()
  "Pixels of spacing prose rows get, from the global value; the buffer's is 0."
  (let ((ls (or (default-value 'line-spacing) 0)))
    (if (floatp ls) (round (* ls (frame-char-height))) ls)))

(defun ads/org-block-brackets--height ()
  "One height for every segment: the tallest block row plus the prose spacing."
  (+ (apply #'max (frame-char-height)
            (mapcar (lambda (f) (or (ignore-errors (window-font-height nil f)) 0))
                    '(org-block org-block-begin-line org-block-end-line)))
     (ads/org-block-brackets--spacing)))

(defun ads/org-block-brackets--segment (kind)
  "Propertized space displaying the KIND segment: `begin', `mid' or `end'."
  (let* ((w (* 2 (frame-char-width)))
         (h (ads/org-block-brackets--height))
         (color (ads/org-block-brackets--color))
         (key (list kind w h color)))
    (or (gethash key ads/org-block-brackets--cache)
        (puthash
         key
         (let* ((x (/ w 4.0))
                (cy (/ h 2.0))
                (r (min (* 0.6 (frame-char-width)) cy))
                (path (pcase kind
                        ('begin (format "M %f %f H %f Q %f %f %f %f V %d"
                                        w cy (+ x r) x cy x (+ cy r) h))
                        ('end   (format "M %f 0 V %f Q %f %f %f %f H %d"
                                        x (- cy r) x cy (+ x r) cy w))
                        (_      (format "M %f 0 V %d" x h))))
                (svg (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\"><path d=\"%s\" fill=\"none\" stroke=\"%s\" stroke-width=\"1.5\"/></svg>"
                             w h path color)))
           ;; `:scale 1' stops Emacs resizing the bar, which would reopen the gaps.
           (propertize " " 'display (create-image svg 'svg t :ascent 'center :scale 1)))
         ads/org-block-brackets--cache))))

(defun ads/org-block-brackets--build ()
  "Rebuild the spacing and every bracket overlay in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (remove-overlays (point-min) (point-max) 'ads-org-block-brackets t)
      (let ((ls (default-value 'line-spacing)))
        (with-silent-modifications
          (if (and ls (not (eql ls 0)))
              (put-text-property (point-min) (point-max) 'line-spacing ls)
            (remove-text-properties (point-min) (point-max) '(line-spacing nil)))))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*#\\+begin_\\([[:alnum:]_-]+\\)" nil t)
          (let ((first (line-beginning-position))
                (name (match-string 1)))
            (when (re-search-forward
                   (concat "^[ \t]*#\\+end_" (regexp-quote name) "\\_>") nil t)
              (let ((last (line-beginning-position)))
                (goto-char first)
                (while (<= (point) last)
                  (let* ((bol (point))
                         (kind (cond ((= bol first) 'begin)
                                     ((= bol last) 'end)
                                     (t 'mid)))
                         (lp (or (get-text-property bol 'line-prefix) ""))
                         (wp (or (get-text-property bol 'wrap-prefix) lp))
                         (ov (make-overlay bol (min (1+ (line-end-position))
                                                    (point-max)))))
                    (overlay-put ov 'ads-org-block-brackets t)
                    (overlay-put ov 'line-prefix
                                 (concat lp (ads/org-block-brackets--segment kind)))
                    (overlay-put ov 'wrap-prefix
                                 (concat wp (ads/org-block-brackets--segment 'mid)))
                    (with-silent-modifications
                      (put-text-property bol (overlay-end ov) 'line-spacing 0)))
                  (forward-line 1))
                (goto-char (line-end-position))))))))))

(defun ads/org-block-brackets--schedule (&rest _)
  "Rebuild shortly after a change, once org-indent has set its prefixes."
  (when (timerp ads/org-block-brackets--timer)
    (cancel-timer ads/org-block-brackets--timer))
  (let ((buf (current-buffer)))
    (setq ads/org-block-brackets--timer
          (run-with-idle-timer
           0.3 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when ads/org-block-brackets-mode
                   (ads/org-block-brackets--build)))))))))

(define-minor-mode ads/org-block-brackets-mode
  "SVG brackets beside org blocks, independent of org-indent."
  :lighter nil
  (if ads/org-block-brackets-mode
      (progn
        (setq-local line-spacing 0)
        (add-hook 'after-change-functions #'ads/org-block-brackets--schedule nil t)
        (add-hook 'text-scale-mode-hook #'ads/org-block-brackets--schedule nil t)
        (ads/org-block-brackets--build))
    (remove-hook 'after-change-functions #'ads/org-block-brackets--schedule t)
    (remove-hook 'text-scale-mode-hook #'ads/org-block-brackets--schedule t)
    (when (timerp ads/org-block-brackets--timer)
      (cancel-timer ads/org-block-brackets--timer))
    (remove-overlays (point-min) (point-max) 'ads-org-block-brackets t)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(line-spacing nil)))
    (kill-local-variable 'line-spacing)))

(defun ads/org-block-brackets--refresh-all (&rest _)
  "Redraw in every org buffer, after a theme change recolours the bars."
  (clrhash ads/org-block-brackets--cache)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when ads/org-block-brackets-mode (ads/org-block-brackets--build)))))

(add-hook 'org-mode-hook #'ads/org-block-brackets-mode 95)
(add-hook 'enable-theme-functions #'ads/org-block-brackets--refresh-all)

(provide 'org-block-brackets)
;;; org-block-brackets.el ends here
