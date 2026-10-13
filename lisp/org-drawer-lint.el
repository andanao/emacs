;;; org-drawer-lint.el --- Warn about drawers that never close  -*- lexical-binding: t; -*-
;;; Commentary:
;; An unclosed drawer can wedge Org's parser, which freezes the agenda.  A
;; warning on save, and a scan of the agenda files on demand or from the weekly
;; review.  The scan is plain text, so it cannot hang on the thing it looks for.
;;; Code:

(require 'seq)

(defun ads/org-unclosed-drawers ()
  "Return the drawers in the current buffer that never close.
Each is (LINE . NAME), LINE being where the drawer opens.  A drawer ends at
:END: and cannot contain a heading, so a heading inside one means it was
never closed.  Drawer-looking lines inside source blocks are ignored."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (open in-src found)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             (open
              (cond ((string-match-p "\\`[ \t]*:END:[ \t]*\\'" (upcase line))
                     (setq open nil))
                    ((string-match-p "\\`\\*+ " line)
                     (push open found)
                     (setq open nil))))
             ((string-match-p "\\`[ \t]*#\\+begin_src\\_>" (downcase line))
              (setq in-src t))
             ((string-match-p "\\`[ \t]*#\\+end_src\\_>" (downcase line))
              (setq in-src nil))
             ((and (not in-src)
                   (string-match "\\`[ \t]*:\\([[:alnum:]_-]+\\):[ \t]*\\'" line)
                   (not (string= (upcase (match-string 1 line)) "END")))
              (setq open (cons (line-number-at-pos) (match-string 1 line))))))
          (forward-line 1))
        (when open (push open found))
        (nreverse found)))))

(defun ads/org-warn-unclosed-drawers ()
  "Warn, without blocking the save, if this buffer has an unclosed drawer."
  (when (derived-mode-p 'org-mode)
    (when-let* ((found (ads/org-unclosed-drawers)))
      (display-warning
       'org-drawer-lint
       (format "%s: unclosed drawer%s at %s"
               (buffer-name)
               (if (cdr found) "s" "")
               (mapconcat (lambda (d) (format ":%s: on line %d" (cdr d) (car d)))
                          found ", "))
       :warning))))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook #'ads/org-warn-unclosed-drawers nil t)))

(defun ads/org-scan-unclosed-drawers (&optional quiet)
  "List unclosed drawers across `org-agenda-files' in *Unclosed drawers*.
Return how many there are.  With QUIET, only show the buffer when there are
some, and do not select it."
  (interactive)
  (when (fboundp 'ads/org-agenda-files-update)
    (ads/org-agenda-files-update))
  (let ((total 0)
        (report (get-buffer-create "*Unclosed drawers*")))
    (with-current-buffer report
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (file (org-agenda-files t))
          (let ((found (with-temp-buffer
                         (insert-file-contents file)
                         (ads/org-unclosed-drawers))))
            (dolist (d found)
              (setq total (1+ total))
              (insert (format "%s:%d: :%s:\n" (abbreviate-file-name file)
                              (car d) (cdr d))))))
        (when (zerop total) (insert "No unclosed drawers.\n"))
        (goto-char (point-min)))
      (compilation-mode)
      (setq-local compilation-error-regexp-alist '(("^\\([^:\n]+\\):\\([0-9]+\\):" 1 2))))
    (cond ((not quiet) (pop-to-buffer report))
          ((> total 0) (display-buffer report)))
    (message "%d unclosed drawer%s in the agenda files" total (if (= total 1) "" "s"))
    total))

(provide 'org-drawer-lint)
;;; org-drawer-lint.el ends here
