;;; transclusion.el --- org-transclusion  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-transclusion, transclusion quote only
;;; Code:

(use-package org-transclusion
  :after org
  :config
  ;; Render transclusions automatically in daily files.
  (add-hook 'org-mode-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-prefix-p
                          (expand-file-name org-roam-dailies-directory org-directory)
                          (expand-file-name buffer-file-name)))
                (org-transclusion-add-all))))
  (ads/leader-keys
    :keymaps 'org-mode-map
    "oe"  '(:ignore t :wk "transclusion (Embed)")
    "oea" 'org-transclusion-add
    "oek" 'org-transclusion-make-from-link
    "oeK" 'ads/org-transclusion-make-quote-from-link
    "oeA" 'org-transclusion-add-all
    "oes" 'org-transclusion-move-to-source
    "oer" 'org-transclusion-remove
    "oeR" 'org-transclusion-remove-all
    "oem" 'org-transclusion-mode
    "oel" 'org-transclusion-open-source
    "oeg" 'org-transclusion-refresh))

(with-eval-after-load 'org-transclusion
  (defun ads/org-transclusion-make-quote-from-link ()
    "Convert the link at point into a `:only-quote' transclusion.
Like `org-transclusion-make-from-link', but the resulting keyword
carries `:only-quote' so only the source's first quote block is
transcluded.  Placed on the next empty line; rendered immediately
when `org-transclusion-mode' is active."
    (interactive)
    (let ((context (org-element-lineage (org-element-context) '(link) t)))
      (unless context (user-error "No link at point"))
      (let* ((contents-beg (org-element-property :contents-begin context))
             (contents-end (org-element-property :contents-end context))
             (contents (when contents-beg
                         (buffer-substring-no-properties contents-beg contents-end)))
             (link (org-element-link-interpreter context contents)))
        (save-excursion
          (org-transclusion-search-or-add-next-empty-line)
          (insert (format "#+transclude: %s :only-quote\n" link))
          (when org-transclusion-mode
            (forward-line -1)
            (org-transclusion-add))))))

  (defun ads/org-transclusion-keyword-value-only-quote (string)
    "Parse the :only-quote flag out of a #+transclude: STRING."
    (when (string-match ":only-quote" string)
      (list :only-quote t)))

  (defun ads/org-transclusion-only-quote-to-string (plist)
    "Re-serialise :only-quote so it survives refresh."
    (when (plist-get plist :only-quote) ":only-quote"))

  (defun ads/org-transclusion-add-only-quote (link plist)
    "Payload containing only the first quote block of LINK's source.
Returns nil (falling through to the default handlers) unless
:only-quote is set.  Handles id: and file: links."
    (when (plist-get plist :only-quote)
      (let* ((type (org-element-property :type link))
             (marker
              (cond
               ((string= type "id")
                (ignore-errors (org-id-find (org-element-property :path link) t)))
               ((string= type "file")
                (set-marker (make-marker) 1
                            (find-file-noselect
                             (org-element-property :path link)))))))
        (when (and marker (marker-buffer marker))
          (with-current-buffer (marker-buffer marker)
            (org-with-wide-buffer
             (goto-char marker)
             ;; Narrow to the subtree when the id points at a heading,
             ;; otherwise search the whole (file-level) buffer.
             (let* ((subtree
                     (unless (org-before-first-heading-p)
                       (cons (progn (org-back-to-heading t) (point))
                             (progn (org-end-of-subtree t t) (point)))))
                    (beg (or (car subtree) (point-min)))
                    (end (or (cdr subtree) (point-max))))
               (save-restriction
                 (narrow-to-region beg end)
                 (when-let* ((q (org-element-map (org-element-parse-buffer)
                                    'quote-block #'identity nil t))
                             (qbeg (org-element-property :begin q))
                             (qend (org-element-property :end q)))
                   (list :tc-type "org-quote"
                         :src-content (buffer-substring-no-properties qbeg qend)
                         :src-buf (current-buffer)
                         :src-beg qbeg
                         :src-end qend))))))))))

  (add-hook 'org-transclusion-keyword-value-functions
            #'ads/org-transclusion-keyword-value-only-quote)
  (add-hook 'org-transclusion-keyword-plist-to-string-functions
            #'ads/org-transclusion-only-quote-to-string)
  ;; Front of the list so it intercepts before the default id/file handlers.
  (add-hook 'org-transclusion-add-functions
            #'ads/org-transclusion-add-only-quote))

;;; transclusion.el ends here
