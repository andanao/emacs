;;; quartz.el --- Quartz site publishing  -*- lexical-binding: t; -*-
;;; Commentary:
;; quartz
;;; Code:

(setopt ads/quartz-url "https://andanao.github.io/org-quartz/"
        ads/quartz-dir "~/git/org-quartz")

(defun ads/quartz-heading-anchor ()
  "Return the anchor for the heading at point, or \"\" when there isn't one.

Only fires on an active region, so from normal state the URL functions still
give the plain page and from visual state they point at the heading.  The slug
matches what Quartz generates: downcase, drop punctuation, spaces to hyphens.
Tags come along for the ride because `ox-md' writes them into the heading."
  (if (not (and (use-region-p) (derived-mode-p 'org-mode)))
      ""
    (save-excursion
      (goto-char (region-beginning))
      (if (org-before-first-heading-p)
          ""
        (org-back-to-heading t)
        (let* ((tags (org-get-tags nil t))
               (heading (concat (org-get-heading t t t t)
                                (and tags (concat "     " (org-make-tag-string tags))))))
          (concat "#" (mapconcat
                       (lambda (c)
                         (cond ((= c ?\s) "-")
                               ((or (and (>= c ?a) (<= c ?z))
                                    (and (>= c ?0) (<= c ?9))
                                    (memq c '(?- ?_)))
                                (char-to-string c))
                               (t "")))
                       (downcase heading) "")))))))

(defun ads/quartz-get-url ()
  "Return URL for current org-roam note.
With a region active, anchor it to the heading the region starts in."
  (let* ((title (or (org-get-title) (file-name-base (buffer-file-name))))
         (slug (replace-regexp-in-string "-+" "-"
                 (replace-regexp-in-string "[^a-z0-9-]" ""
                   (replace-regexp-in-string "[_ ]+" "-" (downcase title)))))
         (slug (string-trim slug "-")))
    (concat ads/quartz-url slug (ads/quartz-heading-anchor))))

(defun ads/quartz-copy-url ()
  "Copy URL for current org-roam note to clipboard."
  (interactive)
  (let ((url (ads/quartz-get-url)))
    (kill-new url)
    (message "Copied: %s" url)))

(defun ads/quartz-open-url ()
  "Open current org-roam note in browser."
  (interactive)
  (browse-url (ads/quartz-get-url)))

(defun ads/quartz-projectile ()
  "Projectile search through my quartz repo"
  (interactive)
  (projectile-switch-project-by-name ads/quartz-dir))

(ads/leader-keys
  "ou" 'ads/quartz-copy-url
  "oU" 'ads/quartz-open-url
  "oz" 'ads/quartz-projectile)

;;; quartz.el ends here
