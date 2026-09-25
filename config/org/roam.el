;;; roam.el --- org-roam and the roam helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-roam, roam-agenda, roam-active-projects, roam-categories,
;; roam-capture-dailies, roam-daily-today, roam-daily-archive,
;; roam-insert-immediate, roam-node-display, roam-modeline,
;; roam-project-complete, roam-refile-tag-file-list, roam-refile-category,
;; roam-refile-note, roam-stub-tag, org-roam-consult, org-roam-ui,
;; org-roam-ql
;;; Code:

(use-package org-roam
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory org-directory)
  (org-roam-completion-everywhere t)
  ;; Index links inside #+transclude: keywords so transclusions show up as
  ;; forward/backlinks like any other roam link.  org-roam excludes the
  ;; "transclude" keyword by default; drop it from the exclusion list.
  (org-roam-db-extra-links-exclude-keys '((node-property . ("ROAM_REFS"))))
  (org-roam-db-node-include-function
   (lambda ()
     (not (member "ATTACH" (org-get-tags))))) ;; ignore node if ATTACH is a tag

  (org-roam-capture-templates '(("d" "default" plain "%?" :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
":PROPERTIES:
:CREATED: %U
:END:
#+title: ${title}
") :unnarrowed t)))

  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  (require 'org-roam-dailies)

  (ads/leader-keys
    "f" '(:ignore t :wk "roam")
    "d" 'org-roam-node-find
    "fD" '((lambda () (interactive)
	     (org-roam-db-sync)
	     (ads/org-agenda-files-update))
	   :wk "db sync & agenda")
    "fd" 'org-roam-dailies-map)

  (general-define-key :states '(insert visual) "C-f" 'org-roam-node-insert)

  (ads/leader-keys
    :keymaps 'org-mode-map
    "C-f" 'org-roam-node-insert
    "fi" 'org-roam-node-insert
    "fa" 'org-roam-alias-add
    "fe" 'org-roam-extract-subtree
    "f;" 'org-roam-tag-add
    "f:" 'org-roam-tag-remove
    "fr" 'org-roam-ref-add
    "fR" 'org-roam-ref-remove
    "ft" 'org-roam-buffer-toggle))

(add-to-list 'org-tags-exclude-from-inheritance "agenda")
(add-to-list 'org-tags-exclude-from-inheritance "refile")
(add-to-list 'org-tags-exclude-from-inheritance "project_a")

(defun ads/org-todo-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map                          ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match))                     ; (3))

(defun ads/org-update-agenda-tag ()
  "Add :agenda: tag to the current org-roam buffer"
  (when (and (not (active-minibuffer-window))
             (not (string-match-p "/daily/" buffer-file-name))
             (org-roam-file-p buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (if (and (ads/org-todo-p)
               (not (member "project" (org-roam-node-tags (org-roam-node-at-point))))
               (not (member "project_h" (org-roam-node-tags (org-roam-node-at-point)))))
	  (org-roam-tag-add '("agenda"))
	  (org-roam-tag-remove '("agenda"))))))

(add-hook 'before-save-hook 'ads/org-update-agenda-tag)

(defun ads/org-roam-agenda-files ()
  "return a list of files containing the :agenda: tag"
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"agenda\"%"))]))))

(defun ads/org-agenda-files-update (&optional arg)
  "Update org agenda files list"
  ;; Claim org-agenda-files here so custom.el doesn't mess with it
  (defvar org-agenda-files)
  (customize-set-variable 'org-agenda-files (ads/org-roam-agenda-files))
  (add-to-list 'org-agenda-files ads/inbox-file)
  (message "Agenda files updated from roam tags"))
(add-hook 'after-init-hook 'ads/org-agenda-files-update)

(advice-add 'org-agenda :before #'ads/org-agenda-files-update)
(advice-add 'org-todo-list :before #'ads/org-agenda-files-update)

(defun ads/roam-active-projects ()
  "Return roam links to active projects, one per line"
  (mapconcat
   (lambda (node)
     (format "[[id:%s][%s]]" (car node) (cadr node)))
   (org-roam-db-query
    [:select [nodes:id, nodes:title]
     :from tags
     :left-join nodes
     :on (= tags:node-id nodes:id)
     :where (like tag (quote "%\"project_a\"%"))])
   "\n"))

(defun ads/roam-active-projects-insert ()
  "Insert roam links to active projects"
  (interactive)
  (insert (ads/roam-active-projects) "\n"))

(defun ads/org-node-name-to-category ()
  (interactive)
  (when (org-roam-file-p)
    (let* ((category-icon (ads/nerd-icons-select))
           (node-title (org-roam-node-title (org-roam-node-at-point)))
           (category-text (read-string
                           "Category: "
                           node-title ))
           (category (concat category-icon " " category-text)))
      (save-excursion
        (goto-char (point-min))
        (org-set-property "CATEGORY" category))
      (message category))))


(ads/leader-keys
  :keymaps 'org-mode-map
  "fc" 'ads/org-node-name-to-category)

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))))

(setq org-roam-dailies-directory "daily/")
(defun ads/roam-daily-today ()
  "Return path of active daily"
  (concat
   org-roam-directory
   org-roam-dailies-directory
   (format-time-string "%Y-%m-%d.org")))

(defun ads/roam-refile-today ()
  "Refile current node to active daily"
  (interactive)
  (org-roam-refile
    (org-roam-node-from-title-or-alias
     (format-time-string "%Y-%m-%d"))))

(ads/leader-keys
  "fdr" 'ads/roam-refile-today)

(setq org-archive-location (concat (ads/roam-daily-today) "::")
      org-archive-save-context-info '(time file olpath category itags))

(run-at-time "00:01"
             86400
             '(lambda ()
                (setq org-archive-location
                      (concat (ads/roam-daily-today) "::"))))

(defun ads/org-archive ()
  "Archive file adding ARCHIVE_NODE property"
  (interactive)
  (let* ((node (org-roam-node-at-point))
	 (id (org-roam-node-id node))
	 (title (org-roam-node-title node))
	 (ref (concat "[[id:"id "][" title "]]")))
    (when (not (string= "Inbox" title))
     (org-set-property "ARCHIVE_NODE" ref)))
  (org-archive-subtree)
  (save-buffer))

(defun ads/org-archive-done ()
  "Change status to done and archive"
  (interactive)
  (org-todo 'done)
  (ads/org-archive))

(ads/leader-keys
  :keymaps 'org-mode-map
  "ox" 'ads/org-archive
  "oX" 'ads/org-archive-done)

(defun org-archive-with-attachments ()
  (when (org-attach-dir)
    (let*
       ((attach-dir (org-attach-dir))
        (destination (file-name-directory org-archive-location))
        (attach-dir-new
         (concat destination "data" (nth 1 (split-string attach-dir "data")))))
      (make-directory attach-dir-new t)
      (rename-file attach-dir attach-dir-new t))))
(advice-add 'org-archive-subtree :before 'org-archive-with-attachments)

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates
         '(("d" "stub" plain "%?"
            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
":PROPERTIES:
:CREATED: %U
:END:
#+title: ${title}
#+filetags: :stub:
")
            :immediate-finish t
            :unnarrowed t))))
    (apply #'org-roam-node-insert args)))

(ads/leader-keys
  :keymaps 'org-mode-map
  "fI" 'org-roam-node-insert-immediate)

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat (propertize "${title:50}" 'face 'org-verbatim)
	      (propertize " ${tags:*}" 'face 'org-tag)
	      (propertize " ${type:1}" 'face 'org-roam-dim)
	      ))

(defun ads/org-buffer-title ()
  "Return the #+title of the current org buffer, if any."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (goto-char (point-min))
     (and (re-search-forward "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$"
                             (min (point-max) 4096) t)
          (match-string-no-properties 1)))))

(defun ads/doom-modeline-org-title (fn)
  "Use the org title in place of the file name, falling back to FN."
  (if-let ((title (ads/org-buffer-title)))
      (propertize title
                  'face 'doom-modeline-buffer-file
                  'mouse-face 'mode-line-highlight
                  'help-echo (concat buffer-file-name
                                     "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
                  'local-map mode-line-buffer-identification-keymap)
    (funcall fn)))

(advice-add 'doom-modeline-buffer-file-name :around #'ads/doom-modeline-org-title)

(defun ads/org-mark-project-complete ()
  "Retag the current roam project as complete and celebrate"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-roam-tag-remove '("project_a" "project_h"))
    (org-roam-tag-add '("project_c"))
    (org-set-property "COMPLETED" (format-time-string "[%Y-%m-%d %a]")))
  (save-buffer)
  (when-let* ((confetti (executable-find "confetti")))
    (call-process confetti nil 0 nil "-p" "intense")))

(defun ads/org-refile-files (&optional refile-arg)
  "Create a list of files containing the :refile: tag"
  (setopt ads/org-refile-files
   (seq-uniq
    (seq-map
     #'car
     (org-roam-db-query
      [:select [nodes:file]
       :from tags
       :left-join nodes
       :on (= tags:node-id nodes:id)
       :where (like tag (quote "%\"refile\"%"))])))))

(ads/org-refile-files)

(advice-add 'org-refile :before 'ads/org-refile-files)

(setopt org-refile-targets
        '((org-agenda-files :level . 0)
          (ads/org-refile-files :level . 0)))

(defun ads/org-refile-category (file)
  "Return FILE's file level CATEGORY, cutesy icon and all"
  (when-let* ((buffer (find-buffer-visiting file)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (org-entry-get (point-min) "CATEGORY")))))

(defun ads/org-refile-show-category (targets)
  "Prefix each refile target in TARGETS with its category"
  (mapcar
   (lambda (target)
     (if-let* ((name (car target))
               (category (ads/org-refile-category (nth 1 target))))
         ;; the category usually is the title with an icon glued on, so only
         ;; keep both when they actually say different things
         (cons (if (string-suffix-p name category)
                   category
                 (concat category " · " name))
               (cdr target))
       target))
   targets))

(advice-add 'org-refile-get-targets :filter-return 'ads/org-refile-show-category)

(defun org-move-note-with-attachments (file destination)
  "Move note and it's attachment directory to a new place"
  (let ((attach-dirs '()))
  (rename-file file (concat destination "/"))

  ;; make list of attach dirs
  (org-with-wide-buffer
   (goto-char (point-min))
   ;; At root
   (when (org-attach-dir)
     (push (org-attach-dir) attach-dirs))

   ;; At children
   (while (re-search-forward "^*" nil t)
     (when (org-attach-dir)
       (push (org-attach-dir) attach-dirs))))

  ;; move all files
  (dolist
      (attach-dir attach-dirs)
    (let
        ((new-dir (concat destination "/data" (nth 1 (split-string attach-dir "data")))))
      (make-directory new-dir t)
      (rename-file attach-dir new-dir t)))))


(defun ads/refile-move-work ()
  "move org file to work"
  (interactive)
  (org-move-note-with-attachments buffer-file-name (concat org-directory "k2"))
  (kill-buffer)
  (if-let ((next-file (car (directory-files org-directory t "\\.org$"))))
      (find-file next-file)
    (message "all org notes filed properly")))

(defun ads/refile-move-personal ()
  "move org file to personal"
  (interactive)
  (org-move-note-with-attachments buffer-file-name (concat org-directory "personal"))
  (kill-buffer)
  (if-let ((next-file (car (directory-files org-directory t "\\.org$"))))
      (find-file next-file)
    (message "all org notes filed properly")))

(defun ads/org-roam-stub-p ()
  "Return t when the buffer holds nothing but its org-roam file header."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (looking-at org-property-drawer-re)
     (goto-char (match-end 0))
     (forward-line))
   (while (and (not (eobp))
               (looking-at-p "^[ \t]*\\(#\\+.*\\)?$"))
     (forward-line))
   (eobp)))

(defun ads/org-update-stub-tag ()
  "Add :stub: tag to empty org-roam buffers, remove it once they have content"
  (when (and buffer-file-name
             (not (active-minibuffer-window))
             (not (string-match-p "/daily/" buffer-file-name))
             (org-roam-file-p buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      ;; roam files without a file-level ID (the k2 archive, say) have no node
      ;; at point-min, and both tag functions need one
      (when-let* ((node (org-roam-node-at-point)))
        (if (ads/org-roam-stub-p)
            (org-roam-tag-add '("stub"))
          (when (member "stub" (org-roam-node-tags node))
            (org-roam-tag-remove '("stub"))))))))

(add-hook 'before-save-hook 'ads/org-update-stub-tag)

(use-package consult-org-roam
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?o)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers nil)
   :config
   ;; Upstream never declares this; it only gets `setq'd once the Org-roam
   ;; source computes its items, so previewing before that is a void-variable.
   (defvar org-roam-buffer-open-buffer-list nil)

   ;; Stock `consult-org-roam-forward-links' only sees parsed [[id:]] link
   ;; elements, so links inside #+transclude: keywords are invisible.  Collect
   ;; links via `org-roam-db-map-links' instead (keyword-aware, and it honours
   ;; `org-roam-db-extra-links-exclude-keys') so transclusions count as forward
   ;; links.
   (defun consult-org-roam-forward-links (&optional other-window)
     "Select a forward link contained in the current buffer.
If OTHER-WINDOW, visit the node in another window."
     (interactive)
     (let ((id-links '()))
       (org-roam-db-map-links
        (list (lambda (link)
                (when (string= (org-element-property :type link) "id")
                  (push (org-element-property :path link) id-links)))))
       (if id-links
           (consult-org-roam--open-or-capture
            other-window
            (consult-org-roam-node-read
             ""
             (lambda (n)
               (and (org-roam-node-p n)
                    (member (org-roam-node-id n) id-links)))))
         (user-error "No forward links found"))))

   (consult-customize
    consult-org-roam-forward-links :preview-key "C-l"
    consult-org-roam-backlinks :preview-key "C-l"
    consult-org-roam-backlinks-recursive :preview-key "C-l"
    )
   ;; Finding and searching the roam graph is useful from any buffer, like
   ;; `org-roam-node-find' on ~SPC d~.  The link commands are relative to the
   ;; current node, so they stay scoped to org.
   (ads/leader-keys
    "ff" 'consult-org-roam-file-find
    "fs" 'consult-org-roam-search
    "fg" 'consult-org-roam-search ;; roam grep
    )

   (ads/leader-keys
    :keymaps 'org-mode-map
    "fl" 'consult-org-roam-forward-links
    "fh" 'consult-org-roam-backlinks
    "fH" 'consult-org-roam-backlinks-recursive
    )
   )

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-roam-ql
  :after (org-roam)
  :bind
  ((:map org-roam-mode-map
         ("Q" . org-roam-ql-buffer-dispatch)))
  :config
  (ads/leader-keys
    "oq" 'org-roam-ql-search))

;;; roam.el ends here
