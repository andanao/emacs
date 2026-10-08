;;; extras.el --- Smaller org add-ons  -*- lexical-binding: t; -*-
;;; Commentary:
;; anki-editor, org-autolist, org-cliplink, org-download, org-fragtog,
;; org-habit, org-noter
;;; Code:

(use-package anki-editor
  :vc (:url "https://github.com/anki-editor/anki-editor" :rev :newest)
  ;; I think I can get away with resetting with a hook in the capture template
  ;; :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t
        anki-editor-org-tags-as-anki-tags t
        ads/anki-file (concat org-directory "anki.org"))


  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (anki-editor-reset-cloze-number)

  (add-hook 'find-file-hook
            '(lambda ()
               (when
                 (string-equal-ignore-case buffer-file-name ads/anki-file)
                 (anki-editor-mode))))
  )

(ads/leader-keys
  :keymaps 'org-mode-map
  "ok" '(:ignore t :wk "anKi")
  "okp" 'anki-editor-push-new-notes
  "okP" 'anki-editor-push-notes
  "okk" 'anki-editor-cloze-region-auto-incr
  "okj" 'anki-editor-cloze-dwim
  "okg" 'anki-editor-gui-browse
  "okG" 'anki-editor-gui-add-cards
  )

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package org-cliplink)
(ads/leader-keys "oL" '(org-cliplink :wk "org-cliplink"))

;; ~/Downloads is where most attachments start.
(defun ads/org-attach-from-downloads ()
  "Attach a file to the heading at point, prompting from ~/Downloads."
  (interactive)
  (org-attach-attach (read-file-name "Attach from downloads: " "~/Downloads/")))

(ads/leader-keys
  "oF" '(ads/org-attach-from-downloads :wk "attach from downloads")
  "oj" '(ads/open-downloads :wk "dired downloads"))

(defun ads/open-downloads ()
  "Open ~/Downloads in dired."
  (interactive)
  (dired "~/Downloads/"))

(use-package org-download
  :vc (org-download
       :url "https://github.com/andanao/org-download"
       :main-file "org-download.el"
       :branch "master"
       :rev :newest)
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method 'imagemagick/convert)
  :config
  ;; firefox makes copied images into bmp so this helps
  (add-to-list 'image-file-name-extensions "bmp")
  (setq org-download-annotate-function '(lambda (link) ""))
  (ads/leader-keys
    :keymaps '(org-mode-map)
    "os" 'org-download-clipboard))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(use-package org-noter
  :after (pdf-tools org)
  :init
  (setq org-noter-supported-modes '(doc-view-mode pdf-view-mode nov-mode))
  :custom
  (org-noter-notes-search-path (list org-directory))
  (org-noter-suggest-from-attachments t)
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-auto-save-last-location t))

(ads/leader-keys
  "on" 'org-noter)

;;; extras.el ends here
