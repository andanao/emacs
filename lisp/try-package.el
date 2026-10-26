;;; try-package.el --- try a MELPA/GNU package without touching the flake  -*- lexical-binding: t; -*-

;; `package-archives' is nil and `package-user-dir' points into the repo, so
;; `package-install' has nothing to work with.  This rebinds both for one call,
;; installs into a scratch directory, and leaves the package activated for this
;; session only.  Nothing is written to ~/.emacs.d/elpa or the repo.
;;
;; The prompt completes over every package in the archives, in the `package'
;; completion category, so marginalia annotates it the way it does for
;; `package-install'.

(require 'package)

(defvar ads/try-package-directory "/tmp/try-pkgs"
  "Scratch `package-user-dir' for `ads/try-package'.")

(defvar ads/try-package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("gnu"   . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  "Archives `ads/try-package' installs from.")

(defun ads/try-package--load-archives (refresh)
  "Make `package-archive-contents' hold the scratch archives' packages.
Read the cached index under `ads/try-package-directory', or fetch it when there
is none or REFRESH is non-nil."
  (let ((package-user-dir ads/try-package-directory)
        (package-archives ads/try-package-archives))
    (make-directory package-user-dir t)
    (if (and (not refresh)
             (file-exists-p (expand-file-name "archives/melpa/archive-contents"
                                              package-user-dir)))
        (package-read-all-archive-contents)
      (message "try-package: fetching the package index...")
      (package-refresh-contents))))

(defun ads/try-package--annotate (name)
  "Version, archive and summary for the candidate NAME.
Only seen without marginalia, which has its own annotator for this category."
  (when-let* ((desc (cadr (assq (intern name) package-archive-contents))))
    (format "  %-14s %-8s %s"
            (package-version-join (package-desc-version desc))
            (or (package-desc-archive desc) "")
            (or (package-desc-summary desc) ""))))

(defun ads/try-package--read (refresh)
  "Read a package name from everything the scratch archives offer."
  (ads/try-package--load-archives refresh)
  (let ((names (sort (mapcar (lambda (p) (symbol-name (car p)))
                             package-archive-contents)
                     #'string<)))
    (completing-read
     (format "Try package (%d available): " (length names))
     (lambda (str pred action)
       (if (eq action 'metadata)
           '(metadata (category . package)
                      (annotation-function . ads/try-package--annotate))
         (complete-with-action action names str pred)))
     nil t)))

(defun ads/try-package (name &optional refresh)
  "Install package NAME into a scratch directory and activate it for this session.
Dependencies the session already has, such as the flake's, are not reinstalled.
The archive index is cached under `ads/try-package-directory'; with a prefix
argument REFRESH, fetch it again."
  (interactive (let ((refresh current-prefix-arg))
                 (list (ads/try-package--read refresh) refresh)))
  (let ((sym (intern name))
        (package-user-dir ads/try-package-directory)
        (package-archives ads/try-package-archives))
    (if (package-installed-p sym)
        (message "try-package: %s is already available in this session" name)
      (unless package-archive-contents
        (ads/try-package--load-archives refresh))
      (package-install sym)
      (message "try-package: %s installed under %s for this session; restart drops it"
               name package-user-dir))))

(defun ads/try-package-clean ()
  "Delete `ads/try-package-directory'.
Packages already activated stay loaded until Emacs restarts."
  (interactive)
  (when (and (file-directory-p ads/try-package-directory)
             (yes-or-no-p (format "Delete %s? " ads/try-package-directory)))
    (delete-directory ads/try-package-directory t)
    (message "try-package: removed %s" ads/try-package-directory)))

;;; try-package.el ends here
