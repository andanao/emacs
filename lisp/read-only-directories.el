;;; read-only-directories.el --- Mark directories read-only  -*- lexical-binding: t; -*-
;;; Commentary:
;; read-only-directories
;;; Code:

(defcustom read-only-directories '( )
  "list of directories or files that will be opened in read only mode")

(defun find-file-read-only-directories ()
  "Start buffer in read only mode if file is in a child directory of any of the directories defined in read-only-directories."
  (dolist (read-only-directory read-only-directories)
    (when (string-search read-only-directory buffer-file-name)
      (read-only-mode))))

(add-hook 'find-file-hook 'find-file-read-only-directories)

;;; read-only-directories.el ends here
