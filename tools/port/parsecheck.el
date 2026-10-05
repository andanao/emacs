;;; parsecheck.el --- read every config file, report any that will not parse  -*- lexical-binding: t; -*-
;;; Commentary:
;; Cheap pre-flight: catches unbalanced parens and bad reader syntax without
;; needing the package set.  Run with emacs -Q --batch -l parsecheck.el
;;; Code:

(let* ((bad 0)
       (n 0)
       ;; Emacs' own scratch ends in .el too: .#foo.el is the lock symlink
       ;; for a buffer with unsaved changes and dangles by design, and
       ;; #foo.el# is an auto-save.  Reading either reports a failure that
       ;; says nothing about the config.
       (sources (lambda (dir)
                  (seq-remove
                   (lambda (f) (string-match-p "\\`[.]?#" (file-name-nondirectory f)))
                   (directory-files-recursively dir "\\.el\\'"))))
       (files (append (seq-filter #'file-exists-p
                                  '("early-init.el" "init.el" "mac.el"
                                    "linux.el" "ms-windows.el"))
                      (funcall sources "config")
                      (funcall sources "lisp")
                      (funcall sources "themes"))))
  (dolist (f (sort files #'string<))
    (setq n (1+ n))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (let ((forms 0))
            (while (progn (skip-chars-forward " \t\n\f")
                          (not (eobp)))
              (if (eq (char-after) ?\;)
                  (forward-line 1)
                (read (current-buffer))
                (setq forms (1+ forms))))
            (message "  ok %4d forms  %s" forms f)))
      (error
       (setq bad (1+ bad))
       (message "  FAIL            %s: %s" f (error-message-string err)))))
  (message "")
  (message "%d files checked, %d failed" n bad)
  (kill-emacs (if (> bad 0) 1 0)))
;;; parsecheck.el ends here
