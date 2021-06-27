(defvar temp-file "c:/Git/emacs/scratch.org")

(print temp-file)

(defun efs/tangle-test ()
  (message "\n\nstarted efs")
  (print buffer-file-name)
  (print efs/user-dir-readme)
  (when
      (string= buffer-file-name temp-file)
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil)))
      (message "\n\n\tefs/tangle-test\n\n")
      (org-babel-tangle)))

  ;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/tangle-test)))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/tangle-test)))

(defun tangle-test ()
  (message "\n\nstarted")
  (when (string-equal buffer-file-name temp-file))
    (let
      (message "\n\n\tTangle Test\n \n")
      ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'tangle-test)))

(when
  (string= buffer-file-name temp-file)
      (message "yay")
    )

(when
   (string= buffer-file-name temp-file)
   ;; Dynamic scoping to the rescue
   (let ((org-confirm-babel-evaluate nil)))
   (message "\n\n\tefs/tangle-test\n\n")
   (org-babel-tangle))
