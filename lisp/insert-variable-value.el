;;; insert-variable-value.el --- Insert a variable value at point  -*- lexical-binding: t; -*-
;;; Commentary:
;; insert-variable-value
;;; Code:

(defun insert-any-variable-value (var)
  "Insert the value of any variable VAR at point."
  (interactive
   (list (intern (completing-read
		  "Insert variable value: "
                  (let (vars)
                    (mapatoms (lambda (sym)
				(when (boundp sym)
				  (push (symbol-name sym) vars))))
                    vars)))))
  (insert (format "%S" (symbol-value var))))

(ads/leader-keys
  "iv" 'insert-any-variable-value)

;;; insert-variable-value.el ends here
