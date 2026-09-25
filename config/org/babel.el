;;; babel.el --- org-babel languages and templates  -*- lexical-binding: t; -*-
;;; Commentary:
;; org-babel
;;; Code:

(require 'org-tempo)
(require 'ob-tangle)

(customize-set-variable 'org-src-window-setup 'current-window)
(customize-set-variable 'org-src-preserve-indentation t)
(customize-set-variable 'org-edit-src-content-indentation 0)

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-link-preview-refresh)

(dolist
    (template
     '(("el" . "src emacs-lisp")
       ("py" . "src python")
       ("sh" . "src shell")
       ("z" . "src zsh")
       ("nu" . "src nu")
       ("b" . "src bat")
       ("rs" . "src rust")
       ("html" . "src html")
       ("css" . "src css")
       ("cc" . "src C")
       ("cpp" . "src C++")
       ("cs" . "src C#")
       ("k" . "src calc")
       ("yaml" . "src yaml")
       ("toml" . "src toml")
       ("js" . "src javascript")
       ("json" . "src json")
       ("j" . "src json")
       ("ja" . "src java")
       ("sql" . "src sql")))
  (add-to-list 'org-structure-template-alist template))
(tempo-define-template
 "org-d2-diagram"
 '("#+name: " p n "#+begin_src d2" n> p n "#+end_src")
 "<d"
 "Insert a named d2 diagram block"
 'org-tempo-tags)

(tempo-define-template
 "org-d2-ascii"
 '("#+begin_src d2 :results verbatim :wrap example" n> p n "#+end_src")
 "<da"
 "Insert a d2 block that renders to text"
 'org-tempo-tags)
(with-eval-after-load 'org
     (org-babel-do-load-languages
         'org-babel-load-languages
         '((emacs-lisp . t)
           (shell . t)
           (calc . t)
           (latex . t)
           (dot . t)
           (python . t)))

  ;; Same ergonomics as d2: name the block, get =img/NAME.svg=.
  (setq org-babel-default-header-args:dot
        '((:results . "file")
          (:exports . "results")
          (:file-ext . "svg")
          (:output-dir . "img")))

  ;; Evaluate a block the way init.el loads it.
  (setq org-babel-default-header-args:emacs-lisp
        '((:lexical . t))))

    (setq org-confirm-babel-evaluate nil)

(setq org-babel-default-header-args:python
	     '((:results . "output")
	       ))

;;; babel.el ends here
