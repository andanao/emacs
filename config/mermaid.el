;;; mermaid.el --- mermaid diagrams and their previews  -*- lexical-binding: t; -*-
;;; Commentary:
;; mermaid-mode for =.mmd= files (=C-c C-c= renders and opens the image),
;; mmdc driven through the installed Chrome (nixpkgs ships no browser on macOS),
;; save before rendering, since mmdc reads the file on disk,
;; ob-mermaid for babel blocks,
;; block defaults that match d2 (name the block, get =img/NAME.svg=),
;; =<m= template
;;; Code:

(defvar ads/mermaid-puppeteer-config
  (expand-file-name "config/mermaid-puppeteer.json" ads/config-directory)
  "Puppeteer config that points mmdc at the installed Chrome.")

(use-package mermaid-mode
  :mode "\\.\\(mmd\\|mermaid\\)\\'"
  :custom
  ;; `mermaid-flags' is split on spaces, so the path must not contain any.
  (mermaid-flags (format "-p %s --scale 2" ads/mermaid-puppeteer-config)))

(defun ads/mermaid-save-before-compile (&rest _)
  "Save the buffer so mmdc renders what is on screen, not the last save."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(advice-add 'mermaid-compile :before #'ads/mermaid-save-before-compile)

(use-package ob-mermaid
  ;; mmdc comes from the flake's `runtimeTools', so PATH finds it.
  :commands (org-babel-execute:mermaid))

;; mermaid-mode defines its own `org-babel-execute:mermaid', which ignores
;; :puppeteer-config-file and :output-dir.  Load ob-mermaid over it.
(with-eval-after-load 'mermaid-mode
  (load "ob-mermaid" nil :nomessage))

(with-eval-after-load 'ob-mermaid
  (setq org-babel-default-header-args:mermaid
        `((:results . "file")
          (:exports . "results")
          (:file-ext . "svg")
          (:output-dir . "img")
          (:background-color . "transparent")
          (:puppeteer-config-file . ,ads/mermaid-puppeteer-config))))

(with-eval-after-load 'org-tempo
  (tempo-define-template
   "org-mermaid-diagram"
   '("#+name: " p n "#+begin_src mermaid" n> p n "#+end_src")
   "<m"
   "Insert a named mermaid diagram block"
   'org-tempo-tags))

;;; mermaid.el ends here
