;;; text.el --- Prose, markup and search syntax  -*- lexical-binding: t; -*-
;;; Commentary:
;; auctex LaTeX, auto-fill, cdlatex, copy (yank) as markdown, jinx,
;; markdown, markdown keybindings, multiple-cursors, ox-gfm, pcre2el,
;; Evil search arity fix, PCRE input for consult
;;; Code:

(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  (LaTeX-electric-left-right-brace t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t))
(setq-default TeX-master nil)

;; (setq preview-latex-debug t)

;; some of these do apply to linux I just haven't tested it
(when (eq system-type 'windows-nt)
  (setq preview-gs-command "C:/Program Files/gs/gs10.04.0/bin/gswin64c.exe"
	preview-gs-options '("-q" "-dNOPAUSE" "-dDELAYSAFER" "-dBATCH")

	preview-image-type 'dvipng  ; Try dvipng first
	preview-dvipng-command "dvipng"

	preview-prefer-preview-pdf nil  ; Don't prefer PDF preview
	preview-transparent-border 1
	preview-auto-cache-preamble nil
	preview-pdf-color-adjust-method nil))

(customize-set-variable 'fill-column 100)
(add-hook 'text-mode-hook 'auto-fill-mode)

(use-package cdlatex
  :hook ((org-mode . org-cdlatex-mode)
         (LaTeX-mode . turn-on-cdlatex))
  :config
  (with-eval-after-load 'org
    (define-key org-cdlatex-mode-map (kbd "`") nil)
    (define-key org-cdlatex-mode-map (kbd "'") nil)))

(defun ads/region-or-buffer ()
  "Bounds of the active region, or of the whole buffer."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (point-min) (point-max))))

(defun ads/unfill-string (text mode)
  "Return TEXT with hard line breaks inside paragraphs joined up, as MODE."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (funcall mode))
    (if (not (derived-mode-p 'org-mode))
        (let ((fill-column most-positive-fixnum))
          (fill-region (point-min) (point-max)))
      (let (paragraphs)
        (org-element-map (org-element-parse-buffer) 'paragraph
          (lambda (p)
            (push (cons (org-element-property :contents-begin p)
                        (org-element-property :contents-end p))
                  paragraphs)))
        ;; Back to front, so rewriting one never invalidates a later position.
        (dolist (p (sort paragraphs :key #'car :lessp #'>))
          (let* ((beg (car p)) (end (cdr p))
                 (str (buffer-substring-no-properties beg end))
                 (trailing (if (string-suffix-p "\n" str) "\n" "")))
            (delete-region beg end)
            (goto-char beg)
            (insert (replace-regexp-in-string "[ \t]*\n[ \t]*" " "
                                              (string-trim-right str))
                    trailing)))))
    (string-trim (buffer-string))))

(defun ads/org-plain-contents (_element contents _info)
  "Drop the markup around CONTENTS."
  contents)

(defun ads/org-plain-value (element _contents _info)
  "Return ELEMENT's literal value, unquoted and unboxed."
  (org-remove-indentation (org-element-property :value element)))

(defun ads/org-plain-link (link contents _info)
  "Return LINK's description as CONTENTS, or its URL when it has none."
  (or contents (org-element-property :raw-link link)))

(defun ads/org-plain-headline (headline contents info)
  "Render HEADLINE with org's own stars instead of an underline, over CONTENTS."
  (concat (make-string (org-export-get-relative-level headline info) ?*)
          " "
          (org-export-data (org-element-property :title headline) info)
          "\n\n"
          (and contents (concat (string-trim-right contents) "\n\n"))))

(with-eval-after-load 'ox-ascii
  (org-export-define-derived-backend 'ads-plain 'ascii
    :translate-alist '((bold . ads/org-plain-contents)
                       (italic . ads/org-plain-contents)
                       (underline . ads/org-plain-contents)
                       (strike-through . ads/org-plain-contents)
                       (code . ads/org-plain-value)
                       (verbatim . ads/org-plain-value)
                       (link . ads/org-plain-link)
                       (headline . ads/org-plain-headline)
                       (src-block . ads/org-plain-value)
                       (example-block . ads/org-plain-value))))

(defun ads/yank-region-as-plaintext (start end)
  "Copy START to END as plain text, unwrapped and stripped of org markup.
Outside org there is no markup to strip, so the text is only unfilled."
  (interactive (ads/region-or-buffer))
  (require 'ox-ascii)
  (let* ((text (buffer-substring-no-properties start end))
         (text (if (not (derived-mode-p 'org-mode))
                   (ads/unfill-string text major-mode)
                 ;; A finite width: `most-positive-fixnum' is taken literally
                 ;; when underlining a headline and exhausts memory.
                 (let ((org-ascii-text-width 10000)
                       (org-ascii-global-margin 0)
                       (org-ascii-inner-margin 0)
                       (org-ascii-links-to-notes nil))
                   (string-trim
                    (replace-regexp-in-string
                     "\n\\{3,\\}" "\n\n"
                     (org-export-string-as
                      text 'ads-plain t
                      '(:with-toc nil :with-tags nil :section-numbers nil
                        :with-properties nil :with-drawers nil))))))))
    (kill-new text)
    (message "Yanked %d chars as plain text" (length text))))

(defun ads/yank-region-as-markdown (start end)
  "Copy START to END as markdown, joining the hard line breaks back up.
Outside org there is nothing to convert, so the text is only unfilled."
  (interactive (ads/region-or-buffer))
  (require 'ox-gfm)
  (let* ((text (ads/unfill-string (buffer-substring-no-properties start end)
                                  major-mode))
         (text (if (derived-mode-p 'org-mode)
                   (string-trim
                    (org-export-string-as text 'gfm t '(:with-toc nil)))
                 text)))
    (kill-new text)
    (message "Yanked %d chars as markdown" (length text))))

(ads/leader-keys
  "oY" 'ads/yank-region-as-plaintext
  "oy" 'ads/yank-region-as-markdown)

(use-package jinx
  :ensure t
  :defer t
  :custom
  (jinx-languages "en_US")
  :init
  (defun ads/jinx-sweep ()
    "Walk every misspelling in the buffer, leaving jinx as it was found.
`jinx-correct-all' enables `jinx-mode' to do its work and leaves it enabled,
which is wrong when the point is to check once and go back to quiet."
    (interactive)
    (let ((enabled (bound-and-true-p jinx-mode)))
      (unwind-protect (jinx-correct-all)
        (unless enabled (jinx-mode -1)))))

  (ads/leader-keys
    "tJ" 'jinx-mode
    "tj" 'ads/jinx-sweep))

(defun ads/markdown-ts-image-preview ()
  "Fontify image previews, which sit above the default font-lock level."
  (treesit-font-lock-recompute-features '(image-preview))
  (font-lock-flush))

(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :custom
  ;; org-hide-emphasis-markers / org-modern equivalents
  (markdown-ts-hide-markup t)
  (markdown-ts-inline-images t)
  (markdown-ts-ellipsis " ·")
  (markdown-ts-fontify-code-blocks-natively t)
  (markdown-ts-menu-bar-show nil)
  :config
  (require 'markdown-ts-mode-x)
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-hook 'markdown-ts-mode-hook 'variable-pitch-mode)
  (add-hook 'markdown-ts-mode-hook 'ads/markdown-ts-image-preview))

(general-define-key
 :states '(normal) :keymaps '(markdown-ts-mode-map markdown-ts-view-mode-map)
 (kbd "<tab>") 'markdown-ts-outline-cycle
 (kbd "<backtab>") 'outline-cycle-buffer
 "C-j" 'outline-next-heading
 "C-k" 'outline-previous-heading)

(general-define-key
 :states '(motion) :keymaps '(markdown-ts-mode-map markdown-ts-view-mode-map)
 (kbd "RET") 'push-button)

(ads/leader-keys
  :keymaps 'markdown-ts-mode-map
  "dm" 'markdown-ts-view-mode
  "oh" 'consult-outline
  "o TAB" 'outline-cycle-buffer
  "oc" 'markdown-ts-convert
  "ol" 'markdown-ts-insert-structure
  "ot" 'markdown-ts-toc-generate
  "ti" 'markdown-ts-toggle-inline-images
  "tm" 'markdown-ts-toggle-hide-markup
  "ne" 'ads/narrow-section-dwim)

(ads/leader-keys
  :keymaps 'markdown-ts-view-mode-map
  "dm" 'markdown-ts-mode
  "oh" 'consult-outline
  "o TAB" 'outline-cycle-buffer
  "oc" 'markdown-ts-convert
  "ti" 'markdown-ts-toggle-inline-images)

(use-package multiple-cursors
  :custom
  (mc/always-run-for-all t)
  :config
  (define-key mc/keymap (kbd "C-g") 'mc/keyboard-quit))

(use-package ox-gfm
  :ensure t
  :defer t)

(use-package pcre2el
  :demand t
  :hook (prog-mode . rxt-mode)
  :config
  (pcre-mode +1)

  ;; anzu counts matches with `re-search-forward', so it needs the translation too.
  (defun ads/anzu-pcre-transform-input (str)
    "Translate STR from PCRE when `pcre-mode' is searching by regexp."
    (if (and pcre-mode isearch-regexp (stringp str))
        (or (ignore-errors (rxt-pcre-to-elisp str)) str)
      str))
  (advice-add 'anzu--transform-input :filter-return #'ads/anzu-pcre-transform-input))

(with-eval-after-load 'evil
  (ad-remove-advice 'evil-search-function 'around 'pcre-mode)
  (ad-activate 'evil-search-function)

  (defun ads/pcre-evil-search-function (fn &rest args)
    "Read evil's search input as PCRE, outside of isearch.
Isearch gets the translation from `pcre-isearch-search-fun-function'.
FN is the advised `evil-search-function', ARGS its arguments."
    (let ((search-function (apply fn args))
          (regexp-p (nth 1 args)))
      (if (and pcre-mode regexp-p (not isearch-mode))
          (pcre-decorate-search-function search-function)
        search-function)))
  (advice-add 'evil-search-function :around #'ads/pcre-evil-search-function))

(with-eval-after-load 'consult
  (defun ads/consult-pcre-regexp-compiler (input type ignore-case)
    "Compile INPUT, read as PCRE, into a list of regexps of TYPE.
PCRE flavoured `consult--default-regexp-compiler'; a component that
fails to translate (a half-typed regexp, say) passes through as-is."
    (let* ((parts (consult--split-escaped input))
           (elisp (mapcar (lambda (part)
                            (or (ignore-errors (rxt-pcre-to-elisp part)) part))
                          parts)))
      (cons (if (memq type '(emacs basic)) elisp parts)
            (when-let* ((regexps (seq-filter #'consult--valid-regexp-p elisp)))
              (apply-partially #'consult--highlight-regexps regexps ignore-case)))))

  (setq consult--regexp-compiler #'ads/consult-pcre-regexp-compiler))

;;; text.el ends here
