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

;; `org-cdlatex-mode' is not on `org-mode-hook': with it on in every agenda
;; file, the first agenda hangs inside `org-element-cache-map'.  Turn it on
;; per buffer with M-x org-cdlatex-mode.
(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
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

(defun ads/region-as-plaintext (start end)
  "Return START to END as plain text, unwrapped and stripped of org markup.
Outside org there is no markup to strip, so the text is only unfilled."
  (require 'ox-ascii)
  (let ((text (if (derived-mode-p 'org-mode)
                  ;; Exporting a string loses the buffer, so an unresolved
                  ;; attachment: link has no directory to look in and errors.
                  (ads/org-region-with-absolute-links start end)
                (buffer-substring-no-properties start end))))
    (if (not (derived-mode-p 'org-mode))
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

(defun ads/yank-region-as-plaintext (start end)
  "Copy START to END as plain text, unwrapped and stripped of org markup.
Outside org there is no markup to strip, so the text is only unfilled."
  (interactive (ads/region-or-buffer))
  (let ((text (ads/region-as-plaintext start end)))
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

;; Markdown has no way to carry an image, so these put HTML or RTFD on the
;; macOS pasteboard instead, each with the plain text beside it for fields
;; that take nothing richer.
(defun ads/org-region-with-absolute-links (start end)
  "Return START to END with attachment: and relative file: links made absolute.
An exported string has lost the buffer, and with it the entry whose
attachment directory a link points into, so resolve them here."
  (require 'org-attach)
  (let ((pos start) pieces)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[\\[\\(attachment\\|file\\):\\([^]\n]+\\)\\]"
                                end t)
        (let* ((beg (match-beginning 0))
               (mend (match-end 0))
               (type (match-string 1))
               (path (match-string 2))
               (abs (ignore-errors
                      (if (equal type "attachment")
                          (org-attach-expand path)
                        (expand-file-name path)))))
          (when abs
            (push (buffer-substring-no-properties pos beg) pieces)
            (push (concat "[[file:" abs "]") pieces)
            (setq pos mend)))))
    (push (buffer-substring-no-properties pos end) pieces)
    (apply #'concat (nreverse pieces))))

(defun ads/org-image-files (text)
  "Return the readable local image files linked as [[file:...]] in TEXT."
  (let ((pos 0) files)
    (while (string-match "\\[\\[file:\\([^]\n]+\\)\\]" text pos)
      (setq pos (match-end 0))
      (let ((file (match-string 1 text)))
        (when (and (file-readable-p file) (image-type-from-file-name file))
          (push file files))))
    (nreverse files)))

(defun ads/html-embed-images (html)
  "Return HTML with each local <img> inlined as a data: URI."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "<img[^>]* src=\"\\(\\(?:file://\\)?\\(/[^\"]+\\)\\)\""
                              nil t)
      (let* ((file (url-unhex-string (match-string 2)))
             (type (and (file-readable-p file)
                        (image-type-from-file-name file))))
        (when type
          (replace-match
           (concat "data:image/" (if (eq type 'svg) "svg+xml" (symbol-name type))
                   ";base64,"
                   (base64-encode-string
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally file)
                      (buffer-string))
                    t))
           t t nil 1))))
    (buffer-string)))

(defconst ads/rich-clipboard-script
  "ObjC.import('AppKit');
function read(path) {
  return $.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null);
}
function run(argv) {
  var html = read(argv[1]), plain = read(argv[2]);
  var pb = $.NSPasteboard.generalPasteboard;
  pb.clearContents;
  if (argv[0] == 'rtfd') {
    var data = html.dataUsingEncoding($.NSUTF8StringEncoding);
    var attr = $.NSAttributedString.alloc.initWithHTMLDocumentAttributes(data, null);
    var rtfd = attr.RTFDFromRangeDocumentAttributes(
      $.NSMakeRange(0, attr.length), $());
    pb.setDataForType(rtfd, 'com.apple.flat-rtfd');
    pb.setStringForType(plain, $.NSPasteboardTypeString);
    return;
  }
  // One item carrying every flavor: an app reads the richest one it knows.
  // Slack ignores images inside HTML but takes image data; Confluence and
  // the rest take the HTML.  Only the first item is ever read, so one image.
  var item = $.NSPasteboardItem.alloc.init;
  item.setStringForType(html, $.NSPasteboardTypeHTML);
  item.setStringForType(plain, $.NSPasteboardTypeString);
  if (argv[3]) {
    var img = $.NSImage.alloc.initWithContentsOfFile(argv[3]);
    if (img.js !== undefined)
      item.setDataForType(img.TIFFRepresentation, $.NSPasteboardTypeTIFF);
  }
  pb.writeObjects($.NSArray.arrayWithObject(item));
}"
  "JXA that puts HTML or RTFD, and plain text, on the pasteboard.
Takes the kind, then the paths of the HTML and plain text files, since an
image-laden HTML string is far over the argument size limit, then optionally
the path of an image to offer as image data.")

(defun ads/yank-region-as-rich (kind start end)
  "Copy START to END to the clipboard with its images, as KIND.
KIND is `html' or `rtfd'.  The plain text goes along for fields that
take nothing richer."
  (unless (eq system-type 'darwin)
    (user-error "Rich clipboard is only implemented for macOS"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Rich clipboard needs an org buffer"))
  (require 'ox-html)
  (let* ((plain (ads/region-as-plaintext start end))
         (image (car (ads/org-image-files
                      (ads/org-region-with-absolute-links start end))))
         ;; An id: link has nowhere to point once it leaves org, so keep its
         ;; text rather than abort the copy over it.
         (html (ads/html-embed-images
                (let ((org-export-with-broken-links t))
                  (org-export-string-as
                   (ads/org-region-with-absolute-links start end)
                   'html t '(:with-toc nil :section-numbers nil)))))
         (html-file (make-temp-file "ads-clip" nil ".html"))
         (plain-file (make-temp-file "ads-clip" nil ".txt"))
         (coding-system-for-write 'utf-8))
    (unwind-protect
        (progn
          (write-region html nil html-file nil 'silent)
          (write-region plain nil plain-file nil 'silent)
          ;; Before the pasteboard is set: `kill-new' would overwrite it.
          (kill-new plain)
          (with-temp-buffer
            (unless (zerop (call-process "osascript" nil t nil
                                         "-l" "JavaScript" "-e"
                                         ads/rich-clipboard-script
                                         (symbol-name kind)
                                         html-file plain-file
                                         (or image "")))
              (user-error "Clipboard script failed: %s"
                          (string-trim (buffer-string)))))
          (message "Copied %d chars as %s with %d images" (length plain)
                   (upcase (symbol-name kind))
                   (with-temp-buffer
                     (insert html)
                     (how-many "src=\"data:" (point-min)))))
      (delete-file html-file)
      (delete-file plain-file))))

(defun ads/yank-region-as-html (start end)
  "Copy START to END as HTML with its images embedded."
  (interactive (ads/region-or-buffer))
  (ads/yank-region-as-rich 'html start end))

(defun ads/yank-region-as-rtfd (start end)
  "Copy START to END as RTFD with its images, for Apple's own apps."
  (interactive (ads/region-or-buffer))
  (ads/yank-region-as-rich 'rtfd start end))

(defconst ads/files-clipboard-script
  "ObjC.import('AppKit');
function run(argv) {
  var pb = $.NSPasteboard.generalPasteboard;
  pb.clearContents;
  var urls = $.NSMutableArray.alloc.init;
  argv.forEach(function (path) {
    urls.addObject($.NSURL.fileURLWithPath(path));
  });
  pb.writeObjects(urls);
}"
  "JXA that puts the files at the given paths on the pasteboard, as Finder does.")

(defun ads/yank-region-images-as-files (start end)
  "Copy the images linked from START to END to the clipboard as files.
Slack takes images only as files, and then ignores any text beside them, so
the text has to be pasted separately."
  (interactive (ads/region-or-buffer))
  (unless (eq system-type 'darwin)
    (user-error "Rich clipboard is only implemented for macOS"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Rich clipboard needs an org buffer"))
  (let ((files (ads/org-image-files
                (ads/org-region-with-absolute-links start end))))
    (unless files
      (user-error "No local images in the region"))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "osascript" nil t nil
                            "-l" "JavaScript" "-e" ads/files-clipboard-script
                            files))
        (user-error "Clipboard script failed: %s"
                    (string-trim (buffer-string)))))
    (message "Copied %d image files" (length files))))

(ads/leader-keys
  "oy" '(:ignore t :wk "yank as")
  "oyf" 'ads/yank-region-images-as-files
  "oym" 'ads/yank-region-as-markdown
  "oyp" 'ads/yank-region-as-plaintext
  "oyh" 'ads/yank-region-as-html
  "oya" 'ads/yank-region-as-rtfd)

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
