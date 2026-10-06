;;; cjk.el --- CJK faces by role, tagged onto runs of Han text  -*- lexical-binding: t; -*-
;;; Commentary:
;; cjk fonts per role (mono, serif, sans), tagging CJK runs by context
;;; Code:

;; Why this is a minor mode and not `set-fontset-font'.  Emacs resolves a CJK
;; character through the fontset *before* the face's :family, so a fontset
;; rule such as (set-fontset-font t 'han "X") overrides every face at once and
;; code and prose could never get different CJK fonts.  A fontset set on a
;; single face through `:font' does not work either (measured: it falls back
;; to the default).  What does work is no han rule at all and :family on the
;; CJK text itself, with that face first in the face list.
;;
;; Text that carries no tag - the minibuffer, the modeline, a terminal -
;; falls to the system's CJK font.  An appended fontset rule cannot give it a
;; default: the system fallback wins over it (measured), and a prepended one
;; would override the tags.

(defface ads/cjk-mono  '((t)) "CJK in fixed-pitch text: code, src blocks, tables.")
(defface ads/cjk-serif '((t)) "CJK in proportional prose set in the serif.")
(defface ads/cjk-sans  '((t)) "CJK in proportional prose set in the sans.")

;; Simplified Chinese cuts: han glyphs differ by language, and a Japanese face
;; cannot draw simplified-only forms such as 汉.  The first of each is pinned
;; by the flake; the rest are what a machine without it might have.
(defvar ads/cjk-mono-stack
  '("Maple Mono NF CN" "Noto Sans Mono CJK SC" "PingFang SC")
  "CJK families for code, best first.  Wants exactly 2:1 against the mono.")

(defvar ads/cjk-serif-stack
  '("LXGW WenKai" "Noto Serif CJK SC" "Songti SC")
  "CJK families for serif prose, best first.  A Kai face leads.")

(defvar ads/cjk-sans-stack
  '("Noto Sans CJK SC" "PingFang SC")
  "CJK families for sans prose, best first.")

(defun ads/cjk-apply (&optional frame)
  "Point each CJK face at the first family in its stack this machine has.
Run per frame for the same reason as `ads/apply-fonts': `font-family-list'
answers nil until a frame exists."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (pcase-dolist (`(,face . ,stack) `((ads/cjk-mono  . ,ads/cjk-mono-stack)
                                         (ads/cjk-serif . ,ads/cjk-serif-stack)
                                         (ads/cjk-sans  . ,ads/cjk-sans-stack)))
        (when-let* ((family (ads/font-first stack)))
          (set-face-attribute face nil :family family))))))

(add-hook 'after-make-frame-functions #'ads/cjk-apply)
(ads/cjk-apply)

;;;; Choosing a role for a run of text

(defconst ads/cjk-regexp "[　-〿぀-ヿ㐀-䶿一-鿿＀-￯]+"
  "A run of CJK: punctuation, kana, extension A, the main Han block, full-width.")

(defun ads/cjk--fixed-face-p (face &optional depth)
  "Non-nil if FACE is, or inherits from, a fixed-pitch face.
This is how org marks src blocks, tables, verbatim and inline code."
  (let ((depth (or depth 0)))
    (cond
     ((> depth 6) nil)
     ((memq face '(fixed-pitch fixed-pitch-serif modus-themes-fixed-pitch)) t)
     ((not (and face (symbolp face) (facep face))) nil)
     (t (let ((inh (face-attribute face :inherit nil t)))
          (seq-some (lambda (f) (ads/cjk--fixed-face-p f (1+ depth)))
                    (if (listp inh) inh (list inh))))))))

(defun ads/cjk--prose-family ()
  "Latin family this buffer's prose is set in, from `buffer-face-mode'."
  (let ((f buffer-face-mode-face))
    (cond ((and (consp f) (plist-get f :family)))
          ((and f (symbolp f)) (face-attribute f :family nil t)))))

(defun ads/cjk--role (pos)
  "Face for the CJK run starting at POS.
Code - anything inheriting fixed-pitch, or any buffer not in `buffer-face-mode'
- is mono.  Prose takes the sans face when its Latin is set in one of
`ads/sans-stack' (agent-shell's transcript) and the serif face otherwise
(org and markdown, which use `variable-pitch')."
  (cond
   ((seq-some #'ads/cjk--fixed-face-p (ensure-list (get-text-property pos 'face)))
    'ads/cjk-mono)
   ((not (bound-and-true-p buffer-face-mode)) 'ads/cjk-mono)
   ((member (ads/cjk--prose-family) ads/sans-stack) 'ads/cjk-sans)
   (t 'ads/cjk-serif)))

(defconst ads/cjk--keywords
  `((,ads/cjk-regexp 0 (ads/cjk--role (match-beginning 0)) prepend)))

(define-minor-mode ads/cjk-mode
  "Tag runs of CJK text with the face for their role."
  :lighter nil
  ;; Remove before adding: a positive call on a buffer where the mode is
  ;; already on re-runs this body, and the keyword would be added twice.
  (font-lock-remove-keywords nil ads/cjk--keywords)
  (when ads/cjk-mode
    (font-lock-add-keywords nil ads/cjk--keywords 'append))
  (when font-lock-mode (font-lock-flush)))

(define-globalized-minor-mode ads/cjk-global-mode ads/cjk-mode
  (lambda () (unless (minibufferp) (ads/cjk-mode 1))))

(ads/cjk-global-mode 1)

;;; cjk.el ends here
