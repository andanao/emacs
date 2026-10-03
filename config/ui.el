;;; ui.el --- Icons, ligatures, padding, scrolling and other chrome  -*- lexical-binding: t; -*-
;;; Commentary:
;; all-the-icons, all-the-icons-ibuffer, default-text-scale, emojify,
;; helpful, ident-bars, ligature, nerd-icons, rainbow-delimiters,
;; rainbow-mode, spacious-padding, ultra-scroll, visual-fill-column,
;; which-key, zoom
;;; Code:

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package default-text-scale
  :after transient
  :config
  (transient-define-prefix ads/text-scale-transient ()
    "Text scaling commands."
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-quit-one
    [["Buffer"
      ("j" "increase" text-scale-increase)
      ("k" "decrease" text-scale-decrease)
      ("h" "reset" ads/text-scale-reset :transient nil)]
     ["Global"
      ("J" "increase" default-text-scale-increase)
      ("K" "decrease" default-text-scale-decrease)
      ("H" "reset" default-text-scale-reset :transient nil)]])
  (defun ads/text-scale-reset ()
    "Reset the current buffer's text scale."
    (interactive)
    (text-scale-set 0))
  (ads/leader-keys "ts" 'ads/text-scale-transient))

(use-package emojify
  ;; :hook (after-init . global-emojify-mode)
  :custom
  (use-default-font-for-symbols nil)
  (emojify-emoji-styles '(unicode))
  (emojify-display-style 'unicode)
  :config
  (if (eq system-type 'windows-nt)
      (print "emojis skipped on windows")
    (add-hook 'after-init-hook 'global-emojify-mode))

  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
    (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'prepend))
  ;; (when (member "OpenMoji" (font-family-list))
  ;;   (set-fontset-font t 'symbol "OpenMoji" nil 'prepend)
  ;;   (set-fontset-font t 'emoji "OpenMoji" nil 'prepend))
  (ads/leader-keys
    "ie" 'emojify-insert-emoji))

(use-package helpful
  :demand t
  )

(general-define-key
  :states '(normal insert)
  "C-h C-v" 'describe-variable
  "C-h C-f" 'describe-function
  "C-h C-b" 'describe-bindings
  "C-h C-c" 'describe-key-briefly
  "C-h C-k" 'describe-key
  "C-h C-e" 'view-echo-area-messages
  "C-h C-j" 'describe-face)

(defun indent-bars-mode-unless-org-src-fontification ()
  "Enable `indent-bars-mode' outside of org src fontification buffers."
  (unless (string-prefix-p " *org-src-fontification:" (buffer-name))
    (indent-bars-mode 1)))

(use-package indent-bars
  :hook ((prog-mode) . indent-bars-mode-unless-org-src-fontification))

(defvar ads/org-ligature-sequences
  '(;; arrows
    "<--->" "<===>" "<-->" "<==>" "<---" "--->" "<===" "===>"
    "<--" "-->" "<==" "==>" "<->" "<=>" "<-" "->" "=>"
    "->>" "=>>" ">>=" "=<<" "<<-" "<<=" "-<" ">-"
    ;; tildes
    "~~>" "<~~" "~>" "<~" "~=" "~~"
    ;; comparison
    "<=" ">=" "!=" "!==" "==" "===" "=/="
    ;; punctuation and misc
    "..." ".." "::" ":=" "++" "--" "<>" "</>" "|>" "<|" "www")
  "Sequences ligated in org buffers, prose included.")

(defun ads/org-ligatures ()
  "Render `ads/org-ligature-sequences' in `fixed-pitch' so FiraCode ligates them."
  ;; regexp-opt rather than hand-ordered alternation: it prefers the longest
  ;; match, so "<-->" cannot be eaten by "<--" leaving a stray ">".
  (font-lock-add-keywords
   nil
   `((,(regexp-opt ads/org-ligature-sequences) 0 'fixed-pitch prepend))
   t))
(add-hook 'org-mode-hook #'ads/org-ligatures)

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     "://" "//" "/*" "*/" "<-" "->" "=>" "<=" ">=" "!=" "==" "==="
     "&&" "||" "++" "--" "::" ":=" "<>" "<<" ">>" "..." ".." "?."
     "|>" "<|" "=>>" "<<=" "www"))
  (ligature-set-ligatures 'org-mode ads/org-ligature-sequences)
  (global-ligature-mode t))

(use-package nerd-icons
  :ensure t)

(defun ads/nerd-icons-select ()
  "Select and return a nerd icon"
  (let* ((standard-output (current-buffer))
         (candidates (nerd-icons--read-candidates))
         (prompt    "Icon : ")
         (selection (completing-read prompt candidates nil t)))
        (cdr (assoc selection candidates))))

(ads/leader-keys
   "i" '(:ignore t :wk "insert")
   "ii" 'nerd-icons-insert
   "ic" 'insert-char)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; dirvish draws the dired icons now, in the parent columns too, which
;; nerd-icons-dired can't reach.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package spacious-padding
  :custom
  (spacious-padding-widths
      '(:internal-border-width 15
        :header-line-width 4
        :mode-line-width 6
        :tab-width 4
        :right-divider-width 30
        :scroll-bar-width 0
        :fringe-width 8))
  :config
  (spacious-padding-mode 1))

(use-package ultra-scroll
  :vc (ultra-scroll
       :url "https://github.com/jdtsmith/ultra-scroll"
       :main-file "ultra-scroll.el"
       :branch "main"
       :rev :newest)
  :init
  (setq scroll-conservatively 101
        scroll-margin 2)
  :config
  (when (not (eq system-type 'windows-nt))
    (ultra-scroll-mode 1)))

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 130)
  (global-visual-fill-column-mode t)
  :config
  (ads/leader-keys "tf" 'visual-fill-column-mode)
  )

(use-package which-key
  :demand t
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.5))

(use-package zoom
  :custom
  (zoom-ignored-major-modes '(dired-mode markdown-ts-mode))
  (zoom-ignored-buffer-names '("readme.org" "init.el"))
  (zoom-ignored-buffer-name-regexps '("^*calc"))
  (zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
  (zoom-size '(0.618 . 0.618))
  (zoom-mode nil)
  :config
  (ads/leader-keys
    "tz" 'zoom-mode))

;;; ui.el ends here
