;;; completion.el --- Vertico, corfu and the completion stack  -*- lexical-binding: t; -*-
;;; Commentary:
;; cape, consult, corfu, marginalia, orderless, vertico
;;; Code:

(use-package cape
  :bind ("M-p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package consult
  :demand t
  :config
  (general-define-key
   :states '(normal hybrid motion visual operator emacs)
   '"M-y" 'consult-yank-pop
   '"C-s" 'consult-line))
(ads/leader-keys
  "C-SPC" 'consult-buffer
  "SPC" 'consult-buffer
  "C-;" 'consult-register-store
  "C-r" 'consult-ripgrep
  "b" 'consult-bookmark)

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :custom
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.0 . 0.2))

  :config

  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package marginalia
  :ensure t
  :demand t
  :after (vertico orderless)
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :demand t
  :hook (after-init . vertico-mode)
  :config
  ;; Guarded because reloading init re-runs this, and toggling a global mode
  ;; while a minibuffer is open warns.
  (unless (bound-and-true-p vertico-multiform-mode)
    (vertico-multiform-mode 1))
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (general-define-key
   :states '(insert)
   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous))

;;; completion.el ends here
