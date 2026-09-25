;;; evil.el --- Evil and its companions  -*- lexical-binding: t; -*-
;;; Commentary:
;; evil, evil-anzu, evil-collection, evil-surround
;;; Code:

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-want-integration t)
  (evil-want-keybinding  nil)
  (evil-want-C-u-scroll  nil)
  (evil-want-C-i-jump    nil)
  (evil-want-C-w-delete  nil)
  (evil-complete-all-buffers nil)
  :hook
  (after-init . evil-mode)
  (after-save . evil-normal-state)

  :config
  (general-define-key :states 'insert "C-g" 'evil-normal-state)
  (general-define-key "C-;" 'evil-switch-to-windows-last-buffer)

  ;; Use visual line motions even outside of visual-line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; set back normal mouse behaviour
  (define-key evil-motion-state-map [down-mouse-1] nil)
  ;; unbind q for macros
  (define-key evil-normal-state-map (kbd "q") 'nil)
  (define-key evil-normal-state-map (kbd "Q") 'nil)
  (evil-mode))

(general-define-key
  :states '(normal insert)
  "C-w C-h" 'evil-window-left
  "C-w C-j" 'evil-window-down
  "C-w C-k" 'evil-window-up
  "C-w C-l" 'evil-window-right)

(use-package evil-anzu
  :after (evil)
  :config
  (global-anzu-mode))

(use-package evil-collection
  :after (evil)
  :custom
  (evil-collection-calendar-setup-want-org-bindings t)
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;; evil.el ends here
