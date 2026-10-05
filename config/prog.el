;;; prog.el --- Language servers, formatting, treesit and modes  -*- lexical-binding: t; -*-
;;; Commentary:
;; apheleia, csv mode, flycheck, json, kanata, kdl, lsp-mode, lsp-ui,
;; lsp-rust, lsp-pyright, nix, rust, terraform mode, treesit, treesit-auto,
;; yaml
;;; Code:

(defun ads/rustfmt-command ()
  "Path to a nightly rustfmt, or plain \"rustfmt\" if rustup has no nightly."
  (or (car (last (file-expand-wildcards
                  (expand-file-name "~/.rustup/toolchains/nightly-*/bin/rustfmt"))))
      "rustfmt"))

(defun ads/rustfmt-config-args ()
  "Point rustfmt at the personal config, unless the project has one of its own."
  (unless (locate-dominating-file default-directory "rustfmt.toml")
    (list "--config-path" (expand-file-name "~/.config/rustfmt/rustfmt.toml"))))

(use-package apheleia
  :hook ((json-mode . apheleia-mode)
         (json-ts-mode . apheleia-mode)
         (rust-mode . apheleia-mode)
         (rust-ts-mode . apheleia-mode)
         (emacs-lisp-mode . apheleia-mode)
         (lisp-data-mode . apheleia-mode))
  :config
  ;; Define a jq formatter: read stdin, pretty-print with 2-space indent.
  (setf (alist-get 'jq apheleia-formatters)
        '("jq" "--indent" "2" "-M" "."))
  (setf (alist-get 'json-mode apheleia-mode-alist) 'jq)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'jq)
  ;; Rust: nightly rustfmt so unstable options in rustfmt.toml take effect.
  (setf (alist-get 'rustfmt apheleia-formatters)
        `(,(ads/rustfmt-command)
          "--quiet" "--emit" "stdout"
          (ads/rustfmt-config-args))))

(use-package csv-mode
    :mode "\\.csv\\'"
    :hook
    (csv-mode . csv-align-mode)
    (csv-mode . csv-header-line)
    (csv-mode . hl-line-mode)
    (csv-mode . (lambda ()
                  (visual-fill-column-mode -1)
                  (setq-local truncate-lines t)
                  (face-remap-add-relative 'hl-line :background (ef-themes-get-color-value 'bg-dim))))
    :bind (:map csv-mode-map
                ("C-c C-a" . csv-align-fields)
                ("C-c C-z" . csv-unalign-fields))
    :custom
    (csv-align-style 'auto)
    (csv-align-padding 2)
    (csv-align-max-width 100) ;; a lot of big csv rows
    (csv-separators '("," ";" "\t")))

(use-package flycheck
  :hook ((json-mode . flycheck-mode)
         (json-ts-mode . flycheck-mode)))

(use-package json-mode)

(use-package kanata-kbd-mode
  :vc (:url "https://github.com/chmouel/kanata-kbd-mode/" :rev :newest)
  :mode ("\\.kbd\\'" . kanata-kbd-mode)
  )

(use-package kdl-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :preface
  (defvar ads/lsp-autostart-inhibit-functions nil
    "Predicates consulted before a server is autostarted for a buffer.
If any returns non-nil `ads/lsp-maybe-deferred' does nothing, leaving the
buffer to start its server explicitly.  Run with no arguments in the buffer
that is about to start one.")

  (defun ads/lsp-maybe-deferred ()
    "Start a server for this buffer unless something has claimed it."
    (unless (run-hook-with-args-until-success 'ads/lsp-autostart-inhibit-functions)
      (lsp-deferred)))

  (defun ads/lsp-skip-missing-root (fn dir &rest args)
    "Don't set a file watch on DIR when it no longer exists.
Deleting a worktree leaves its folder in the session, and the watch walks
the directory before anything notices it is gone."
    (when (file-directory-p dir)
      (apply fn dir args)))

  (defun ads/lsp-prune-missing-folders ()
    "Drop workspace folders whose directory has been deleted.
Remote folders are left alone: deciding whether one is missing would open
a connection."
    (interactive)
    (if-let* ((session (lsp-session))
              (dead (seq-remove (lambda (folder)
                                  (or (file-remote-p folder)
                                      (file-directory-p folder)))
                                (lsp-session-folders session))))
        (progn (mapc #'lsp-workspace-folders-remove dead)
               (message "Dropped %d missing workspace folder(s): %s"
                        (length dead) (string-join dead ", ")))
      (message "No missing workspace folders")))
  :hook
  (rust-ts-mode . ads/lsp-maybe-deferred)
  (python-ts-mode . ads/lsp-maybe-deferred)
  (c-ts-mode . ads/lsp-maybe-deferred)
  (c++-ts-mode . ads/lsp-maybe-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-file-watchers nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-file-watch-threshold 20000)
  (lsp-warn-no-matched-clients nil)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]external\\'")
  (advice-add 'lsp-watch-root-folder :around #'ads/lsp-skip-missing-root)
  (ads/leader-keys
    :keymaps 'lsp-mode-map
    "l" '(:ignore t :which-key "lsp")
    "lA" 'lsp-execute-code-action
    "ld" 'lsp-find-definition
    "lD" 'lsp-find-declaration
    "le" 'lsp-treemacs-errors-list
    "lF" 'lsp-format-buffer
    "lh" 'lsp-describe-thing-at-point
    "li" 'lsp-find-implementation
    "ln" 'lsp-rename
    "lR" 'lsp-find-references
    "ls" 'lsp-signature-help
    "lS" 'lsp-restart-workspace))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-peek-enable t))

(defun ads/rust-analyzer-command ()
  "Return the command list to start rust-analyzer with.
Prefer `exec-path'; fall back to the newest rustup toolchain that has the
component installed, then to bare \"rust-analyzer\" so lsp-mode reports a
missing server rather than a broken path."
  (list (or (executable-find "rust-analyzer")
            (car (last (file-expand-wildcards
                        (expand-file-name "~/.rustup/toolchains/*/bin/rust-analyzer"))))
            "rust-analyzer")))

(with-eval-after-load 'lsp-rust
  (setq lsp-rust-analyzer-server-command (ads/rust-analyzer-command))
  (setq lsp-rust-analyzer-use-client-watching nil))

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  :init
  (add-hook 'python-ts-mode-hook (lambda () (require 'lsp-pyright)) -50))

(use-package nix-mode
  :config
  (global-nix-prettify-mode))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  ;; :custom
  ;; (rust-format-on-save t)
  ;; :hook
  ;; (rust-mode-hook .(lambda () (setq indent-tabs-mode nil)))
  )

(ads/leader-keys
  :keymaps 'rust-ts-mode-map
  "m" '(:ignore t :which-key "rust")
  "md" 'rust-dbg-wrap-or-unwrap
  "mm" 'rust-toggle-mutability)

(use-package terraform-mode
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust"
              "v0.23.3" "src")
        (python "https://github.com/tree-sitter/tree-sitter-python"
                "v0.23.6" "src")))

(use-package treesit-auto
  :config
  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'rust
                :ts-mode 'rust-ts-mode
                :remap 'rust-mode
                :url "https://github.com/tree-sitter/tree-sitter-rust"
                :revision "v0.23.3"
                :ext "\\.rs\\'"))
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist))

   (setq treesit-auto-langs '(rust python))
   (treesit-auto-add-to-auto-mode-alist '(rust python))
   (global-treesit-auto-mode))

(use-package yaml-mode
  :bind (:map yaml-mode-map
         ("RET" . newline-and-indent)))

;;; prog.el ends here
