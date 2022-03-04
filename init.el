(add-to-list 'load-path efs/user-dir-emacs)
(add-to-list 'load-path efs/user-dir-org)

(setq debug-on-error t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ))

(package-initialize)
     
(unless package-archive-contents
     (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq-default
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 require-final-newline t                ; Newline at end of file
 scroll-margin 3                        ; Add a margin when scrolling vertically
 scroll-conservatively 10               ; Avoid recentering when scrolling far
 help-window-select t                   ; Focus new help windows when opened
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 visible-bell t                         ; set up the visible bell (no annoying beeping sounds)
 )

(blink-cursor-mode 0)                   ; Prefer a still cursor
(fset 'yes-or-no-p 'y-or-n-p)           ; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ; Iterate through CamelCase words
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
(scroll-bar-mode -1)                    ; Disable the visible scrollbar
(tool-bar-mode -1)                      ; Disablet the toolbar
(tooltip-mode -1)                       ; Disable tooltips
(menu-bar-mode -1)                      ; Disable the menu bar
(set-fringe-mode 10)                    ; Add some breathing room

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(defun efs/browse-url-edge (url)
    (shell-command (concat "start msedge " url)))

(setq mono "Fira Code Retina")
(setq sans "Cantarell")
(setq serif "EtBembo")
;; (setq serif "Garamond")

;; Set Font sizes
(defvar efs/default-font-size 160)
(defvar efs/default-variable-font-size 160)

;Set line spacing
(setq line-spacing .2)

;; Set default font
(set-face-attribute 'default nil :font mono :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font mono :height efs/default-font-size)

;; Set the variable pitch fa
(set-face-attribute 'variable-pitch nil :font serif :height efs/default-variable-font-size :weight 'regular)

;; Enable line numbers
;; (global-display-line-numbers-mode nil)

;; Disable lines in some modes 
(dolist (mode '(prog-mode-hook
		))
	(add-hook mode(lambda () (display-line-numbers-mode 1))))

(column-number-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(desktop-save-mode 1)
;(savehist-mode 1)
;(add-to-list 'savehist-additional-variables 'kill-ring) ;; for example

(global-set-key (kbd "C-x C-c") 'nil)
(global-set-key (kbd "C-x C-z") 'nil)

;;Make ESC quit prompts (why wouldn't you want that?)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
 :after evil
 :config
 (general-create-definer efs/leader-keys
   :keymaps '(normal insert visual emacs)
   :prefix "SPC"
   :global-prefix "C-SPC")
 (efs/leader-keys
   "t" '(:ignore t :wk "toggles")
   "tl" '(toggle-truncate-lines :wk"toggle truncate")
   
   "j" '(:ignore t :which-key "org")
   
   "er" '(lambda () (interactive) 
	   (find-file (concat efs/user-dir-readme))
           :wk "Readme Config")
   
   "ew" '(lambda () (interactive) 
	   (find-file (concat efs/user-dir-org "work-config.org"))
           :wk "Readme Config")
   
   "e." '(lambda () (interactive) 
	   (find-file efs/user-dir-config)
           :wk ".emacs Config")
   
   "ei" '(lambda () (interactive) (find-file (concat efs/user-dir-emacs "init.el"))
           :which-key "ORG init")
   
   "e" '(:ignore t :whch-key "eval")
   "eb" '(eval-buffer :which-key "eval-buffer")
   "ep" '(eval-last-sexp :which-key "eval-last-sexp")
   "eo" '(org-mode-restart :which-key "org-mode-restart")
   
   "k" '(:ignore t :wk "kill")
   "kj" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
   "kk" '(kill-this-buffer :which-key "kill-this-buffer")
   "kl"'(delete-window :wk "delete-window")
   ))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-w-delete nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "q") 'nil)
  ;; Use visual line motions even outside of visual-line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Use C-<hjkl> to move around in insert 
  (evil-global-set-key 'insert (kbd "C-h") 'evil-backward-char)
  (evil-global-set-key 'insert (kbd "C-l") 'evil-forward-char)
  (evil-global-set-key 'insert (kbd "C-k") 'evil-previous-line)
  (evil-global-set-key 'insert (kbd "C-j") 'evil-next-line)

  ;; beginning and end of line
  (evil-global-set-key 'normal (kbd "gl") 'evil-end-of-visual-line)
  (evil-global-set-key 'normal (kbd "gL") 'evil-end-of-line)
  (evil-global-set-key 'normal (kbd "gh") 'evil-beginning-of-visual-line)
  (evil-global-set-key 'normal (kbd "gH") 'evil-beginning-of-line)
  
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(efs/leader-keys
  "C-<escape>" 'kill-emacs)

(add-hook 'after-save-hook 'evil-normal-state)

(evil-global-set-key 'normal (kbd "C-M-j")  'counsel-switch-buffer)

(evil-global-set-key 'normal (kbd "C-w C-h") 'evil-window-left)
(evil-global-set-key 'insert (kbd "C-w C-h") (lambda () (interactive)
					       ;; (normal-mode)
					       (call-interactively 'evil-window-left)))


(evil-global-set-key 'normal (kbd "C-w C-j") 'evil-window-down)
(evil-global-set-key 'insert (kbd "C-w C-j") (lambda () (interactive)
					       ;; (normal-mode)
					       (call-interactively 'evil-window-down)))


(evil-global-set-key 'normal (kbd "C-w C-k") 'evil-window-up)
(evil-global-set-key 'insert (kbd "C-w C-k") (lambda () (interactive)
					       ;; (normal-mode)
					       (call-interactively 'evil-window-up)))


(evil-global-set-key 'normal (kbd "C-w C-l") 'evil-window-right)
(evil-global-set-key 'insert (kbd "C-w C-l") (lambda () (interactive)
					       ;; (normal-mode)
					       (call-interactively 'evil-window-right)))

(evil-global-set-key 'normal (kbd "C-w C-e") 'balance-windows)
(evil-global-set-key 'insert (kbd "C-w C-e") (lambda () (interactive)
					       ;; (normal-mode)
					       (call-interactively 'balance-windows)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq evil-undo-system 'undo-tree)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(evil-global-set-key 'normal (kbd "C-x C-u") 'undo-tree-visualize)

(defun efs/remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match " " nil t))))

(defun efs/format-dir-winstyle ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\\" nil t) (replace-match "\\\\" nil t))))

(efs/leader-keys
    "r"  '(:ignore t :wk "replace")
    "rr" 'replace-regexp
    "rn" '(efs/remove-newlines-in-region :wk "remove new lines")
    "rs" '(efs/format-dir-winstyle :wk "format dir style windows")
)

(evil-global-set-key 'normal (kbd "<insert>") 'comment-line)
(evil-global-set-key 'insert (kbd "<insert>") 'comment-line)
(evil-global-set-key 'visual (kbd "<insert>") 'comment-line)

(setq custom-theme-directory efs/user-dir-emacs)

(defvar efs/switch-themes-var
  (let ((themes-list (list 
			    'ads-dark
			    'ads-light
)))
    (nconc themes-list themes-list))
  "A circular list of themes to keep switching between.
  Make sure that the currently enabled theme is at the head of this
  list always.

  A nil value implies no custom theme should be enabled.")

(use-package doom-themes
    :init 
(load-theme (car efs/switch-themes-var) t)
    )

(defun efs/quick-switch-theme ()
  "Switch between to commonly used faces in Emacs.
One for writing code and the other for reading articles."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (if-let* ((next-theme (cadr efs/switch-themes-var)))
      (progn (when-let* ((current-theme (car efs/switch-themes-var)))
               (disable-theme (car efs/switch-themes-var)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq efs/switch-themes-var (cdr efs/switch-themes-var))
  )


(efs/leader-keys
    "tt" '(efs/quick-switch-theme :wk "toggle theme"))

(defun efs/refresh-theme ()
  (interactive)
  (load-theme (car custom-enabled-themes) t)
)
(efs/leader-keys "tj" 'efs/refresh-theme)

(defun efs/refresh-theme-auto()
  (when (cl-search
     (symbol-name (car custom-enabled-themes))
     (file-name-base buffer-file-name)) 
    (efs/refresh-theme)))
(add-hook 'after-save-hook #'efs/refresh-theme-auto)

(use-package all-the-icons)

;; Doom modeline config
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
)

(setq frame-title-format "%b")

(use-package which-key
 :defer 0
 :diminish which-key-mode
 :config
 (which-key-mode)
 (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
      :map minibuffer-local-map
      ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy
 :diminish ;; Hides from the mode line
 :bind (("C-s" . swiper)
      :map ivy-minibuffer-map
      ("TAB" . ivy-alt-done)
      ("C-l" . ivy-alt-done)
      ("C-j" . ivy-next-line)
      ("C-k" . ivy-previous-line)
      :map ivy-switch-buffer-map
      ("C-k" . ivy-previous-line)
      ("C-l" . ivy-done)
      ("C-d" . ivy-switch-buffer-kill)
      :map ivy-reverse-i-search-map
      ("C-k" . ivy-previous-line)
      ("C-d" . ivy-reverse-i-search-kill))
 :config
 (ivy-mode 1))


(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
   ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
     :ensure t
     :commands (helpful-callable helpful-variable helpful-command helpful-key)
     :custom
       (counsel-describe-function-function #'helpful-callable)
       (counsel-describe-variable-function #'helpful-variable)
     :bind
       ([remap describe-function] . counsel-describe-function)
       ([remap describe-command] . helpful-command)
       ([remap describe-variable] . counsel-describe-variable)
       ([remap describe-key] . helpful-key))

(global-set-key (kbd "C-h C-v") 'describe-variable)
(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-b") 'describe-bindings)
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)
(global-set-key (kbd "C-h C-k") 'describe-key)
(global-set-key (kbd "C-h C-e") 'view-echo-area-messages)
(global-set-key (kbd "C-h C-j") 'describe-face)

(use-package browse-kill-ring
  :ensure t
  :config
  ;; (browse-kill-ring-default-keybindings) ended up setting this myself down below same thing but I know what idiot set it
  (setq browse-kill-ring-highlight-current-entry t)
  )
(global-set-key (kbd "M-y") 'browse-kill-ring)

;; I like my evil bindings
(define-key browse-kill-ring-mode-map (kbd "j") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "k") 'browse-kill-ring-previous)

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 2)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defhydra hydra-window-adjust (:timeout 5)
  ("k" (evil-window-increase-height 5) "increase height")
  ("j" (evil-window-decrease-height 5) "decrease height")
  ("h" (evil-window-increase-width 10) "increase width")
  ("l" (evil-window-decrease-width 10) "decrease width")
  ("e" balance-windows "balance windows")
  ("f" nil "finished" :exit t)
  )

(efs/leader-keys "C-w" '(hydra-window-adjust/body :wk "hydra window adjust"))

(use-package evil-smartparens)

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))
(require 'smartparens-config)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(show-paren-mode t)

(use-package emojify
  :hook (after-init . global-emojify-mode)
  ;; (setq emojify-emoji-styles '(ascii unicode))
  )
(add-hook 'after-init-hook #'global-emojify-mode)

(setq emojify-emoji-styles '(ascii unicode))
(efs/leader-keys
       ";" '(emojify-insert-emoji :wk "insert emoji")
     )

(defun efs/org-mode-setup ()
      (interactive)
      ;; (org-indent-mode t)
      (variable-pitch-mode 1)
      (visual-line-mode 1)
      (setq org-image-actual-width (/ (car (window-text-pixel-size)) 1.5))
      ;; (org-redisplay-inline-images)
      ;; Replace list hyphen with dots
      (font-lock-add-keywords 'org-mode
			      '(("^ *\\([-]\\) "
				 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
      )


(add-hook 'org-mode-hook 'efs/org-mode-setup)

(efs/leader-keys "of" '(efs/org-mode-setup :wk "org mode setup fn"))

(efs/leader-keys
    "o" '(:ignore t :wk "org")
    "oc" '(org-capture :wk "capture")
    "oj" '(org-store-link :wk "org-store-link")
    "oi" '(org-insert-last-stored-link :wk "org-insert-last-stored-link")
)

(use-package org
     :config
     (setq org-ellipsis " ▾ "
	   org-directory efs/user-dir-org
	   org-agenda-start-with-log-mode t
	   org-log-done 'time
	   org-log-into-drawer t
	   org-pretty-entities t
	   org-pretty-entities-include-sub-superscripts nil 
	   org-hidden-keywords '(title) 
	   org-hide-emphasis-markers t
	   org-src-preserve-indentation t
	   org-image-actual-width (/ (car (window-text-pixel-size)) 2)
	   org-startup-with-inline-images t
	   org-startup-indented t
	   org-startup-folded t
	   org-agenda-block-separator ""
	   org-fontify-whole-heading-line t
	   org-fontify-done-headline t
	   org-fontify-quote-and-verse-blocks t
	   org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
	   org-cycle-separator-lines 0
	   org-blank-before-new-entry '((heading . nil)
					(plain-list-item . nil))
	   )

     ;:hook (org-mode . efs/org-mode-setup)
     (require 'org-habit)
     (add-to-list 'org-modules 'org-habit 'org-checklist)
     (setq org-habit-graph-column 60)

     (setq org-todo-keywords
       '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d!)")
	 (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)"
		   "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

     ;; Save Org buffers after refiling!
     (advice-add 'org-refile :after 'org-save-all-org-buffers)

     (setq org-tag-alist
       '((:startgroup)
	  ; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

     (org-indent-mode t)
     (variable-pitch-mode 1)
     (visual-line-mode 1)
     )

(defun efs/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which
do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))


;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (add-hook 'before-save-hook
;; 	    'efs/org-add-ids-to-headlines-in-file nil 'local)))

(defun efs/copy-id-to-clipboard()
  "Copy the ID property value
to killring, if no ID is there then create a new unique ID.
This function works only in org-mode buffers.
The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
  (interactive)
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
    (kill-new mytmpid)
    (message "Copied %s to killring (clipboard)" mytmpid)
    ))
(efs/leader-keys "C-l" '(efs/copy-id-to-clipboard :wk "ID to clipboard"))

(setq efs/conf-task-file (concat efs/personal-dir-org "conf-tasks.org"))
(setq efs/work-task-file (concat efs/user-dir-org "work.org"))
  (if (string= efs/computer-id "personal") 
	(setq org-refile-targets
	      (list
		'("personal.org" :maxlevel . 1)
		'(efs/conf-task-file :maxlevel . 1))))


    (if (string= efs/computer-id "work") 
	(setq org-refile-targets
	      (list
		'(efs/work-task-file :maxlevel . 1)
		'(efs/conf-task-file :maxlevel . 1))))

	    ;; Save Org buffers after refiling!
	;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-archive-location "archive.org::datetree/")
(efs/leader-keys 
    "C-a" '(org-archive-subtree :wk "org-archive-subtree"))

(use-package org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)
(setq  org-appear-autolinks t)
(setq org-appear-autoentities t)
(setq org-appear-autosubmarkers t)
(setq org-appear-autokeywords t)

(add-hook 'evil-insert-state-exit-hook 
	  (lambda ()
	    (setq org-appear-delay 2)))

(add-hook 'evil-insert-state-entry-hook 
	  (lambda ()
	    (setq org-appear-delay .3)))

(setq org-agenda-files (list 
    (concat efs/personal-dir-org "dates.org")
    (concat efs/personal-dir-org "inbox.org")
    ;(concat efs/personal-dir-org "conf-tasks.org")
))

(if (string= efs/computer-id "work") 
    (add-to-list 'org-agenda-files  
	(concat efs/user-dir-org "work.org")))

(if (string= efs/computer-id "personal") 
    (progn
	(add-to-list 'org-agenda-files  
	      (concat efs/personal-dir-org "habits.org"))
	(add-to-list 'org-agenda-files
	      (concat efs/personal-dir-org "personal.org"))))

(defun efs/org-agenda-open ()
    (interactive)
    (evil-window-split)
    (evil-window-down 1)
    (org-agenda nil "a"))
(efs/leader-keys
      "oa"'(efs/org-agenda-open :wk "org-agenda"))

(defun efs/org-agenda-quit ()
    (interactive)
    (org-agenda-quit)
    (delete-window))
(evil-define-key 'motion org-agenda-mode-map
    (kbd "q") 'efs/org-agenda-quit)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 1)
(setq org-agenda-persistent-filter t)

(setq org-agenda-scheduled-leaders '("__ :" "%02d :"))

(defun efs/org-prettify-symbols-alist ()
  ;; I'm not happy with how these look, I'll have to figure out somethings at a future date
  ;; (push '("[ ]" . "☐" ) prettify-symbols-alist)
  ;; (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;; (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . "“") prettify-symbols-alist)
  (push '("#+END_QUOTE" . "”") prettify-symbols-alist)
  (push '("#+begin_quote" . "“") prettify-symbols-alist)
  (push '("#+end_quote" . "”") prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "«") prettify-symbols-alist)
  (push '("#+END_SRC" . "»") prettify-symbols-alist)
  (push '("#+begin_src" . "«") prettify-symbols-alist)
  (push '("#+end_src" . "»") prettify-symbols-alist)
  (push '("#+options:" . "⌥") prettify-symbols-alist)
  (push '("#+RESULTS:" . "🠶") prettify-symbols-alist)
  (push '(":PROPERTIES:" ."⚙" ) prettify-symbols-alist)
  (prettify-symbols-mode))
(add-hook 'org-mode-hook 'efs/org-prettify-symbols-alist)

(setq diary-file (concat efs/user-dir-org "diary.org"))

(setq  org-capture-templates   
   (list  '( "c" "Task" entry 
	(file+headline (lambda () (concat efs/personal-dir-org "inbox.org"))"Inbox")
 "* TODO %^{Task}
 SCHEDULED: %t
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U
 :REF:\t%a
 %i
 :END:
 %?\n
 "
	:kill-buffer t)))

(if (string= efs/computer-id "work") 
(add-to-list  'org-capture-templates   
    '("w" "Work Task" entry 
	 (file+headline (lambda () (concat efs/user-dir-org "work.org"))"Tasks")
 "* TODO %^{Work Task}
 SCHEDULED: %t
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U
 :REF:\t%a
 %i
 :END:
 %?\n
 "
   :kill-buffer t)
))

(add-to-list  'org-capture-templates   
    '("p" "Personal Task" entry 
	 (file+headline (lambda () (concat efs/personal-dir-org "personal.org"))"Tasks")
 "* TODO %^{Personal Task}
 SCHEDULED: %t
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U
 :REF:\t%a
 %i
 :END:
 %?\n
 "
   :kill-buffer t)
)

(add-to-list  'org-capture-templates   
    '("i" "Quick Inbox" entry 
	 (file+headline (lambda () (concat efs/personal-dir-org "inbox.org"))"Inbox")
 "* TODO %^{Task to inbox}
 SCHEDULED: %t\n
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U
 :REF:\t%a
 %i
 :END:
 "
   :immediate-finish t
   :kill-buffer t)
)

(efs/leader-keys
    "C-c" '(lambda () (interactive) (org-capture nil "i") :wk "Capture to Inbox"))

(add-to-list  'org-capture-templates   
    '("k" "Clipboard Link to Inbox" entry 
	 (file+headline (lambda () (concat efs/personal-dir-org "inbox.org"))"Inbox")
 "* TODO %(org-cliplink-capture)
 SCHEDULED: %t\n
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U
 :REF:\t%a
 %i
 :END:
 "
   :immediate-finish t
   :kill-buffer t)
)

(add-to-list  'org-capture-templates   
    '("P" "Project" entry 
	 (file+headline (lambda () (concat efs/personal-dir-org "personal.org"))"Projects")
 "* TODO %^{Project Name}
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U
 :REF:\t%a 
 :Effort: \t%^{effort|1:00|2:00|4:00|8:00|16:00}
 :Cost-est:\t%^{Cost estimate}
 %i
 :END:
 %?\n
   "
   :kill-buffer t)
)

(add-to-list  'org-capture-templates   
    '("B" "Book" entry 
	 (file+headline (lambda () (concat efs/personal-dir-org "books.org"))"Endless Pile")
 "* PILE %^{Book Title}
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U 
 :AUTHOR:
 :RECCOMMENDER:
 :END:
 %?\n
   "
   :kill-buffer t)
)

(add-to-list  'org-capture-templates   
    '("t" "Quote" entry 
	 (file (lambda () (concat efs/personal-dir-org "quotes.org")))
 "* %^{Quote or Note}
 :PROPERTIES:
 :ID:     \t%(org-id-new)
 :CREATED:\t%U 
 :SOURCE: %^{Source}
 :REF: \t%a
 :END:
 %?\n
   "
   :kill-buffer t))

(add-to-list  'org-capture-templates   
    '("l" "Log to Archive" entry 
	 (file+datetree (lambda () (concat efs/user-dir-org "archive.org")) )
 "* %U
:PROPERTIES:
:ID:\t%(org-id-new)
:REF:\t%a
:FILE:\t%f
:TASK_CLOCK:\t%K
:END:
 \n%?\n
 "
   :kill-buffer t)
)

(defun efs/log-to-archive ()
    (interactive) 
    (org-capture nil "l")
    (evil-insert-state)
    )

(efs/leader-keys
    "l" '(efs/log-to-archive :wk "Log to Archive"))

(add-to-list  'org-capture-templates   
    '("L" "Log to Archive Subject" entry 
	 (file+datetree (lambda () (concat efs/user-dir-org "archive.org")) )
 "* %^{Subject} %U
:PROPERTIES:
:ID:\t%(org-id-new)
:REF:\t%a
:FILE:\t%f
:TASK_CLOCK:\t%K
:END:
 \n%?\n
 "
   :kill-buffer t)
)

(defun efs/log-to-archive-subject ()
    (interactive) 
    (org-capture nil "L")
    (evil-insert-state)
    )

(efs/leader-keys
    "L" '(efs/log-to-archive-subject :wk "Log to Archive"))

(use-package org-bullets
       :after org
       :hook (org-mode . org-bullets-mode)
      )

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 90
	visual-fill-column-center-text t
	visual-fill-column-enable-sensible-window-split t
	;; header-line-format ""
	)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t))))

 (setq org-confirm-babel-evaluate nil)

(with-eval-after-load 'org
 ;; This is needed as of Org 9.2
 (require 'org-tempo)

 (add-to-list 'org-structure-template-alist '("sh" . "src shell\n"))
 (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp\n"))
 (add-to-list 'org-structure-template-alist '("py" . "src python\n"))
 (add-to-list 'org-structure-template-alist '("cc" . "src C\n"))
 (add-to-list 'org-structure-template-alist '("cp" . "src C++\n"))
 (add-to-list 'org-structure-template-alist '("js" . "src js\n"))
 (add-to-list 'org-structure-template-alist '("jj" . "src java\n"))
 )

(global-set-key (kbd "C-c C-'") 'org-edit-special)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-cliplink)
(efs/leader-keys
    "ok" '(org-cliplink :wk "org-cliplink")
)

(setq efs/user-dir-org-screenshot
    (concat efs/user-dir-org "images/screenshot/"))

(defun efs/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)

  (setq org--screenshot-filename
        (concat
         (make-temp-name
          (concat efs/user-dir-org-screenshot
		    (file-relative-name buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

  (shell-command "snippingtool /clip")
  

  (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" org--screenshot-filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))

  (insert (concat "[[file:" org--screenshot-filename "]]"))
  (org-display-inline-images))


(efs/leader-keys 
    "os" '(efs/org-screenshot :wk "org-screenshot")
)

(setq efs/user-dir-org-images
    (concat efs/user-dir-org "images/"))

(defun efs/org-clip-image ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)

  (setq org--image-filename
        (concat
         (make-temp-name
          (concat efs/user-dir-org-images
		    (file-relative-name buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

  (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" org--image-filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))

  (insert (concat "[[file:" org--image-filename "]]"))
  (org-display-inline-images))


(efs/leader-keys 
    "s" '(efs/org-clip-image :wk "org-insert-clipboard-image")
)

(defun efs/org-find-file ()
    "Find file in efs/user-dir-org"
    (interactive)
    (counsel-find-file efs/user-dir-org)
)
(efs/leader-keys "f" '(efs/org-find-file :wk "Org Find File"))

(add-hook 'org-ctrl-c-ctrl-c-hook 'evil-normal-state)

(defun efs/org-checkbox-next ()
  (interactive)
    (when (org-at-item-checkbox-p)
      (org-toggle-checkbox)
      (org-next-item)
      (evil-normal-state))
  )

;; (add-hook 'org-ctrl-c-ctrl-c-hook 'efs/org-checkbox-next)

(use-package async
  :config
  (defun efs/init-hook ()
    "If the current buffer is 'readme.org' the code-blocks
are tangled."
    (when (string= buffer-file-truename efs/user-dir-readme)
      (async-start
       `(lambda ()
          (require 'org)
	  (let ((org-confirm-babel-evaluate nil)))
          (org-babel-tangle-file ,efs/user-dir-readme))
       (lambda (result)
         (message "Tangled file compiled.")))))
  (add-hook 'after-save-hook 'efs/init-hook))

;; (use-package dired-async
;;   :after async
;;   :config
;;   (dired-async-mode 1))

(defun efs/new-org-note ()
  (interactive)
  (setq input (read-string "Enter new Filename:\t"))
  ;; (setq input "test file NaMe")
  (setq input (replace-regexp-in-string "\s" "-" input))
  (setq input (downcase input))
  (setq input (concat efs/user-dir-org input ".org"))
  (find-file input)
  (evil-insert-state)
  )
(efs/leader-keys
  "on" '(efs/new-org-note :wk "new-org-note")
)

(efs/leader-keys
    "C-s" '(lambda () (interactive) (
    find-file (concat efs/user-dir-org "scratch.org"))
    :wk "ORG Config")
)

(use-package projectile
	:diminish projectile-mode
	:config (projectile-mode)
	:custom ((projectile-completion-system 'ivy))
	:bind-keymap
	("C-c p" . projectile-command-map)
	:init
	(when (file-directory-p "c:/Users/Adrian/Documents/GitHub/") 
	;; Specify folder where you keep your coding projects
	    (setq projectile-project-search-path '("c:/Users/Adrian/Documents/GitHub/")))
	(setq projectile-switch-project-action #'projectile-dired))

    (use-package counsel-projectile
	:after projectile
	:config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  ;; display the diff from git in the same window (may be worth trying different options as well 
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;(use-package evil-magit
;   :after magit)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(global-set-key (kbd "C-x C-g") 'magit-status)

(defun efs/git-commit-all ()
  (interactive)
  (shell-command (concat "git commit -am \"" (read-string "Commit Message:\t") "\"")))

(efs/leader-keys 
    "gc"'(efs/git-commit-all :wk "git commit all"))

(defun efs/git-stage-all ()
  (interactive)
  (shell-command "git stage --all")
  (message 
    (concat "Staging all changed files in: " (file-name-directory buffer-file-name))))
(efs/leader-keys 
    "gs"'(efs/git-stage-all :wk "git stage all"))

(defun efs/prog-mode-configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (setq prettify-symbols-alist '(("lambda" . "λ")
                                 ("->" . "→")
                                 ("->>" . "↠")
                                 ("=>" . "⇒")
                                 ("map" . "↦")
                                 ("/=" . "≠")
                                 ("!=" . "≠")
                                 ("==" . "≡")
                                 ("<=" . "≤")
                                 (">=" . "≥")
                                 ("=<<" . "=≪")
                                 (">>=" . "≫=")
                                 ("<=<" . "↢")
                                 (">=>" . "↣")
                                 ("&&" . "∧")
                                 ("||" . "∨")
                                 ("not" . "¬")))
  (prettify-symbols-mode))

(add-hook 'prog-mode-hook 'efs/prog-mode-configure-prettify-symbols-alist)

(use-package dired
      :ensure nil
      :commands (dired dired-jump)
      :bind (("C-x C-j" . dired-jump))
      :custom ((dired-listing-switches "-agho --group-directories-first"))
      :hook (dired-mode . dired-hide-details-mode)
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
	"h" 'dired-single-up-directory
	"l" 'dired-single-buffer)

       (setq delete-by-moving-to-trash t)
       (setq-default dired-hide-details-mode t))

  (use-package dired-single
      :commands (dired dired-jump))

  (use-package dired-git-info
      :ensure t
      ;:hook (dired-mode . (local-set-key ")" 'dired-git-info-mode))
      :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))
  ;(use-package dired-open
  ;    :commands (dired dired-jump)
  ;    :config
  ;    ;; Doesn't work as expected!
  ;    ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;    (setq dired-open-extensions '(("pdf" . "feh")
				   ;; ("mkv" . "mpv"))))

(setq counsel-find-file-extern-extensions 
        '("xlsx"
          "xls"
          "mkv"
          "mp4"
          "pdf"
          "doc"
          "docx"
          "ppt"
          "pptx"
          "wmv"
          "mp3"))

(with-eval-after-load 'dired
;; C-c l to launch a file in Windows similar to running
;; start "" filename in the console
(defun efs/dired-win-default ()
  (interactive)
  (let ((filename (dired-replace-in-string "/"
                                           "\\"
                                           (dired-get-filename))))

    (w32-shell-execute 1 filename)))
(define-key dired-mode-map (kbd "C-c C-c") 'efs/dired-win-default))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package treemacs
  :ensure t
  :defer t
  ;; :init
  
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  (define-key treemacs-mode-map (kbd "C-c C-p C-a") 'treemacs-add-project-to-workspace)
  (define-key treemacs-mode-map (kbd "C-c C-p C-d") 'treemacs-remove-project-from-workspace)
  (define-key treemacs-mode-map (kbd "C-SPC C-t") 'treemacs-quit)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t

  :config (treemacs-set-scope-type 'Perspectives))

(efs/leader-keys
    "C-t" '(treemacs :wk "treemacs"))

(setq debug-on-error nil)
