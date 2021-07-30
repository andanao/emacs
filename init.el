;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
(package-refresh-contents))



;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)


(scroll-bar-mode -1)  ;; Disable the visible scrollbar
(tool-bar-mode -1)    ;; Disablet the toolbar
(tooltip-mode -1)     ;; Disable tooltips
(set-fringe-mode 10)  ;; Add some breathing room


(menu-bar-mode -1) ; Disable the menu bar

(setq visible-bell t) ; set up the visible bell (no annoying beeping sounds)

;; Set Font sizesb
 (defvar efs/default-font-size 140)
 (defvar efs/default-variable-font-size 140)

;; Set default font
(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch fa
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Disable lines in some modes 
(dolist (mode '(org-mode-hook
	    term-mode-hook
	    eshell-mode-hook))
	(add-hook mode(lambda () (display-line-numbers-mode 0))))

(column-number-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(desktop-save-mode 1)
;(savehist-mode 1)
;(add-to-list 'savehist-additional-variables 'kill-ring) ;; for example

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
   "k" '(:ignore t :wk "kill")
   "e" '(:ignore t :which-key "eval")
   "j" '(:ignore t :which-key "org")

   "ec" '(lambda () (interactive) (
         find-file (concat efs/user-dir-readme))
         :which-key "ORG Config")

   "ei" '(lambda () (interactive) (
         find-file (concat efs/user-dir-emacs "init.el"))
         :which-key "ORG init")

   "eb" '(eval-buffer :which-key "eval-buffer")
   "ep" '(eval-last-sexp :which-key "eval-last-sexp")
   "eo" '(org-mode-restart :which-key "org-mode-restart")


   ;"SPC"  '(normal-mode :which-key "normal-mode")


   "kj" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
   "kk" '(kill-this-buffer :which-key "kill-this-buffer")
   "kl"'(delete-window :wk "delete-window")


   ;"jk" '(org-capture :which-key "org-capture")

   "h" '(:ignore t :which-key "describe")
   "he" 'view-echo-area-messages
   "hf" 'describe-function
   "hF" 'describe-face
   "hk" 'describe-key
   "hK" 'describe-keymap
   "hp" 'describe-package
   "hv" 'describe-variable
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

  ;; Use visual line motions even outside of visual-line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq evil-undo-system 'undo-tree)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package doom-themes
    :init (load-theme 'doom-acario-dark t))

(defvar efs/switch-themes-var
  (let ((themes-list (list 
			    'doom-acario-dark
			    'doom-solarized-light
)))
    (nconc themes-list themes-list))
  "A circular list of themes to keep switching between.
  Make sure that the currently enabled theme is at the head of this
  list always.

  A nil value implies no custom theme should be enabled.")

(defun efs/quick-switch-theme ()
  "Switch between to commonly used faces in Emacs.
One for writing code and the other for reading articles."
  (interactive)
  (if-let* ((next-theme (cadr efs/switch-themes-var)))
      (progn (when-let* ((current-theme (car efs/switch-themes-var)))
               (disable-theme (car efs/switch-themes-var)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq efs/switch-themes-var (cdr efs/switch-themes-var)))


(efs/leader-keys
    "tt" '(efs/quick-switch-theme :wk "toggle theme"))

(use-package all-the-icons)

;; Doom modeline config
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
)

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

;    (use-package ivy-prescient
;      :after counsel
;      :custom
;      (ivy-prescient-enable-filtering nil)
;      :config
      ;; Uncomment the following line to have sorting remembered across sessions!
;      (prescient-persist-mode 1)
;      (ivy-prescient-mode 1))

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

(use-package hydra
 :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))


(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

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
  :hook (after-init . global-emojify-mode))
(add-hook 'after-init-hook #'global-emojify-mode)

(efs/leader-keys
  ";" '(emojify-insert-emoji :wk "insert emoji")
)

(defun efs/org-font-setup ()
    ;; Replace list hyphen with dots
    (font-lock-add-keywords 'org-mode
        '(("^ *\\([-]\\) "
            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Set faces for heading levels
    (dolist 
        (face '((org-level-1 . 1.2)
            (org-level-2 . 1.1)
            (org-level-3 . 1.05)
            (org-level-4 . 1.0)
            (org-level-5 . 1.0)
            (org-level-6 . 1.0)
            (org-level-7 . 1.0)
            (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
	  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
    (interactive)
    ;; (org-indent-mode t)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (efs/org-font-setup))

(efs/leader-keys
  "of" '(efs/org-mode-setup :wk "org mode setup fn")
)

(efs/leader-keys
    "o" '(:ignore t :wk "org")
    "oc" '(org-capture :wk "capture")
    "oj" '(org-store-link :wk "org-store-link")
    "oi" '(org-insert-last-stored-link :wk "org-insert-last-stored-link")
)

(use-package org
	:config
	(setq org-ellipsis " ▾")
	(setq org-directory efs/user-dir-org)
	(setq org-agenda-start-with-log-mode t)
	(setq org-log-done 'time)
	(setq org-log-into-drawer t)


	(setq org-hide-emphasis-markers t)


	;:hook (org-mode . efs/org-mode-setup)
	(require 'org-habit)
	(add-to-list 'org-modules 'org-habit)
	(setq org-habit-graph-column 60)

	(setq org-todo-keywords
	  '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d!)")
	    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

	(setq org-refile-targets
	  '(("archive.org" :maxlevel . 1)
	    ("personal.org" :maxlevel . 1)
	    ("work.org" :maxlevel . 1)))

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


;;	(efs/org-font-setup)
	(org-indent-mode t)
	(variable-pitch-mode 1)
	(visual-line-mode 1)

	(efs/org-font-setup))

(efs/leader-keys
    "oa"'(org-agenda :wk "org-agenda") 
  )
(if (string= efs/computer-id "work") 
    (setq org-agenda-files
        (list 
         (concat efs/user-dir-org "work.org")
         (concat efs/user-dir-org "dates.org")
         (concat efs/user-dir-org "inbox.org")
         ))

    ((setq org-agenda-files
        (list 
         (concat efs/user-dir-org "personal.org")
         (concat efs/user-dir-org "habits.org")
         (concat efs/user-dir-org "dates.org")
         (concat efs/user-dir-org "inbox.org")
         )))
)

(setq org-capture-templates
   '( 
    ("t" "Task" entry 
	(file+headline (lambda () (concat efs/user-dir-org "inbox.org"))"Inbox")
"* TODO %^{Task}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:REF:\t%a
%i
:END:
"
  :kill-buffer t)
    ("w" "Work Task" entry 
	(file+headline (lambda () (concat efs/user-dir-org "work.org"))"Tasks")
"* TODO %^{Work Task}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:REF:\t%a
%i
:END:
"
  :kill-buffer t)

    ("p" "Personal Task" entry 
	(file+headline (lambda () (concat efs/user-dir-org "personal.org"))"Tasks")
"* TODO %^{Personal Task}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:REF:\t%a
%i
:END:
"
  :kill-buffer t)




    ("i" "Quick Inbox" entry 
	(file+headline (lambda () (concat efs/user-dir-org "inbox.org"))"Inbox")
"* TODO %^{Task to inbox}\n
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:REF:\t%a
%i
:END:
"
  :immediate-finish t
  :kill-buffer t)



    ("k" "Clipboard Link to Inbox" entry 
	(file+headline (lambda () (concat efs/user-dir-org "inbox.org"))"Inbox")
"* TODO %(org-cliplink-capture)\n
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:REF:\t%a
%i
:END:
"
  :immediate-finish t
  :kill-buffer t)


    ("m" "CAD Model" entry 
	(file+headline (lambda () (concat efs/user-dir-org "personal.org"))"CAD")
"* TODO %^{Thing to CAD}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
"
  :kill-buffer t)
    ("P" "Project" entry 
	(file+headline (lambda () (concat efs/user-dir-org "personal.org"))"Projects")
"* TODO %^{Project Name}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U
:REF:\t%a 
:Effort: \t%^{effort|1:00|2:00|4:00|8:00|16:00}
:Cost-est:\t%^{Cost estimate}
%i
:END:
  "
  :kill-buffer t)
    ("B" "Book" entry 
	(file+headline (lambda () (concat efs/user-dir-org "books.org"))"Endless Pile")
"* PILE %^{Book Title}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U 
:AUTHOR:
:RECCOMMENDER:
:END:
  "
  :kill-buffer t)
    ("c" "config" entry 
	(file+headline (lambda () (concat efs/user-dir-org "personal.org"))"EMACS")
"* TODO %^{Config changes}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U 
:END:
  "
  :kill-buffer t)
    ("p" "Purchase" entry 
	(file+headline (lambda () (concat efs/user-dir-org "personal.org" ))"Purchase")
"* TODO %^{What the fuck do you want now?}\n
%?
:PROPERTIES:
:ID:     \t%(org-id-new)
:CREATED:\t%U 
:Cost:\t%^{Cost}
:END:
  "
  :kill-buffer t)





))

;; Configure custom agenda views
(setq org-agenda-custom-commands
	 '(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
	      ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
	      ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

(use-package org-bullets
       :after org
       :hook (org-mode . org-bullets-mode)
       :custom
       (org-bullets-bullet-lits '(("◉" "○" "●" "○" "●" "○" "●"))))

(defun efs/org-mode-visual-fill ()
     (setq visual-fill-column-width 100
	 visual-fill-column-center-text t)
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

      (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("py" . "src python")))

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

(defun efs/org-babel-tangle-config ()
  (when
      (string= buffer-file-name efs/user-dir-readme)
      (let ((org-confirm-babel-evaluate nil)))
      (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

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

(defun efs/remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match " " nil t))))

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

  (use-package all-the-icons-dired
      :hook (dired-mode . all-the-icons-dired-mode))

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

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
