;;; ms-windows.el --- Windows-only configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; MS Windows, server mode, ahk,
;; Window Spy, align windows theme with emacs, auto hide taskbar,
;; browse in edge, org clip image, hide dos eol,
;; exec ~.bat~ in new cmd window, org-attach dir in windows explorer,
;; dired open in windows default, overwrite git-lazy function,
;; projectile caching, projectil shell-global, provide ~ms-windows.el~
;;; Code:

(set-message-beep 'silent)
(setq win/.emacs.d (concat "C:\\Users\\" user-login-name "\\AppData\\Roaming\\.emacs.d\\"))

(ads/leader-def "cW" "Dired .emacs.d" (find-file win/.emacs.d))

(add-hook 'after-init-hook 'server-mode)

(use-package ahk-mode
  :ensure t
  :bind (:map ahk-mode-map
	      ("C-c C-c" . ahk-run-script)
	      ("C-c C-k" . nil)
	      )
  )

(defun ahk-launch-window-spy ()
  (interactive)
  (w32-shell-execute 1 "C:/Users/adanaos/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/AutoHotkey Window Spy.lnk"))

;; win/theme
;;   0 - dark
;;   1 - light
(setq win/theme "0")

(add-to-list 'display-buffer-alist
  (cons "win/theme-toggle" (cons #'display-buffer-no-window nil)))
(defvar win/sync-os-appearance t
  "Whether a theme change should drag Windows light/dark along with it.
The Mac equivalent is off while the Ef themes are being chosen; this one
is untested against them, so it keeps its old behaviour.")

(defun win/theme-align-with-emacs ()
  ;;check if light or dark theme in emacs
  (when win/sync-os-appearance
    (if (modus-themes-color-dark-p (ads/theme-color 'bg-main))
	(setq win/theme "0")
      (setq win/theme "1"))
    (async-shell-command
     (concat
      "powershell New-ItemProperty -Path HKCU:/SOFTWARE/Microsoft/Windows/CurrentVersion/Themes/Personalize -Name AppsUseLightTheme -Value "
      win/theme
      " -Type Dword -Force")
     "win/theme-toggle"
     )))


(add-hook 'ef-themes-after-load-theme-hook 'win/theme-align-with-emacs)

(defun win/taskbar-auto-hide ()
  (interactive)
  (async-shell-command
     "powershell -command
\"&{$p='HKCU:SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StuckRects3';
$v=(Get-ItemProperty -Path $p).Settings;
$v[8]=3;
&Set-ItemProperty -Path $p -Name Settings -Value $v;&Stop-Process -f -ProcessName explorer}\""
     "win/taskbar-auto-hide"))

(defun win/browse-url-edge (url)
    (shell-command (concat "start msedge " url)))

(defun win/org-clip-image ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq temp-image-filename
	  (make-temp-file
	   (concat
	    (file-relative-name buffer-file-name)
	    (format-time-string "_%Y%m%d_%H%M%S_"))
	   nil
	   ".png"))

  (shell-command (concat
		  "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;"
		  "if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('"
		  temp-image-filename
		  "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
  (org-attach-attach
   temp-image-filename
   nil
   `mv)
  (insert (concat
	   "[[file:"
	   (org-attach-dir)
	   "/"
	   (file-name-nondirectory temp-image-filename)
	   "]]"))
    (org-link-preview-region))

(defun win/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun win/cmd-exec-bat-new-window (input-str)
  (let ((cmd-str (concat "start cmd /k " input-str)))
    (start-process "cmd" nil "cmd.exe" "/C" cmd-str)))

(defun org-attach-open-win-explorer ()
  (interactive)
  (w32-shell-execute 1 (org-attach-dir-get-create)))

(defun ads/dired-win-default ()
    (interactive)
    (let ((filename
	   (dired-replace-in-string "/" "\\" (dired-get-filename))))
      (w32-shell-execute 1 filename)))

(general-define-key
 :keymaps 'dired-mode-map
 "<tab>" 'ads/dired-win-default)

(add-to-list 'display-buffer-alist
  (cons "ads/git-lazy" (cons #'display-buffer-no-window nil)))
(defun ads/git-lazy ()
  (interactive)
  (save-buffer)
  (shell-command (concat "git stage " buffer-file-name) )
  (magit-diff-staged)
  (delete-other-windows)
  (shell-command (concat "git commit -m \"" (read-string "Commit Message:\t") "\""))
  (async-shell-command "git push" "ads/git-lazy")
  (magit-mode-bury-buffer))

(setq projectile-indexing-method 'hybrid)

(defun ads/projectile-shell-global ()
  (interactive)
  (projectile-switch-project-by-name  (concat git-directory "shell-global")))

(ads/leader-keys "cg" 'ads/projectile-shell-global)

(provide 'ms-windows.el)

;;; ms-windows.el ends here
