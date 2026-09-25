;;; org-latex-preview.el --- LaTeX preview rendering in org  -*- lexical-binding: t; -*-
;;; Commentary:
;; Drawing preamble, inline previews, babel blocks to image files,
;; regenerate previews on theme change, keybindings
;;; Code:

(defconst ads/latex-drawing-preamble
  "\\usepackage{tikz}
\\usepackage{pgfplots}
\\usepackage{circuitikz}
\\usepackage{forest}
\\usepackage{tikz-cd}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usetikzlibrary{automata,positioning,arrows.meta,shapes.geometric,calc,fit,
  trees,decorations.pathmorphing,backgrounds,matrix,chains}
\\pgfplotsset{compat=1.18}"
  "LaTeX packages needed to draw plots, circuits, trees, graphs and state machines.
Shared by inline previews and by `latex' babel blocks so both render the same.")

(with-eval-after-load 'org
  (setq org-preview-latex-default-process 'dvisvgm)

  (plist-put org-format-latex-options :scale 1.4)
  (plist-put org-format-latex-options :background "Transparent")

  (setq org-format-latex-header
        (concat org-format-latex-header "\n" ads/latex-drawing-preamble)))

(with-eval-after-load 'ob-latex
  (setq org-babel-latex-preamble
        (lambda (_params)
          (concat "\\documentclass[preview,border=4pt]{standalone}\n"
                  ads/latex-drawing-preamble "\n")))

  (setq org-babel-latex-pdf-svg-process "pdftocairo -svg %f %O"))

(defvar ads/org-latex-preview-timer nil
  "Idle timer scheduled by `ads/org-refresh-latex-previews'.")

(defun ads/org--latex-rerender (beg end)
  "Drop and rebuild LaTeX previews between BEG and END."
  (org-clear-latex-preview beg end)
  (org--latex-preview-region beg end))

(defun ads/org-refresh-latex-previews ()
  "Regenerate LaTeX previews in every visible org buffer.
Previews embed the theme's foreground colour, so they must be rebuilt
after a theme change or they become unreadable.  Runs off an idle timer,
visible regions first, so the theme switch itself stays instant."
  (interactive)
  (when (timerp ads/org-latex-preview-timer)
    (cancel-timer ads/org-latex-preview-timer))
  (setq ads/org-latex-preview-timer
        (run-with-idle-timer
         0.3 nil
         (lambda ()
           (let (buffers)
             (dolist (window (window-list))
               (with-current-buffer (window-buffer window)
                 (when (derived-mode-p 'org-mode)
                   (unless (memq (current-buffer) buffers)
                     (push (current-buffer) buffers))
                   (ads/org--latex-rerender (window-start window)
                                            (window-end window t)))))
             (dolist (buffer buffers)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (ads/org--latex-rerender (point-min) (point-max))))))))))

(add-hook 'modus-themes-after-load-theme-hook #'ads/org-refresh-latex-previews)

(defun ads/org-latex-preview-buffer ()
  "Preview every LaTeX fragment in the buffer."
  (interactive)
  (org-latex-preview '(16)))

(defun ads/org-latex-preview-clear ()
  "Clear every LaTeX preview in the buffer."
  (interactive)
  (org-latex-preview '(64)))

(ads/leader-keys
  :keymaps 'org-mode-map
  "ov"  '(:ignore t :wk "Preview latex")
  "ovv" '(org-latex-preview :wk "toggle at point/section")
  "ovb" '(ads/org-latex-preview-buffer :wk "buffer")
  "ovc" '(ads/org-latex-preview-clear :wk "clear buffer")
  "ovr" '(ads/org-refresh-latex-previews :wk "regenerate visible")
  "ovd" '(ads/org-refresh-d2-images :wk "regenerate d2 images"))

;;; org-latex-preview.el ends here
