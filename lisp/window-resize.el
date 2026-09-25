;;; window-resize.el --- Window resizing commands  -*- lexical-binding: t; -*-
;;; Commentary:
;; window-resize
;;; Code:

(require 'transient)

(defcustom ads/window-resize-step 5
  "Number of columns/rows to resize by."
  :type 'integer
  :group 'convenience)

(defcustom ads/window-resize-big-multiplier 3
  "Multiplier applied to `ads/window-resize-step' for big (HJKL) moves."
  :type 'integer
  :group 'convenience)

(defvar ads/resize-multiplier 1
  "Multiplier applied to `ads/window-resize-step' for the current move.
Let-bound to `ads/window-resize-big-multiplier' during big moves.")

(defun ads/resize--step ()
  "Return the effective resize step for the current move."
  (* ads/resize-multiplier ads/window-resize-step))

(defvar ads/resize-selected-separator nil
  "Currently selected separator: nil, left, right, above, below.")

(defun ads/resize-reset ()
  "Deselect the current separator."
  (interactive)
  (setq ads/resize-selected-separator nil)
  (message "Separator deselected"))

(defun ads/resize--move-selected (delta)
  "Move the selected separator by DELTA. Positive = right/down, negative = left/up."
  (pcase ads/resize-selected-separator
    ('left  (adjust-window-trailing-edge (window-in-direction 'left) delta t))
    ('right (adjust-window-trailing-edge (selected-window) delta t))
    ('above (adjust-window-trailing-edge (window-in-direction 'above) delta nil))
    ('below (adjust-window-trailing-edge (selected-window) delta nil))))

(defun ads/resize-h ()
  "Move separator left, or select left separator if ambiguous."
  (interactive)
  (cond
   ;; Already have a selection -> move it left
   (ads/resize-selected-separator
    (ads/resize--move-selected (- (ads/resize--step))))
   ;; No selection yet
   (t (let ((left-win (window-in-direction 'left))
            (right-win (window-in-direction 'right)))
        (cond
         ;; Only separator to right -> move it left
         ((and right-win (not left-win))
          (adjust-window-trailing-edge (selected-window) (- (ads/resize--step)) t))
         ;; Only separator to left -> move it left
         ((and left-win (not right-win))
          (adjust-window-trailing-edge left-win (- (ads/resize--step)) t))
         ;; Both exist -> select left
         ((and left-win right-win)
          (setq ads/resize-selected-separator 'left)
          (message "Selected LEFT separator"))
         (t (message "No separator to move")))))))

(defun ads/resize-l ()
  "Move separator right, or select right separator if ambiguous."
  (interactive)
  (cond
   ;; Already have a selection -> move it right
   (ads/resize-selected-separator
    (ads/resize--move-selected (ads/resize--step)))
   ;; No selection yet
   (t (let ((left-win (window-in-direction 'left))
            (right-win (window-in-direction 'right)))
        (cond
         ;; Only separator to left -> move it right
         ((and left-win (not right-win))
          (adjust-window-trailing-edge left-win (ads/resize--step) t))
         ;; Only separator to right -> move it right
         ((and right-win (not left-win))
          (adjust-window-trailing-edge (selected-window) (ads/resize--step) t))
         ;; Both exist -> select right
         ((and left-win right-win)
          (setq ads/resize-selected-separator 'right)
          (message "Selected RIGHT separator"))
         (t (message "No separator to move")))))))

(defun ads/resize-k ()
  "Move separator up, or select above separator if ambiguous."
  (interactive)
  (cond
   ;; Already have a selection -> move it up
   (ads/resize-selected-separator
    (ads/resize--move-selected (- (ads/resize--step))))
   ;; No selection yet
   (t (let ((above-win (window-in-direction 'above))
            (below-win (window-in-direction 'below)))
        (cond
         ;; Only separator below -> move it up
         ((and below-win (not above-win))
          (adjust-window-trailing-edge (selected-window) (- (ads/resize--step)) nil))
         ;; Only separator above -> move it up
         ((and above-win (not below-win))
          (adjust-window-trailing-edge above-win (- (ads/resize--step)) nil))
         ;; Both exist -> select above
         ((and above-win below-win)
          (setq ads/resize-selected-separator 'above)
          (message "Selected ABOVE separator"))
         (t (message "No separator to move")))))))

(defun ads/resize-j ()
  "Move separator down, or select below separator if ambiguous."
  (interactive)
  (cond
   ;; Already have a selection -> move it down
   (ads/resize-selected-separator
    (ads/resize--move-selected (ads/resize--step)))
   ;; No selection yet
   (t (let ((above-win (window-in-direction 'above))
            (below-win (window-in-direction 'below)))
        (cond
         ;; Only separator above -> move it down
         ((and above-win (not below-win))
          (adjust-window-trailing-edge above-win (ads/resize--step) nil))
         ;; Only separator below -> move it down
         ((and below-win (not above-win))
          (adjust-window-trailing-edge (selected-window) (ads/resize--step) nil))
         ;; Both exist -> select below
         ((and above-win below-win)
          (setq ads/resize-selected-separator 'below)
          (message "Selected BELOW separator"))
         (t (message "No separator to move")))))))

(defmacro ads/resize--define-big (name fn)
  "Define big-move command NAME wrapping FN with the big multiplier."
  `(defun ,name ()
     ,(format "Like `%s' but move by the big step." fn)
     (interactive)
     (let ((ads/resize-multiplier ads/window-resize-big-multiplier))
       (,fn))))

(ads/resize--define-big ads/resize-H ads/resize-h)
(ads/resize--define-big ads/resize-L ads/resize-l)
(ads/resize--define-big ads/resize-K ads/resize-k)
(ads/resize--define-big ads/resize-J ads/resize-j)

(defun ads/resize--mode-description ()
  "Return mode description with color based on selection state."
  (if ads/resize-selected-separator
      (propertize (format "Moving: %s" (upcase (symbol-name ads/resize-selected-separator)))
                  'face '(:foreground "dodger blue" :weight bold))
    (propertize "Select separator" 'face '(:foreground "green" :weight bold))))

(transient-define-prefix ads/window-resize-transient ()
  "Resize windows by moving separators."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-quit-all
  [:description ads/resize--mode-description
   ["h/j/k/l"
    ("h" "← left"  ads/resize-h)
    ("l" "→ right" ads/resize-l)
    ("k" "↑ up"    ads/resize-k)
    ("j" "↓ down"  ads/resize-j)]
   ["H/J/K/L (3x)"
    ("H" "← left"  ads/resize-H)
    ("L" "→ right" ads/resize-L)
    ("K" "↑ up"    ads/resize-K)
    ("J" "↓ down"  ads/resize-J)]
   ["Control"
    ("SPC" "deselect" ads/resize-reset)
    ("=" "balance" balance-windows)
    ("m" "maximize" delete-other-windows)
    ("u" "undo" winner-undo)
    ("r" "redo" winner-redo)]]
  ;; Start with no separator selected each time the transient opens.
  (interactive)
  (setq ads/resize-selected-separator nil)
  (transient-setup 'ads/window-resize-transient))

(ads/leader-keys "j;" 'ads/window-resize-transient)

;;; window-resize.el ends here
