;;; gps-time.el --- GPS time conversion  -*- lexical-binding: t; -*-
;;; Commentary:
;; GPS time conversion
;;; Code:

(require 'cl-lib)

(defconst gps-time-epoch-unix 315964800
  "Unix time of the GPS epoch, 1980-01-06 00:00:00 UTC.")


(defconst gps-time-seconds-per-week 604800
  "Number of seconds in a GPS week (7 * 86400).")

(defconst gps-time-leap-table
  ;; (UNIX-THRESHOLD . GPS-UTC-OFFSET), newest first.  OFFSET is how many
  ;; seconds GPS is ahead of UTC at/after that instant.  Extend when the
  ;; IERS announces a new leap second.
  (let ((transitions
         ;; (YEAR MONTH DAY OFFSET) at 00:00:00 UTC, oldest first.
         '((1981  7 1  1) (1982  7 1  2) (1983  7 1  3) (1985  7 1  4)
           (1988  1 1  5) (1990  1 1  6) (1991  1 1  7) (1992  7 1  8)
           (1993  7 1  9) (1994  7 1 10) (1996  1 1 11) (1997  7 1 12)
           (1999  1 1 13) (2006  1 1 14) (2009  1 1 15) (2012  7 1 16)
           (2015  7 1 17) (2017  1 1 18))))
    (nreverse
     (mapcar (lambda (tr)
               (cl-destructuring-bind (y m d off) tr
                 (cons (float-time (encode-time 0 0 0 d m y t)) off)))
             transitions))))

(defun gps-time--leap-for-unix (unix)
  "Return the GPS-UTC offset (seconds) in effect at UNIX time."
  (or (cl-loop for (thr . off) in gps-time-leap-table
               when (>= unix thr) return off)
      0))

(defun gps-time--leap-for-gps (gps)
  "Return the GPS-UTC offset (seconds) in effect at GPS seconds GPS."
  (or (cl-loop for (thr . off) in gps-time-leap-table
               when (>= gps (+ (- thr gps-time-epoch-unix) off)) return off)
      0))

(defun gps-time--to-unix (utc)
  "Coerce UTC to a Unix time (float).
UTC may be a number (already Unix time), an Emacs time value, or a
string.  Strings are always interpreted as UTC, e.g.
\"2026-07-10 12:34:56\" or \"2026-07-10T12:34:56\"."
  (cond
   ((numberp utc) utc)
   ((stringp utc)
    (let ((p (parse-time-string utc)))
      (unless (and (nth 3 p) (nth 4 p) (nth 5 p))
        (error "Cannot parse UTC time string: %s" utc))
      (float-time (encode-time (or (nth 0 p) 0) (or (nth 1 p) 0)
                               (or (nth 2 p) 0)
                               (nth 3 p) (nth 4 p) (nth 5 p) t))))
   (t (float-time utc))))

(defun gps-time--gps-to-unix (seconds)
  "Return the Unix time for GPS SECONDS since the GPS epoch."
  (- (+ seconds gps-time-epoch-unix)
     (gps-time--leap-for-gps seconds)))

;;;###autoload
(defun gps-sec2UTC (seconds)
  "Convert GPS SECONDS (since the GPS epoch) to a UTC time string."
  (format-time-string "%Y-%m-%d %H:%M:%S"
                      (seconds-to-time (gps-time--gps-to-unix seconds)) t))

(defun gps-time-pretty (unix)
  "Format UNIX time as a UTC string with local hours:minutes in parens.
E.g. \"2026-07-10 12:00:00 UTC (05:00 PDT)\"."
  (let ((tv (seconds-to-time unix)))
    (concat (format-time-string "%Y-%m-%d %H:%M:%S UTC" tv t)
            (format-time-string " (%H:%M %Z)" tv))))

;;;###autoload
(defun gps-sec2UTC-pretty (seconds)
  "Convert GPS SECONDS to a pretty UTC + local time string.
See `gps-time-pretty'."
  (gps-time-pretty (gps-time--gps-to-unix seconds)))

;;;###autoload
(defun gps-wk-sec2UTC (wk seconds)
  "Convert GPS week WK and seconds-of-week SECONDS to a UTC time string."
  (gps-sec2UTC (+ (* wk gps-time-seconds-per-week) seconds)))

;;;###autoload
(defun utc2gps-sec (utc)
  "Convert UTC to GPS seconds since the GPS epoch.
See `gps-time--to-unix' for accepted forms of UTC."
  (let ((unix (gps-time--to-unix utc)))
    (+ (- unix gps-time-epoch-unix)
       (gps-time--leap-for-unix unix))))

;;;###autoload
(defun utc2gps-wk-sec (utc)
  "Convert UTC to a list (WEEK SECONDS-OF-WEEK) of GPS time."
  (let* ((g (utc2gps-sec utc))
         (wk (floor g gps-time-seconds-per-week)))
    (list wk (- g (* wk gps-time-seconds-per-week)))))

;;; Interactive wrappers -------------------------------------------------

;;;###autoload
(defun gps-time-sec-to-utc (seconds)
  "Prompt for GPS SECONDS and echo the corresponding UTC time."
  (interactive "nGPS seconds: ")
  (message "%s UTC" (gps-sec2UTC seconds)))

;;;###autoload
(defun gps-time-wk-sec-to-utc (wk seconds)
  "Prompt for GPS week WK and SECONDS-of-week and echo the UTC time."
  (interactive "nGPS week: \nnSeconds of week: ")
  (message "%s UTC" (gps-wk-sec2UTC wk seconds)))

;;;###autoload
(defun gps-time-utc-to-gps (utc)
  "Prompt for a UTC string and echo GPS seconds plus week/sec."
  (interactive "sUTC time (e.g. 2026-07-10 12:00:00): ")
  (cl-destructuring-bind (wk sec) (utc2gps-wk-sec utc)
    (message "GPS: %s sec  (week %d, %s sec)" (utc2gps-sec utc) wk sec)))

;;; Transient menu ------------------------------------------------------

(require 'transient)

(defun gps-time--arg (args flag)
  "Return the value set for FLAG (e.g. \"--sec=\") in transient ARGS, or nil."
  (cl-loop for a in args
           when (string-prefix-p flag a)
           return (substring a (length flag))))

(defun gps-time--report (value &optional label)
  "Echo VALUE (with optional LABEL) and copy it to the kill ring."
  (kill-new value)
  (message "%s%s  [copied]" value (if label (concat "  " label) ""))
  value)

(transient-define-suffix gps-time-menu--sec2utc (args)
  "Convert the entered GPS seconds to UTC."
  (interactive (list (transient-args 'gps-time-menu)))
  (let ((s (gps-time--arg args "--sec=")))
    (unless s (user-error "Set GPS seconds (s) first"))
    (gps-time--report (gps-sec2UTC-pretty (string-to-number s)))))

(transient-define-suffix gps-time-menu--wksec2utc (args)
  "Convert the entered GPS week and seconds-of-week to UTC."
  (interactive (list (transient-args 'gps-time-menu)))
  (let ((w (gps-time--arg args "--week="))
        (o (gps-time--arg args "--sow=")))
    (unless (and w o) (user-error "Set GPS week (w) and seconds of week (o) first"))
    (gps-time--report
     (gps-time-pretty (gps-time--gps-to-unix
                       (+ (* (string-to-number w) gps-time-seconds-per-week)
                          (string-to-number o)))))))

(transient-define-suffix gps-time-menu--utc2gps (args)
  "Convert the entered UTC time to GPS seconds and week/sec."
  (interactive (list (transient-args 'gps-time-menu)))
  (let ((u (gps-time--arg args "--utc=")))
    (unless u (user-error "Set UTC time (u) first"))
    (cl-destructuring-bind (wk sow) (utc2gps-wk-sec u)
      (gps-time--report (format "%s" (utc2gps-sec u))
                        (format "GPS sec  (week %d, %s sow)" wk sow)))))

;;;###autoload
(transient-define-prefix gps-time-menu ()
  "GPS <-> UTC time conversions."
  ["Inputs"
   ("s" "GPS seconds"     "--sec=")
   ("w" "GPS week"        "--week=")
   ("o" "Seconds of week" "--sow=")
   ("u" "UTC time"        "--utc=")]
  ["Convert"
   ("g" "GPS sec  -> UTC"    gps-time-menu--sec2utc)
   ("k" "GPS wk/sec -> UTC"  gps-time-menu--wksec2utc)
   ("t" "UTC -> GPS"         gps-time-menu--utc2gps)]
  ["" ("q" "Quit" transient-quit-one)])

(provide 'gps-time)
;;; gps-time.el ends here

;;; gps-time.el ends here
