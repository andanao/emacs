;;; ef-folio-dark-theme.el --- Anthropic brand, dark  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Adrian Danao-Schroeder

;; Author: Adrian Danao-Schroeder <adriandanao@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Derived from the `ef-themes' by Protesilaos Stavrou
;; (Copyright (C) 2022-2026 Free Software Foundation, Inc.), which are
;; GPL-3.0-or-later.  This file follows their palette/mapping structure and
;; is licensed the same way.  The rest of this configuration repository is
;; MIT; this directory is not.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Folio, inverted: ivory on a true-black page.
;;
;; Heavily inspired by Anthropic's published design system, from which the
;; ivory canvas (#faf9f5), the slate ink (#141413) and the eight accent
;; swatches (clay, fig, cactus, sky, olive, heather, coral, kraft) are taken.
;; Unofficial and unaffiliated - not endorsed by or connected with Anthropic.
;; Values were read from their public CSS and adapted, not copied wholesale.
;;
;; Unlike the light theme the accents need no adjustment - measured against
;; true black every one of them clears 4.5:1 as text (cactus 13.11, kraft
;; 9.27, sky 7.17, clay 6.73, fig 5.59).  The published palette is in effect
;; a dark-mode palette.
;;
;; Slate ink #141413 is not text here; it is the lifted surface, which is what
;; gives the theme its band-on-band feel.

;;; Code:

(require 'ef-themes)
(require 'ef-folio-common)

(defconst ef-folio-dark-palette-partial
  '((cursor "#d97757")                  ; clay
    (bg-main "#000000")                 ; inverse band
    (bg-dim "#141413")                  ; slate ink, as surface
    (bg-alt "#1f1f1e")
    (bg-active "#3d3d3a")               ; ink-soft
    (bg-inactive "#141413")
    (fg-main "#faf9f5")                 ; canvas
    (fg-dim "#b0aea5")                  ; cloud-medium
    (fg-alt "#d97757")                  ; clay
    (border "#3d3d3a")                  ; ink-soft

    ;; Brand accents exactly as published - all clear 4.5:1 on black.
    (red "#d97757")                     ; clay
    (red-warmer "#e08a6a")
    (red-cooler "#c6613f")              ; deep
    (red-faint "#b08878")
    (green "#8fa571")
    (green-warmer "#a0b57f")
    (green-cooler "#bcd1ca")            ; cactus
    (green-faint "#788c5d")             ; olive
    (yellow "#d4a27f")                  ; kraft
    (yellow-warmer "#d97757")           ; clay
    (yellow-cooler "#ebdbbc")           ; manilla
    (yellow-faint "#87867f")            ; cloud-dark - carries comments
    (blue "#6a9bcc")                    ; sky
    (blue-warmer "#8fb0d8")
    (blue-cooler "#5a90c8")
    (blue-faint "#8fa0b8")
    (magenta "#c46686")                 ; fig
    (magenta-warmer "#d47f9a")
    (magenta-cooler "#cbcadb")          ; heather
    (magenta-faint "#b08f9c")
    (cyan "#bcd1ca")                    ; cactus
    (cyan-warmer "#a8c4bc")
    (cyan-cooler "#9fc8c0")
    (cyan-faint "#a0b0ac")

    (bg-red-subtle "#3d1f16")
    (bg-green-subtle "#1f2a18")
    (bg-yellow-subtle "#3a2a18")
    (bg-blue-subtle "#1a2634")
    (bg-magenta-subtle "#331f28")
    (bg-cyan-subtle "#1e2c28")

    (bg-red-intense "#6f2f1f")
    (bg-green-intense "#3f5730")
    (bg-yellow-intense "#6f5020")
    (bg-blue-intense "#2f4f70")
    (bg-magenta-intense "#60304a")
    (bg-cyan-intense "#345a50")

    (bg-added "#16240f")
    (bg-added-faint "#0d1a0a")
    (bg-added-refine "#243a18")
    (fg-added "#a0b57f")

    (bg-changed "#2e2410")
    (bg-changed-faint "#1d1708")
    (bg-changed-refine "#45350f")
    (fg-changed "#d4a27f")

    (bg-removed "#30150f")
    (bg-removed-faint "#1e0d09")
    (bg-removed-refine "#4a1d14")
    (fg-removed "#e08a6a")

    (bg-mode-line-active "#141413")     ; slate band
    (fg-mode-line-active "#faf9f5")
    (bg-completion "#1f1f1e")
    (bg-popup "#141413")
    (bg-hover "#3d3d3a")
    (bg-hover-secondary "#2f3a44")
    (bg-hl-line "#141413")
    (bg-paren-match "#3f5750")
    (bg-err "#30150f")
    (bg-warning "#2e2410")
    (bg-info "#16240f")
    (bg-region "#3d3d3a")))

(defvar ef-folio-dark-palette-overrides nil
  "Overrides for `ef-folio-dark-palette'.")

(defconst ef-folio-dark-palette
  (modus-themes-generate-palette
   ef-folio-dark-palette-partial
   'warm
   nil
   (append ef-folio-mappings-partial ef-themes-palette-common)))

;;;###theme-autoload
(modus-themes-theme
 'ef-folio-dark
 'ef-themes
 "Anthropic brand: ivory canvas on the true-black feature band."
 'dark
 'ef-folio-dark-palette
 nil
 'ef-folio-dark-palette-overrides)

(provide 'ef-folio-dark-theme)

;;; ef-folio-dark-theme.el ends here
