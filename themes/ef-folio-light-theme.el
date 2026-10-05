;;; ef-folio-light-theme.el --- Anthropic brand, light  -*- lexical-binding:t -*-

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
;; Folio: warm paper and ink.  Slate ink on an ivory canvas, with the accents
;; held well back, so code reads like a printed page rather than a highlighter.
;;
;; Heavily inspired by Anthropic's published design system, from which the
;; ivory canvas (#faf9f5), the slate ink (#141413) and the eight accent
;; swatches (clay, fig, cactus, sky, olive, heather, coral, kraft) are taken.
;; Unofficial and unaffiliated - not endorsed by or connected with Anthropic.
;; Values were read from their public CSS and adapted, not copied wholesale.
;;
;; The accent swatches as published are SURFACE colours - measured against the
;; cream canvas not one of them reaches 4.5:1 as text (clay 2.96, sky 2.78,
;; cactus 1.52, coral 1.40).  So each is darkened here with hue and saturation
;; held and lightness dropped until it clears 5.5:1, and the published swatch
;; is used where it belongs instead: the subtle backgrounds.

;;; Code:

(require 'ef-themes)
(require 'ef-folio-common)

(defconst ef-folio-light-palette-partial
  '((cursor "#d97757")                  ; clay
    (bg-main "#faf9f5")                 ; canvas / ivory
    (bg-dim "#f0eee6")                  ; surface-secondary
    (bg-alt "#e8e6dc")                  ; surface-secondary-hover
    (bg-active "#e3dacc")               ; surface-warm / oat
    (bg-inactive "#f0eee6")
    (fg-main "#141413")                 ; slate ink
    (fg-dim "#5e5d59")                  ; text-muted
    (fg-alt "#a94726")                  ; clay, darkened to read as text
    (border "#d1cfc5")                  ; hairline

    ;; Brand accents, darkened to clear 5.5:1 on the cream canvas.
    (red "#a14c30")                     ; deep
    (red-warmer "#a94545")              ; coral
    (red-cooler "#a94726")              ; clay
    (red-faint "#8f5f50")
    (green "#5b6a47")                   ; olive
    (green-warmer "#6b7a47")
    (green-cooler "#4a6c61")            ; cactus
    (green-faint "#5f7060")
    (yellow "#8f5730")                  ; kraft
    (yellow-warmer "#a94726")           ; clay
    (yellow-cooler "#8f6340")
    (yellow-faint "#72716b")            ; text-tertiary, darkened - carries comments
    (blue "#35689c")                    ; sky
    (blue-warmer "#63608e")             ; heather
    (blue-cooler "#2d5f8f")
    (blue-faint "#5a6b84")
    (magenta "#a84164")                 ; fig
    (magenta-warmer "#a8415a")
    (magenta-cooler "#7a4f8a")
    (magenta-faint "#8f6675")
    (cyan "#4a6c61")                    ; cactus
    (cyan-warmer "#3f6f7f")
    (cyan-cooler "#2f6f6f")
    (cyan-faint "#5f7570")

    ;; The published swatches, used as the surfaces they were drawn to be.
    (bg-red-subtle "#ebcece")           ; coral
    (bg-green-subtle "#bcd1ca")         ; cactus
    (bg-yellow-subtle "#ebdbbc")        ; manilla
    (bg-blue-subtle "#cbcadb")          ; heather
    (bg-magenta-subtle "#f2d5de")
    (bg-cyan-subtle "#d2e3dd")

    (bg-red-intense "#e8a0a0")
    (bg-green-intense "#a8c8b4")
    (bg-yellow-intense "#e0c070")
    (bg-blue-intense "#a8c0e0")
    (bg-magenta-intense "#e0a8c0")
    (bg-cyan-intense "#a0cdc0")

    (bg-added "#d8e8d8")
    (bg-added-faint "#e8f0e4")
    (bg-added-refine "#c0dcc4")
    (fg-added "#3f5f3f")

    (bg-changed "#f0e4c8")
    (bg-changed-faint "#f6eeda")
    (bg-changed-refine "#e8d4a8")
    (fg-changed "#6f5520")

    (bg-removed "#f2d8d4")
    (bg-removed-faint "#f8e8e4")
    (bg-removed-refine "#e8c0bc")
    (fg-removed "#8f3828")

    (bg-mode-line-active "#e3dacc")     ; oat band
    (fg-mode-line-active "#141413")
    (bg-completion "#e8e6dc")
    (bg-popup "#f0eee6")
    (bg-hover "#e3dacc")
    (bg-hover-secondary "#cbcadb")
    (bg-hl-line "#f0eee6")
    (bg-paren-match "#bcd1ca")
    (bg-err "#f2d8d4")
    (bg-warning "#f0e4c8")
    (bg-info "#d8e8d8")
    (bg-region "#e8e6dc")))


(defvar ef-folio-light-palette-overrides nil
  "Overrides for `ef-folio-light-palette'.")

(defconst ef-folio-light-palette
  (modus-themes-generate-palette
   ef-folio-light-palette-partial
   'warm
   nil
   (append ef-folio-mappings-partial ef-themes-palette-common)))

;;;###theme-autoload
(modus-themes-theme
 'ef-folio-light
 'ef-themes
 "Anthropic brand: slate ink on ivory canvas, accents held in reserve."
 'light
 'ef-folio-light-palette
 nil
 'ef-folio-light-palette-overrides)

(provide 'ef-folio-light-theme)

;;; ef-folio-light-theme.el ends here
