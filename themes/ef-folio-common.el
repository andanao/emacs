;;; ef-folio-common.el --- shared mappings for the Anthropic ef themes  -*- lexical-binding:t -*-

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
;; Shared semantic mappings for the Folio themes.  Both share one mapping
;; table; only the colour values underneath it differ.
;;
;; Heavily inspired by Anthropic's published design system, from which the
;; ivory canvas (#faf9f5), the slate ink (#141413) and the eight accent
;; swatches (clay, fig, cactus, sky, olive, heather, coral, kraft) are taken.
;; Unofficial and unaffiliated - not endorsed by or connected with Anthropic.
;; Values were read from their public CSS and adapted, not copied wholesale.

(require 'ef-themes)

;; Code reads editorially: grey comments, chroma held well back, clay as the
;; single warm pull.  The brand's position is restraint, so syntax does not
;; get a rainbow.
(defconst ef-folio-mappings-partial
  '((err red-warmer)
    (warning yellow)
    (info green)

    (fg-link red-cooler)                ; clay
    (fg-link-visited magenta)           ; fig
    (name blue)
    (keybind red-cooler)
    (identifier fg-dim)
    (fg-prompt red-cooler)

    (builtin blue-faint)
    (comment yellow-faint)              ; grey, not coloured
    (constant red-cooler)               ; clay
    (fnname yellow)                     ; kraft
    (fnname-call yellow-cooler)
    (keyword magenta-cooler)            ; heather
    (preprocessor red)
    (docstring cyan-faint)
    (string green-cooler)               ; cactus
    (type blue)                         ; sky
    (variable cyan)
    (variable-use cyan-faint)
    (rx-backslash magenta-cooler)
    (rx-construct cyan-cooler)

    (accent-0 red-cooler)               ; clay
    (accent-1 blue)                     ; sky
    (accent-2 green-cooler)             ; cactus
    (accent-3 magenta)                  ; fig

    (date-common yellow-cooler)
    (date-deadline red-warmer)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday magenta)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday blue)
    (date-weekend red-faint)

    (fg-prose-code red-cooler)
    (prose-done green)
    (fg-prose-macro magenta-cooler)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula info)
    (prose-tag yellow-faint)
    (prose-todo red-warmer)
    (fg-prose-verbatim green-cooler)

    (mail-cite-0 red-cooler)
    (mail-cite-1 blue)
    (mail-cite-2 green-cooler)
    (mail-cite-3 magenta)
    (mail-part yellow-cooler)
    (mail-recipient blue)
    (mail-subject red-cooler)
    (mail-other cyan)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (bg-space-err bg-yellow-intense)

    ;; Clay first - it is the brand's strongest chromatic accent.
    (rainbow-0 red-cooler)              ; clay
    (rainbow-1 blue)                    ; sky
    (rainbow-2 green-cooler)            ; cactus
    (rainbow-3 magenta)                 ; fig
    (rainbow-4 yellow)                  ; kraft
    (rainbow-5 magenta-cooler)          ; heather
    (rainbow-6 green)                   ; olive
    (rainbow-7 cyan-cooler)
    (rainbow-8 red)))

(provide 'ef-folio-common)

;;; ef-folio-common.el ends here
