#!/usr/bin/env python3
"""Which tangled run goes into which new file.

Keys are output paths relative to the repo root.  Values are (title, [run
numbers]) using the 1-based numbering from runs.py against the tangled
init.el.  Runs keep tangle order inside a file, and files are loaded in order
of their lowest run number, so the global sequence stays as close to the
tangled original as a regrouping allows.

DROPPED lists runs deliberately left out, each with a reason.
"""

import os

# Set PORT_MECHANICAL=1 to emit a pure move: nothing dropped, so the tree is
# byte-equivalent to the tangled output and the split commit reviews as a
# move rather than a move plus edits.
MECHANICAL = os.environ.get('PORT_MECHANICAL') == '1'

DROPPED = {} if MECHANICAL else {
    3: 'auto tangle files - tangle-on-save, removed by requirement 5',
}

_SETTINGS = [1, 2, 4] if not MECHANICAL else [1, 2, 3, 4]

MANIFEST = {
    # --- core, in original order ---------------------------------------
    'config/settings.el': ('General editor settings', _SETTINGS),
    'config/theme.el': ('Fonts, Modus themes and per-project colours',
                        [5, 6, 7, 8, 9]),
    'config/keybindings.el': ('general.el and the leader-key map',
                              [10, 11, 12, 13, 14, 15, 16, 17, 18]),
    'config/agent-shell.el': ('agent-shell sessions, notifications and resume',
                              [19, 20, 21, 22, 23, 24, 25, 26]),

    # --- packages, grouped by domain -----------------------------------
    'config/ui.el': ('Icons, ligatures, padding, scrolling and other chrome',
                     [27, 28, 47, 55, 67, 68, 75, 86,
                      152, 153, 159, 171, 173, 174, 177]),
    'config/prog.el': ('Language servers, formatting, treesit and modes',
                       [30, 41, 60, 71, 72, 73, 76, 77, 78, 79, 87,
                        157, 162, 169, 170, 176]),
    'config/files.el': ('Files, projects, history and shell helpers',
                        [31, 34, 35, 53, 88, 89, 149, 150, 155, 156,
                         158, 160, 168]),
    'config/text.el': ('Prose, markup and search syntax',
                       [32, 33, 37, 39, 70, 83, 84, 85, 145,
                        146, 147, 148]),
    'config/completion.el': ('Vertico, corfu and the completion stack',
                             [36, 38, 40, 82, 90, 172]),
    'config/d2.el': ('d2 diagrams and their previews', [42, 43, 44, 45, 46]),
    'config/dired.el': ('Dired and Dirvish', [48, 49]),
    'config/modeline.el': ('doom-modeline and its indicators',
                           [50, 51, 52, 161]),
    'config/vc.el': ('Magit, ediff and git links', [54, 65, 80, 81]),
    'config/evil.el': ('Evil and its companions', [56, 57, 58, 59]),
    'config/ghostel.el': ('ghostel terminal sessions', [61, 62, 63, 64]),
    'config/knockknock.el': ('knockknock notifications', [74]),
    'config/toggl.el': ('Time zones and toggl time tracking',
                        [163, 164, 165, 166, 167]),
    'config/platform.el': ('Per-machine and per-system loading', [178]),

    # --- org ------------------------------------------------------------
    'config/org/org.el': ('Core org setup, tags, todo keywords, keybindings',
                          [91, 92, 93, 94, 95, 96]),
    'config/org/agenda.el': ('org-agenda and its task frame',
                             [100, 101, 102]),
    'config/org/babel.el': ('org-babel languages and templates', [105]),
    'config/org/capture.el': ('org-capture templates', [106]),
    'config/org/appearance.el': ('org-modern, org-appear, org-tidy',
                                 [103, 118, 119, 141]),
    'config/org/extras.el': ('Smaller org add-ons',
                             [29, 104, 107, 108, 109, 110, 120]),
    'config/org/roam.el': ('org-roam and the roam helpers',
                           [122, 123, 124, 125, 126, 127, 128, 129, 130,
                            131, 132, 133, 134, 135, 136, 138, 139, 140]),
    'config/org/timegrid.el': ('org-timegrid calendar', [142]),
    'config/org/transclusion.el': ('org-transclusion', [143, 144]),

    # --- own code, the :custom: sections ---------------------------------
    'lisp/gps-time.el': ('GPS time conversion', [66]),
    'lisp/insert-variable-value.el': ('Insert a variable value at point', [69]),
    'lisp/org-reviews.el': ('Morning, weekly and monthly review templates',
                            [97, 98, 99]),
    'lisp/org-latex-preview.el': ('LaTeX preview rendering in org',
                                  [111, 112, 113, 114, 115]),
    'lisp/org-meetings.el': ('Meetings and their agenda notifications',
                             [116, 117]),
    'lisp/org-prettify-symbols.el': ('Prettified org keywords', [121]),
    'lisp/org-inbox-review.el': ('Inbox review workflow', [137]),
    'lisp/quartz.el': ('Quartz site publishing', [151]),
    'lisp/read-only-directories.el': ('Mark directories read-only', [154]),
    'lisp/window-resize.el': ('Window resizing commands', [175]),
}
