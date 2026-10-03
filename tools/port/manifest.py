#!/usr/bin/env python3
"""Which tangled run goes into which new file.

Keys are output paths relative to the repo root.  Values are (title, [run
names]) using the section headings runs.py reads out of the tangled init.el.

Names rather than indices on purpose.  The numbering shifts the moment a
section is inserted anywhere above, which silently refiles every run below
it; a name that no longer exists is a hard error instead.  Runs keep tangle
order inside a file, and files are loaded in order of their earliest run, so
the global sequence stays as close to the tangled original as a regrouping
allows.

DROPPED lists runs deliberately left out, each with a reason.
"""

import os

# Set PORT_MECHANICAL=1 to emit a pure move: nothing dropped, so the tree is
# byte-equivalent to the tangled output and the split commit reviews as a
# move rather than a move plus edits.
MECHANICAL = os.environ.get('PORT_MECHANICAL') == '1'

DROPPED = {} if MECHANICAL else {
    'auto tangle files': 'auto tangle files - tangle-on-save, removed by requirement 5',
}

_SETTINGS = (
    ['Emacs Settings', 'replace-match case conversion fix', 'auto tangle files', 'auto chmod']
    if MECHANICAL else
    ['Emacs Settings', 'replace-match case conversion fix', 'auto chmod'])

MANIFEST = {
    'config/settings.el':
        ('General editor settings', _SETTINGS),
    'config/theme.el':
        ('Fonts, Modus themes and per-project colours',
         ['fonts', 'cjk width', 'modus-themes', 'modus-tweaks',
          'force reload', 'project colors']),
    'config/keybindings.el':
        ('general.el and the leader-key map',
         ['General.el', 'eval ~e~', 'quit ~q~', 'narrow ~n~',
          'windows, buffers, frames ~j~', 'kill and restore ~k~',
          'config ~c~', 'Toggles ~t~', 'Toggle frame decoration']),
    'config/agent-shell.el':
        ('agent-shell sessions, notifications and resume',
         ['agent-shell', 'waiting sessions', 'notify when a session finishes',
          'agent-shell consult source ~a~', 'switch-buffer preview',
          "last night's shells", 'global session resume',
          'kickoff with the first prompt']),
    'config/review.el':
        ('Drafting review comments and sending them as one prompt',
         ['review comments', 'entries', 'drawing them', 'writing one',
          'the commands', 'sending the review', 'the transient']),
    'config/ui.el':
        ('Icons, ligatures, padding, scrolling and other chrome',
         ['all-the-icons', 'all-the-icons-ibuffer', 'default-text-scale',
          'emojify', 'helpful', 'ident-bars', 'ligature', 'nerd-icons',
          'rainbow-delimiters', 'rainbow-mode', 'spacious-padding',
          'ultra-scroll', 'visual-fill-column', 'which-key', 'zoom']),
    'config/org/extras.el':
        ('Smaller org add-ons',
         ['anki-editor', 'org-autolist', 'org-cliplink', 'org-download',
          'org-fragtog', 'org-habit', 'org-noter']),
    'config/prog.el':
        ('Language servers, formatting, treesit and modes',
         ['apheleia', 'csv mode', 'flycheck', 'json', 'kanata', 'kdl',
          'lsp-mode', 'lsp-ui', 'lsp-rust', 'lsp-pyright', 'nix', 'rust',
          'terraform mode', 'treesit', 'treesit-auto', 'yaml']),
    'config/files.el':
        ('Files, projects, history and shell helpers',
         ['async', 'auto-revert', 'bookmark+', 'dwim-shell-commands',
          'log files', 'no-littering', 'nov (epub)', 'pdf-tools',
          'projectile', 'recentf', 'rg (ripgrep)', 'save-hist', 'sudo-edit',
          'tramp']),
    'config/text.el':
        ('Prose, markup and search syntax',
         ['auctex LaTeX', 'auto-fill', 'cdlatex', 'copy (yank) as markdown',
          'jinx', 'markdown', 'markdown keybindings', 'multiple-cursors',
          'ox-gfm', 'pcre2el', 'Evil search arity fix',
          'PCRE input for consult']),
    'config/completion.el':
        ('Vertico, corfu and the completion stack',
         ['cape', 'consult', 'corfu', 'marginalia', 'orderless', 'vertico']),
    'config/d2.el':
        ('d2 diagrams and their previews',
         ['d2-mode', 'making d2 blocks behave like latex previews',
          "editing blocks with =C-c '=",
          'refreshing the image after =C-c C-c=', 'block defaults',
          'default image width', 'keep a hand-set width across a re-run',
          'regenerate d2 diagrams on theme change']),
    'config/dired.el':
        ('Dired and Dirvish',
         ['Dired', 'Dirvish']),
    'config/modeline.el':
        ('doom-modeline and its indicators',
         ['display-time-mode', 'display-battery', 'doom-modeline',
          'telephone-line']),
    'config/vc.el':
        ('Magit, ediff and git links',
         ['ediff', 'git-link', 'magit', 'magit-pre-commit']),
    # undo-tree lives here, not in settings.el, because its :config sets
    # `evil-undo-system' through the defcustom's setter and borrows the
    # visualizer remaps `evil-integration' installs.  Both need evil loaded
    # first, so it has to sort after the evil run, not into an earlier file.
    'config/evil.el':
        ('Evil and its companions',
         ['evil', 'evil-anzu', 'evil-collection', 'evil-surround',
          'undo-tree']),
    'config/ghostel.el':
        ('ghostel terminal sessions',
         ['ghostel', 'popup terminal', 'session names',
          'ghostel consult source ~t~']),
    'lisp/gps-time.el':
        ('GPS time conversion',
         ['GPS time conversion']),
    'lisp/insert-variable-value.el':
        ('Insert a variable value at point',
         ['insert-variable-value']),
    'config/knockknock.el':
        ('knockknock notifications',
         ['knockknock']),
    'config/org/org.el':
        ('Core org setup, tags, todo keywords, keybindings',
         ['org', 'org-tags', 'org-todo-keywords', 'org keybindings',
          'resize the image at point', 'org url links']),
    'lisp/org-reviews.el':
        ('Morning, weekly and monthly review templates',
         ['good morning review', 'org weekly review', 'org monthly review']),
    'config/org/agenda.el':
        ('org-agenda and its task frame',
         ['org-agenda', 'Task Frame', 'rebuild on theme change']),
    'config/org/appearance.el':
        ('org-modern, org-appear, org-tidy',
         ['org-appear', 'org-modern', 'org-modern-indent', 'org-tidy']),
    'config/org/babel.el':
        ('org-babel languages and templates',
         ['org-babel']),
    'config/org/capture.el':
        ('org-capture templates',
         ['org-capture']),
    'lisp/org-latex-preview.el':
        ('LaTeX preview rendering in org',
         ['Drawing preamble', 'inline previews',
          'babel blocks to image files',
          'regenerate previews on theme change', 'keybindings']),
    'lisp/org-meetings.el':
        ('Meetings and their agenda notifications',
         ['org meetings', 'agenda notifications']),
    'lisp/org-prettify-symbols.el':
        ('Prettified org keywords',
         ['org-prettify-symbols']),
    'config/org/roam.el':
        ('org-roam and the roam helpers',
         ['org-roam', 'roam-agenda', 'roam-active-projects',
          'roam-categories', 'roam-capture-dailies', 'roam-daily-today',
          'roam-daily-archive', 'roam-insert-immediate', 'roam-node-display',
          'roam-modeline', 'roam-project-complete',
          'roam-refile-tag-file-list', 'roam-refile-category',
          'roam-refile-note', 'roam-stub-tag', 'org-roam-consult',
          'org-roam-ui', 'org-roam-ql']),
    'lisp/org-inbox-review.el':
        ('Inbox review workflow',
         ['org-inbox-review']),
    'config/org/timegrid.el':
        ('org-timegrid calendar',
         ['org-timegrid']),
    'config/org/transclusion.el':
        ('org-transclusion',
         ['org-transclusion', 'transclusion quote only']),
    'lisp/quartz.el':
        ('Quartz site publishing',
         ['quartz']),
    'lisp/read-only-directories.el':
        ('Mark directories read-only',
         ['read-only-directories']),
    'config/toggl.el':
        ('Time zones and toggl time tracking',
         ['time-zones', 'saved timers', 'the mode line', 'the package',
          'toggl sketchybar']),
    'lisp/window-resize.el':
        ('Window resizing commands',
         ['window-resize']),
    'config/workspaces.el':
        ('Workspaces, their switcher and per-project setup',
         ['workspaces', 'switching, with preview ~hh~',
          "this workspace's buffers ~w~", 'a workspace per project',
          'keybindings ~h~']),
    'config/platform.el':
        ('Per-machine and per-system loading',
         ['Computer specific configs']),
}
