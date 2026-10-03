#!/usr/bin/env python3
"""Split the tangled output into the new tree described by manifest.py.

Usage: port.py <reference-dir> <repo-root>

Purely mechanical.  Breadcrumbs come off, file headers go on, nothing else
changes.  Deliberate edits (dropping tangle-on-save, disabling package.el,
repointing the config path) are applied afterwards as visible commits, not
buried in here.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from runs import load_runs                      # noqa: E402
from manifest import MANIFEST, DROPPED          # noqa: E402

COOKIE = '-*- lexical-binding: t; -*-'

# Files tangled alongside init.el that stay whole at the repo root.  The
# loader in "Computer specific configs" finds them via user-emacs-directory,
# which becomes this repo, so the paths already work unchanged.
STANDALONE = {
    'early-init.el': 'Pre-init frame, GC and package settings',
    'mac.el': 'macOS-only configuration',
    'linux.el': 'Linux-only configuration',
    'ms-windows.el': 'Windows-only configuration',
}


def check(runs):
    """Resolve manifest names against the tangled runs; return name -> index.

    Every failure here is loud on purpose.  The manifest used to key on run
    number, which meant a section inserted anywhere in readme.org silently
    refiled everything below it into the wrong files, and the sorted-form
    check in verify.py could not see it.
    """
    idx = {}
    for i, r in enumerate(runs, 1):
        if r['name'] in idx:
            raise SystemExit(f'duplicate run name {r["name"]!r} '
                             f'at {idx[r["name"]]} and {i}')
        idx[r['name']] = i

    seen = {}
    for path, (_, names) in MANIFEST.items():
        for n in names:
            if n not in idx:
                raise SystemExit(f'{path}: no run named {n!r}')
            if n in seen:
                raise SystemExit(f'run {n!r} in both {seen[n]} and {path}')
            if n in DROPPED:
                raise SystemExit(f'run {n!r} is both dropped and in {path}')
            seen[n] = path
    gone = [n for n in DROPPED if n not in idx]
    if gone:
        raise SystemExit(f'dropped runs no longer in the source: {gone}')

    missing = [r['name'] for r in runs
               if r['name'] not in seen and r['name'] not in DROPPED]
    if missing:
        named = [f'{idx[n]} ({n})' for n in missing]
        raise SystemExit('unassigned runs:\n  ' + '\n  '.join(named))
    return idx


def trim(body):
    body = list(body)
    while body and not body[0].strip():
        body.pop(0)
    while body and not body[-1].strip():
        body.pop()
    return body


FOOTER = 'ends here'


def render(base, title, bodies, sections, dedup_footer=False):
    out = [f';;; {base} --- {title}  {COOKIE}',
           ';;; Commentary:']
    out += [f';; {s}' for s in sections]
    out += [';;; Code:', '']
    bodies = [trim(b) for b in bodies]
    # The org source of the standalone files already closes with its own
    # ";;; x.el ends here"; don't emit a second one under it.
    if dedup_footer and bodies and bodies[-1] \
            and bodies[-1][-1].strip() == f';;; {base} {FOOTER}':
        bodies[-1].pop()
        bodies[-1] = trim(bodies[-1])
    for body in bodies:
        out.extend(body)
        out.append('')
    out.append(f';;; {base} {FOOTER}')
    return '\n'.join(out) + '\n'


def write(path, text):
    os.makedirs(os.path.dirname(path) or '.', exist_ok=True)
    with open(path, 'w', encoding='utf-8') as fh:
        fh.write(text)


def wrap_sections(names, width=72):
    """Fold a section list into comment-friendly lines."""
    lines, cur = [], ''
    for n in names:
        piece = n if not cur else f'{cur}, {n}'
        if len(piece) > width and cur:
            lines.append(cur + ',')
            cur = n
        else:
            cur = piece
    if cur:
        lines.append(cur)
    return lines


LOADER_HEAD = '''\
;;; init.el --- Emacs configuration entry point  {cookie}
;;; Commentary:
;; Loads every file under config/ and lisp/ in a fixed order.  That order
;; matches the sequence the old literate config tangled into, so behaviour
;; does not depend on how the sections were regrouped into files.
;;; Code:

;; early-init.el sets this; recomputed here so this file also works when
;; loaded on its own, which is what the "load init" binding does.
(defvar ads/config-directory
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory this configuration really lives in.
See the definition in early-init.el.")

(defun ads/load-config (relative)
  "Load RELATIVE, an elisp file below `ads/config-directory'.
Loaded by explicit path rather than with `require', so that a file named
after a package (org.el, dired.el) cannot shadow the real one."
  (load (expand-file-name relative ads/config-directory) nil :nomessage))
'''


def loader(order):
    out = LOADER_HEAD.format(cookie=COOKIE).split('\n')
    for path, (title, _) in order:
        stem = path[:-3] if path.endswith('.el') else path
        out.append(f'(ads/load-config "{stem}")')
    out += ['', ';;; init.el ends here']
    return '\n'.join(out) + '\n'


def main(refdir, root):
    _, preamble, runs = load_runs(os.path.join(refdir, 'init.el'))
    idx = check(runs)
    print(f'init.el: {len(runs)} runs, preamble {preamble}')

    order = sorted(MANIFEST.items(),
                   key=lambda kv: min(idx[n] for n in kv[1][1]))
    total = 0
    for path, (title, names) in order:
        names = sorted(names, key=lambda n: idx[n])
        text = render(os.path.basename(path), title,
                      [runs[idx[n] - 1]['body'] for n in names],
                      wrap_sections(names))
        write(os.path.join(root, path), text)
        n = len(text.splitlines())
        total += n
        print(f'{n:5d}  {path}')

    write(os.path.join(root, 'init.el'), loader(order))
    print(f'      init.el (loader, {len(order)} entries)')

    for name, title in STANDALONE.items():
        src = os.path.join(refdir, name)
        if not os.path.exists(src):
            print(f'  !! missing {src}')
            continue
        _, pre, rs = load_runs(src)
        bodies = [r['body'] for r in rs]
        # Drop a trailing (provide 'x) / ends-here that the org source
        # carried; render adds its own footer.
        text = render(name, title, bodies,
                      wrap_sections([r['name'] for r in rs]),
                      dedup_footer=True)
        write(os.path.join(root, name), text)
        print(f'{len(text.splitlines()):5d}  {name}')

    print(f'--- {len(order)} split files, {total} lines ---')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
