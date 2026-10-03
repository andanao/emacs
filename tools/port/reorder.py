#!/usr/bin/env python3
"""Report how far each section moved when runs were regrouped into files.

The sorted-form gate is blind to order by design, so this is the other half:
every section that changed position, largest move first, plus the handful of
ordering facts that actually matter for load-time behaviour.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from runs import load_runs                      # noqa: E402
from manifest import MANIFEST, DROPPED          # noqa: E402


# The tangled file opens with core setup and only then starts configuring
# packages.  Named rather than numbered: this used to be "run >= 19", which
# meant a section inserted above it quietly moved the boundary.
FIRST_PACKAGE = 'agent-shell'
PLATFORM = 'Computer specific configs'
THEME = ('modus-themes', 'modus-tweaks', 'force reload')


def main(refdir):
    _, _, runs = load_runs(os.path.join(refdir, 'init.el'))
    tangled = {r['name']: i for i, r in enumerate(runs, 1)}

    order = sorted(MANIFEST.items(),
                   key=lambda kv: min(tangled[n] for n in kv[1][1]))
    new_seq = []
    for path, (_, names) in order:
        for n in sorted(names, key=lambda n: tangled[n]):
            new_seq.append((n, path))

    old_pos = {}
    for r in runs:
        if r['name'] in DROPPED:
            continue
        old_pos[r['name']] = len(old_pos) + 1
    new_pos = {n: i + 1 for i, (n, _) in enumerate(new_seq)}
    in_file = dict(new_seq)

    moves = []
    for n in old_pos:
        d = new_pos[n] - old_pos[n]
        if d:
            moves.append((abs(d), d, n, in_file[n]))
    moves.sort(key=lambda m: (-m[0], m[2]))

    print(f'sections: {len(old_pos)}   moved: {len(moves)}   '
          f'unmoved: {len(old_pos) - len(moves)}')
    print()
    print(f'{"move":>6}  {"was":>4} {"now":>4}  section -> file')
    for _, d, n, path in moves[:30]:
        print(f'{d:+6d}  {old_pos[n]:4d} {new_pos[n]:4d}  {n} -> {path}')
    if len(moves) > 30:
        print(f'   ... {len(moves) - 30} more')

    print()
    print('--- ordering facts that matter ---')
    pkg_from = tangled[FIRST_PACKAGE]
    packages = [n for n in old_pos if tangled[n] >= pkg_from]

    gen = new_pos['General.el']
    first_pkg = min(new_pos[n] for n in packages)
    print(f'General.el loads at {gen}; earliest package section at '
          f'{first_pkg}')
    theme = max(new_pos[n] for n in old_pos if n in THEME)
    print(f'last theme section loads at {theme}')

    for label, limit in (('the theme', theme), ('General.el', gen)):
        early = sorted((new_pos[n], n) for n in packages
                       if new_pos[n] < limit)
        print(f'package sections loading before {label}: {len(early)}')
        for p, s in early:
            print(f'    {p:4d}  {s}')

    print(f'"{PLATFORM}" loads at {new_pos[PLATFORM]} of {len(old_pos)}')


if __name__ == '__main__':
    main(sys.argv[1])
