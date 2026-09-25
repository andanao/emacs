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


def main(refdir):
    _, _, runs = load_runs(os.path.join(refdir, 'init.el'))

    order = sorted(MANIFEST.items(), key=lambda kv: min(kv[1][1]))
    new_seq = []
    for path, (_, nums) in order:
        for n in nums:
            new_seq.append((n, path))

    old_pos = {}
    p = 0
    for i in range(1, len(runs) + 1):
        if i in DROPPED:
            continue
        p += 1
        old_pos[i] = p
    new_pos = {n: i + 1 for i, (n, _) in enumerate(new_seq)}

    moves = []
    for n in old_pos:
        d = new_pos[n] - old_pos[n]
        if d:
            moves.append((abs(d), d, n, runs[n - 1]['name'],
                          dict(new_seq)[n]))
    moves.sort(reverse=True)

    print(f'sections: {len(old_pos)}   moved: {len(moves)}   '
          f'unmoved: {len(old_pos) - len(moves)}')
    print()
    print(f'{"move":>6}  {"was":>4} {"now":>4}  section -> file')
    for _, d, n, name, path in moves[:30]:
        print(f'{d:+6d}  {old_pos[n]:4d} {new_pos[n]:4d}  {name} -> {path}')
    if len(moves) > 30:
        print(f'   ... {len(moves) - 30} more')

    print()
    print('--- ordering facts that matter ---')
    def where(pred):
        return [(new_pos[n], runs[n - 1]['name'])
                for n in old_pos if pred(runs[n - 1]['name'])]

    gen = min(p for p, _ in where(lambda s: s == 'General.el'))
    first_pkg = min(new_pos[n] for n in old_pos if n >= 19)
    print(f'General.el loads at {gen}; earliest package section at '
          f'{first_pkg}')
    theme = max(p for p, _ in where(
        lambda s: s in ('modus-themes', 'modus-tweaks', 'force reload')))
    print(f'last theme section loads at {theme}')
    early = [(new_pos[k], runs[k - 1]['name'])
             for k in old_pos if k >= 19 and new_pos[k] < theme]
    print(f'package sections loading before the theme: {len(early)}')
    for p, s in sorted(early):
        print(f'    {p:4d}  {s}')
    before_general = [(new_pos[k], runs[k - 1]['name'])
                      for k in old_pos if k >= 19 and new_pos[k] < gen]
    print(f'package sections loading before General.el: '
          f'{len(before_general)}')
    for p, s in sorted(before_general):
        print(f'    {p:4d}  {s}')
    plat = new_pos[178]
    print(f'"Computer specific configs" loads at {plat} of {len(old_pos)}')


if __name__ == '__main__':
    main(sys.argv[1])
