#!/usr/bin/env python3
"""Compact review of the remaining faithfulness differences.

Prints one line per differing top-level form instead of the whole form, so
every entry can actually be signed off.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from runs import load_runs                              # noqa: E402
from manifest import MANIFEST, DROPPED                  # noqa: E402
from verify import strip_header, normalise, top_forms   # noqa: E402


def label(form, width=76):
    first = form.split('\n', 1)[0].strip()
    return first[:width]


def main(refdir, root):
    _, _, runs = load_runs(os.path.join(refdir, 'init.el'))

    idx = {r['name']: i for i, r in enumerate(runs, 1)}

    ref = []
    for r in runs:
        if r['name'] in DROPPED:
            continue
        ref += r['body']
    ref = normalise(ref)

    order = sorted(MANIFEST.items(),
                   key=lambda kv: min(idx[n] for n in kv[1][1]))
    new = []
    for path, _ in order:
        new += strip_header(
            open(os.path.join(root, path), encoding='utf-8')
            .read().split('\n'), path)
    new = normalise(new)

    a, b = top_forms(ref), top_forms(new)
    only_ref = [f for f in a if f not in b]
    only_new = [f for f in b if f not in a]

    print(f'forms: tangled {len(a)}, new {len(b)}')
    print()
    print(f'--- gone from the new tree ({len(only_ref)}) ---')
    for f in only_ref:
        print(f'  - {label(f)}')
    print()
    print(f'--- new, not in the tangled output ({len(only_new)}) ---')
    for f in only_new:
        print(f'  + {label(f)}')
    print()
    print(f'--- deliberately dropped runs ({len(DROPPED)}) ---')
    for n, why in DROPPED.items():
        print(f'  ! {n}: {why}')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
