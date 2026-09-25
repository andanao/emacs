#!/usr/bin/env python3
"""Join tangled-chunk runs to the readme.org outline.

Prints each run in tangle order with its level-1/2/3 heading path, tags and
size, so the grouping into files can be designed against real numbers.
"""
import re
import sys

HEADING = re.compile(r'^(\*+)\s+(.*?)(?:\s+(:[\w:@]+:))?\s*$')
sys.path.insert(0, '/tmp/emacs-port')
from analyze import parse, chunks  # noqa: E402


def outline(path):
    """Return list of (level, title, tags, line)."""
    out = []
    in_block = False
    for i, line in enumerate(open(path, encoding='utf-8'), 1):
        low = line.lower().rstrip()
        if low.startswith('#+begin_'):
            in_block = True
        elif low.startswith('#+end_'):
            in_block = False
        if in_block:
            continue
        m = HEADING.match(line.rstrip('\n'))
        if m:
            out.append((len(m.group(1)), m.group(2).strip(),
                        m.group(3) or '', i))
    return out


def main(org, el):
    heads = outline(org)
    by_title = {}
    for lvl, title, tags, line in heads:
        by_title.setdefault(title, []).append((lvl, tags, line))

    _, events = parse(el)
    top = chunks(events)
    runs = []
    for h, s, e in top:
        if runs and runs[-1][0] == h:
            runs[-1][2] = e
        else:
            runs.append([h, s, e])

    dupes = {t: v for t, v in by_title.items() if len(v) > 1}
    if dupes:
        print(f'!! duplicate titles in outline: {len(dupes)}', file=sys.stderr)
        for t, v in dupes.items():
            print(f'   {t}: {v}', file=sys.stderr)

    path = {}          # running level -> title
    pos = {}
    for lvl, title, tags, line in heads:
        pos[line] = (lvl, title, tags)

    ordered = sorted(pos.items())
    missing = []
    print(f'{"n":>3} {"lines":>6} {"lvl":>3}  {"tags":<10} L1 / L2 / L3')
    print('-' * 100)
    for n, (h, s, e) in enumerate(runs, 1):
        info = by_title.get(h)
        if not info:
            missing.append(h)
            print(f'{n:3d} {e-s+1:6d}   ?  {"":<10} ?? {h}')
            continue
        lvl, tags, line = info[0]
        # walk back for ancestors
        anc = {}
        for ln, (l2, t2, g2) in ordered:
            if ln > line:
                break
            anc[l2] = (t2, g2)
        p1 = anc.get(1, ('', ''))[0]
        p2 = anc.get(2, ('', ''))[0] if lvl >= 2 else ''
        p3 = anc.get(3, ('', ''))[0] if lvl >= 3 else ''
        parts = [p1]
        if lvl >= 2:
            parts.append(p2)
        if lvl >= 3:
            parts.append(p3)
        print(f'{n:3d} {e-s+1:6d} {lvl:3d}  {tags:<10} {" / ".join(parts)}')
    if missing:
        print(f'\n!! runs with no matching heading: {missing}', file=sys.stderr)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
