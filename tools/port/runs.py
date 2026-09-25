#!/usr/bin/env python3
"""Shared: turn a tangled .el file into ordered runs of top-level chunks."""
import re

OPEN = re.compile(r'^;; \[\[file:([^:]+)::\*(.+?)\]\[(.+?)\]\]$')
CLOSE = re.compile(r'^;; (.+?) ends here$')


def load_runs(path):
    """Return (lines, preamble, runs).

    preamble: list of lines before the first chunk (the lexical cookie).
    runs: list of dicts {name, start, end, body} where body is the chunk text
          with all breadcrumb lines removed, 1-indexed inclusive line range.
    """
    lines = open(path, encoding='utf-8').read().split('\n')
    events = []
    for i, line in enumerate(lines, 1):
        m = OPEN.match(line)
        if m:
            events.append(('open', i, m.group(2), m.group(3)))
            continue
        m = CLOSE.match(line)
        if m:
            events.append(('close', i, None, m.group(1)))

    top = []
    stack = []
    for kind, ln, heading, label in events:
        if kind == 'open':
            stack.append((heading, label, ln))
        else:
            if not stack:
                raise SystemExit(f'unmatched close at {ln}: {label}')
            h, lab, start = stack.pop()
            if lab != label:
                raise SystemExit(f'mismatch {lab}@{start} / {label}@{ln}')
            if not stack:
                top.append((h, start, ln))
    if stack:
        raise SystemExit(f'unclosed: {stack}')

    first = top[0][1] if top else len(lines) + 1
    preamble = [l for l in lines[:first - 1] if l.strip()]

    # Merge adjacent chunks with the same heading into one run.
    runs = []
    for h, s, e in top:
        if runs and runs[-1]['name'] == h:
            runs[-1]['end'] = e
        else:
            runs.append({'name': h, 'start': s, 'end': e})

    for r in runs:
        body = lines[r['start'] - 1:r['end']]
        r['body'] = [l for l in body
                     if not OPEN.match(l) and not CLOSE.match(l)]
        r['size'] = r['end'] - r['start'] + 1
    return lines, preamble, runs


if __name__ == '__main__':
    import sys
    _, pre, runs = load_runs(sys.argv[1])
    print(f'# preamble: {pre}')
    for i, r in enumerate(runs, 1):
        print(f'{i:4d}  {r["size"]:5d}  {r["name"]}')
    print(f'# {len(runs)} runs, {sum(r["size"] for r in runs)} lines in chunks')
