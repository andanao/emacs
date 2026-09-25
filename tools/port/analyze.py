#!/usr/bin/env python3
"""Parse org :comments link breadcrumbs in a tangled .el file.

Emits the ordered list of top-level chunks with their org heading and line
range, and validates that every opener has a matching closer.
"""
import re
import sys
from collections import OrderedDict

OPEN = re.compile(r'^;; \[\[file:([^:]+)::\*(.+?)\]\[(.+?)\]\]$')
CLOSE = re.compile(r'^;; (.+?) ends here$')


def parse(path):
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
    return lines, events


def chunks(events):
    """Walk events, tracking depth. Yield top-level (heading, start, end)."""
    out = []
    stack = []
    for kind, line, heading, label in events:
        if kind == 'open':
            stack.append((heading, label, line))
        else:
            if not stack:
                print(f'  UNMATCHED CLOSE at line {line}: {label}', file=sys.stderr)
                continue
            h, lab, start = stack.pop()
            if lab != label:
                print(f'  MISMATCH open {lab}@{start} vs close {label}@{line}',
                      file=sys.stderr)
            if not stack:
                out.append((h, start, line))
    if stack:
        for h, lab, start in stack:
            print(f'  UNCLOSED open at line {start}: {lab}', file=sys.stderr)
    return out


def main(path):
    lines, events = parse(path)
    print(f'file: {path}')
    print(f'lines: {len(lines)}')
    print(f'openers: {sum(1 for e in events if e[0] == "open")}')
    print(f'closers: {sum(1 for e in events if e[0] == "close")}')
    print('--- validation ---')
    top = chunks(events)
    print(f'top-level chunks: {len(top)}')

    # Coverage: which lines are outside any chunk?
    covered = set()
    for _, s, e in top:
        covered.update(range(s, e + 1))
    uncovered = [i for i in range(1, len(lines) + 1)
                 if i not in covered and lines[i - 1].strip()]
    print(f'non-blank lines outside chunks: {len(uncovered)}')
    for i in uncovered[:20]:
        print(f'    {i}: {lines[i-1]!r}')

    # Section runs, in order. Consecutive chunks of same heading collapse.
    print('--- section runs (in tangle order) ---')
    runs = []
    for h, s, e in top:
        if runs and runs[-1][0] == h:
            runs[-1][2] = e
            runs[-1][3] += 1
        else:
            runs.append([h, s, e, 1])
    for h, s, e, n in runs:
        print(f'{e - s + 1:6d} lines  {n:3d} blk  {h}')
    print(f'--- {len(runs)} runs ---')

    # Headings that appear in more than one run (non-contiguous).
    seen = OrderedDict()
    for h, s, e, n in runs:
        seen.setdefault(h, []).append((s, e))
    split = {h: v for h, v in seen.items() if len(v) > 1}
    print(f'--- headings split across non-adjacent runs: {len(split)} ---')
    for h, v in split.items():
        print(f'  {h}: {v}')


if __name__ == '__main__':
    main(sys.argv[1])
