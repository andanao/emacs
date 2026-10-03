#!/usr/bin/env python3
"""Faithfulness gate: does the new tree still say what the tangled file said?

Usage: verify.py <reference-dir> <repo-root>

Two diffs, because they catch different things:

  sequence  concatenate the new files in load order and compare the line
            sequence to the tangled original.  Catches content changes and
            reordering alike.
  sorted    sort top-level forms on both sides first.  Catches content
            changes only, and is the gate the spec asks for.
"""
import difflib
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from runs import load_runs, OPEN, CLOSE         # noqa: E402
from manifest import MANIFEST, DROPPED          # noqa: E402

CODE = re.compile(r'^;;; Code:$')
ENDS = re.compile(r'^;;; \S+ ends here$')


def strip_header(lines, base):
    """Drop the generated header (through ';;; Code:') and the footer."""
    out = list(lines)
    for i, line in enumerate(out):
        if CODE.match(line):
            out = out[i + 1:]
            break
    else:
        raise SystemExit(f'{base}: no ";;; Code:" marker')
    while out and not out[-1].strip():
        out.pop()
    if out and ENDS.match(out[-1].strip()):
        out.pop()
    return out


def normalise(lines):
    """Blank lines out, trailing whitespace off."""
    return [l.rstrip() for l in lines if l.strip()]


def top_forms(lines):
    """Split a line sequence into top-level forms, paren-depth aware.

    String state carries across lines, because elisp docstrings routinely
    span several and contain unbalanced parens.
    """
    forms, cur, depth, in_string = [], [], 0, False
    for line in lines:
        cur.append(line)
        i, n = 0, len(line)
        while i < n:
            c = line[i]
            if in_string:
                if c == '\\':
                    i += 2
                    continue
                if c == '"':
                    in_string = False
                i += 1
                continue
            if c == ';':
                break
            if c == '"':
                in_string = True
            elif c == '?':
                # Character literal: ?a, ?\(, ?\C-x.  Skip what follows so a
                # paren written as a character does not move the depth.
                i += 3 if line[i + 1:i + 2] == '\\' else 2
                continue
            elif c == '(':
                depth += 1
            elif c == ')':
                depth -= 1
            i += 1
        if depth <= 0 and not in_string and cur:
            forms.append('\n'.join(cur))
            cur, depth = [], 0
    if cur:
        forms.append('\n'.join(cur))
    return forms


def show(label, a, b, limit=60):
    diff = list(difflib.unified_diff(a, b, 'tangled', 'new', lineterm='', n=1))
    if not diff:
        print(f'{label}: EMPTY')
        return 0
    body = [d for d in diff if d[:1] in '+-' and d[:3] not in ('---', '+++')]
    print(f'{label}: {len(body)} differing lines')
    for line in diff[:limit]:
        print('   ' + line)
    if len(diff) > limit:
        print(f'   ... {len(diff) - limit} more')
    return len(body)


def main(refdir, root):
    _, preamble, runs = load_runs(os.path.join(refdir, 'init.el'))
    idx = {r['name']: i for i, r in enumerate(runs, 1)}

    dropped_lines = []
    for n in DROPPED:
        dropped_lines += runs[idx[n] - 1]['body']

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
        full = os.path.join(root, path)
        lines = open(full, encoding='utf-8').read().split('\n')
        new += strip_header(lines, path)
    new = normalise(new)

    print(f'tangled runs: {len(runs)}   dropped: {sorted(DROPPED)}')
    print(f'reference lines: {len(ref)}   new lines: {len(new)}')
    print()
    a = show('sequence diff', ref, new)
    print()
    b = show('sorted-form diff',
             sorted(top_forms(ref)), sorted(top_forms(new)))
    print()
    if dropped_lines:
        print('deliberately dropped:')
        for n, reason in DROPPED.items():
            print(f'  run {idx[n]} ({n}): {reason}')
    return 0 if (a == 0 and b == 0) else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1], sys.argv[2]))
