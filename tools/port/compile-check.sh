#!/usr/bin/env bash
# Byte-compile the config against the flake's package set.
#
# Compiles a copy, so no .elc lands in the repo.  Warnings of the form
# "the function ... might not be defined at runtime" are expected for
# anything a later use-package loads, and are filtered out.
set -uo pipefail

EMACS=${1:?usage: compile-check.sh /path/to/emacs /path/to/config}
CONFIG=${2:?usage: compile-check.sh /path/to/emacs /path/to/config}

WORK=$(mktemp -d /tmp/emacs-compile-check.XXXXXX)
trap 'rm -rf "$WORK"' EXIT

for item in early-init.el init.el mac.el linux.el ms-windows.el config lisp; do
  [ -e "$CONFIG/$item" ] && cp -R "$CONFIG/$item" "$WORK/"
done

RAW="$WORK/compile.log"

cd "$WORK" || exit 1
# Deliberately no -L for config/ or lisp/.  Putting them on load-path is
# what makes (require 'dired) find config/dired.el, which is the shadowing
# the loader exists to avoid; doing it here would only manufacture errors
# the real startup cannot hit.  Package directories arrive via the wrapper.
"$EMACS" --batch \
  --eval '(setq byte-compile-warnings t load-prefer-newer t)' \
  --eval '(package-activate-all)' \
  -f batch-byte-compile \
  early-init.el init.el mac.el config/*.el config/org/*.el lisp/*.el \
  > "$RAW" 2>&1
status=$?

total=$(find "$WORK" -name '*.elc' | wc -l | tr -d ' ')
echo "produced $total .elc files, batch-byte-compile exit $status"
echo

echo "=== errors ==="
grep -nE '^[^ ]+\.el:[0-9]+:[0-9]+: (Error|error)|^Error|Symbol.s value as variable' "$RAW" \
  | head -40 || true
grep -qE 'Error|error' "$RAW" || echo "  none"
echo

echo "=== warnings, excluding the expected 'might not be defined at runtime' ==="
grep -E 'Warning' "$RAW" \
  | grep -v 'might not be defined at runtime' \
  | grep -v 'assignment to free variable' \
  | grep -v 'reference to free variable' \
  | sed 's/^/  /' | head -60
echo

echo "=== counts ==="
printf '  %-52s %s\n' "'might not be defined at runtime' (expected)" \
  "$(grep -c 'might not be defined at runtime' "$RAW")"
printf '  %-52s %s\n' "free variable warnings" \
  "$(grep -cE 'free variable' "$RAW")"
printf '  %-52s %s\n' "other warnings" \
  "$(grep -E 'Warning' "$RAW" | grep -vc -e 'might not be defined at runtime' -e 'free variable')"
printf '  %-52s %s\n' "errors" "$(grep -cE ': Error|^Error' "$RAW")"

cp "$RAW" /tmp/emacs-compile-check.log
echo
echo "full log: /tmp/emacs-compile-check.log"
exit $status
