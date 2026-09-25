#!/usr/bin/env bash
# Start the new config under a private daemon name and report any backtrace.
#
# Uses its own server socket, so it cannot touch the running editor.
set -uo pipefail

EMACS=${1:?usage: startup-check.sh /path/to/emacs /path/to/config}
CONFIG=${2:?usage: startup-check.sh /path/to/emacs /path/to/config}
SERVER="port-check-$$"
LOG=$(mktemp /tmp/emacs-startup-check.XXXXXX)

echo "emacs:  $EMACS"
echo "config: $CONFIG"
echo "server: $SERVER"
echo "log:    $LOG"
echo

cleanup() {
  "${EMACS}client" -s "$SERVER" --eval '(kill-emacs)' >/dev/null 2>&1
  sleep 1
  # --daemon=X is reported as --bg-daemon=X with the name on its own line,
  # so match the name alone rather than the flag.
  pkill -f "$SERVER" >/dev/null 2>&1
}
trap cleanup EXIT

# debug-on-error is already set by early-init; --debug-init makes a failure
# during init print a backtrace instead of a one-line message.
timeout "${STARTUP_TIMEOUT:-300}" "$EMACS" \
  --init-directory "$CONFIG" \
  --debug-init \
  --daemon="$SERVER" > "$LOG" 2>&1
status=$?

echo "=== daemon exit: $status ==="
echo

if grep -qiE '^Debugger entered|Backtrace|error in process filter|^Error' "$LOG"; then
  echo "=== BACKTRACE OR ERROR ==="
  grep -niE 'Debugger entered|Backtrace|^ *[a-z-]+\(|^Error|error' "$LOG" | head -40
  echo
fi

echo "=== full startup output ($(wc -l < "$LOG") lines) ==="
cat "$LOG"

if [ $status -eq 0 ]; then
  echo
  echo "=== daemon is up, asking it a question ==="
  "${EMACS}client" -s "$SERVER" --eval \
    '(list :features (length features) :packages (length package-activated-list) :errors (if debug-on-error :on :off))' \
    2>&1
fi

exit $status
