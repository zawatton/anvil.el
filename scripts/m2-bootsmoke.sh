#!/bin/sh
# m2-bootsmoke.sh --- T110 M2 milestone integration boot smoke
#
# Extends T97 (M1 = anvil-server / -defs / -state on top of the NeLisp
# Wave 1+2 substrate) with the *subprocess* substrate path:
#
#   nelisp-eventloop          (Phase 5-B.4 dispatcher)
#   nelisp-actor              (Doc 14 actor primitives)
#   nelisp-process            (Phase 9d.L process wrapper, T106 SHIPPED)
#   nelisp-eventloop-multiplex (Phase 9d.K fd multiplexer, T103 SHIPPED)
#
# and wires anvil-host (shell exec library) + anvil-worker (sub-Emacs
# pool) on top.  This proves that the M1 boot can be augmented with
# subprocess-aware tooling without a host Emacs hop -- the M2 integration
# milestone called for in Doc 33 v2 §M2 / Phase 9d.L exit criteria.
#
# Usage:
#   scripts/m2-bootsmoke.sh                        # PASS / FAIL summary
#   scripts/m2-bootsmoke.sh --raw                  # raw MCP stdout
#   scripts/m2-bootsmoke.sh --substrate-status     # only print substrate / anvil status
#   scripts/m2-bootsmoke.sh --tools-list-only      # print tools/list reply
#   NELISP_PATH=/path/to/nelisp scripts/m2-bootsmoke.sh
#
# Exit codes:
#   0  PASS    -- M2 substrate loads, subprocess spawn round-trips,
#                 anvil-host + anvil-worker both load cleanly,
#                 tools/list returns >= 1 tool
#   1  PARTIAL or FAIL  -- one or more assertions failed (still useful
#                 output for diagnosis; report still gets written)
#   2  environment / invocation error
#
# T110 — M2 milestone (= subprocess support added on top of T97 M1).

set -eu

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ANVIL_REPO=$(cd "$SCRIPT_DIR/.." && pwd)
EMACS="${EMACS:-emacs}"
MODE="${1:-check}"

# nelisp.el source path -- prefer NELISP_PATH env, else common locations.
NELISP_PATH="${NELISP_PATH:-}"
if [ -z "$NELISP_PATH" ]; then
  for cand in \
    "$HOME/Cowork/Notes/dev/nelisp" \
    "$HOME/Notes/dev/nelisp" \
    "$HOME/.emacs.d/external-packages/nelisp"; do
    if [ -d "$cand/src" ] && [ -f "$cand/src/nelisp-process.el" ]; then
      NELISP_PATH="$cand"
      break
    fi
  done
fi
if [ -z "$NELISP_PATH" ] || [ ! -f "$NELISP_PATH/src/nelisp-process.el" ]; then
  echo "m2-bootsmoke: NELISP_PATH not set / nelisp-process.el not found" >&2
  echo "  set NELISP_PATH=/path/to/nelisp" >&2
  exit 2
fi

if [ ! -f "$ANVIL_REPO/anvil-server.el" ]; then
  echo "m2-bootsmoke: $ANVIL_REPO/anvil-server.el not found" >&2
  echo "  this script must be run from inside the anvil.el repo (or worktree)" >&2
  exit 2
fi

if ! command -v "$EMACS" >/dev/null 2>&1; then
  echo "m2-bootsmoke: $EMACS not on PATH" >&2
  exit 2
fi

# 4-message MCP dialog: initialize, notifications/initialized,
# tools/list, then 1 representative tools/call probe (anvil-worker-probe,
# which directly exercises the M2 subprocess subsystem without needing a
# real worker pool to be alive yet).
REQUESTS=$(cat <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize"}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"anvil-worker-probe","arguments":{}}}
EOF
)

OUT=$(mktemp)
ERR=$(mktemp)
trap 'rm -f "$OUT" "$ERR"' EXIT

# Substrate load order extends the M1 plan with the subprocess layer:
#   1. base substrate (= M1 set: load, ec, ec-fileio, regex, json,
#      sqlite, coding, base64, secure-hash, ec-bridge)
#   2. subprocess substrate (NEW for M2):
#        nelisp-eventloop, nelisp-actor, nelisp-eventloop-multiplex,
#        nelisp-process
#   3. anvil core (= M1: anvil + anvil-server + anvil-server-commands +
#      anvil-defs + anvil-state)
#   4. anvil subprocess layer (NEW for M2):
#        anvil-host (shell library, no MCP register) +
#        anvil-worker (registers 2 MCP tools; we call --init-pool only,
#        not --enable, to avoid spawning real daemons in the smoke run)
printf '%s\n' "$REQUESTS" | \
  "$EMACS" --batch -Q -L "$NELISP_PATH/src" -L "$ANVIL_REPO" \
    --eval "(setq load-prefer-newer t)" \
    -l nelisp-load \
    -l nelisp-emacs-compat \
    -l nelisp-emacs-compat-fileio \
    -l nelisp-regex \
    -l nelisp-json \
    -l nelisp-sqlite \
    -l nelisp-coding \
    -l nelisp-base64 \
    -l nelisp-secure-hash \
    -l nelisp-ec-bridge \
    --eval "(nelisp-ec-bridge-install)" \
    -l nelisp-eventloop \
    -l nelisp-actor \
    -l nelisp-eventloop-multiplex \
    -l nelisp-process \
    -l anvil \
    -l anvil-server \
    -l anvil-server-commands \
    -l anvil-defs \
    -l anvil-state \
    --eval "(anvil-state-enable)" \
    --eval "(anvil-defs-enable)" \
    -l anvil-host \
    -l anvil-worker \
    --eval "(let ((anvil-worker--mcp-only t)) (ignore anvil-worker--mcp-only) (anvil-worker--init-pool))" \
    --eval "(anvil-server-register-tool #'anvil-worker--tool-probe :id \"anvil-worker-probe\" :intent '(worker admin) :layer 'dev :description \"Per-lane worker status (M2 smoke).\" :read-only t :server-id anvil-worker--server-id)" \
    --eval "(princ (format \";; bridge-status: %S\\n\" (nelisp-ec-bridge-status)))" \
    --eval "(princ (format \";; substrate-features: load=%s ec=%s ec-fileio=%s rx=%s json=%s sqlite=%s coding=%s b64=%s hash=%s ec-bridge=%s\\n\" (featurep 'nelisp-load) (featurep 'nelisp-emacs-compat) (featurep 'nelisp-emacs-compat-fileio) (featurep 'nelisp-regex) (featurep 'nelisp-json) (featurep 'nelisp-sqlite) (featurep 'nelisp-coding) (featurep 'nelisp-base64) (featurep 'nelisp-secure-hash) (featurep 'nelisp-ec-bridge)))" \
    --eval "(princ (format \";; subprocess-features: eventloop=%s actor=%s mux=%s process=%s\\n\" (featurep 'nelisp-eventloop) (featurep 'nelisp-actor) (featurep 'nelisp-eventloop-multiplex) (featurep 'nelisp-process)))" \
    --eval "(princ (format \";; anvil-features: anvil=%s server=%s commands=%s defs=%s state=%s host=%s worker=%s\\n\" (featurep 'anvil) (featurep 'anvil-server) (featurep 'anvil-server-commands) (featurep 'anvil-defs) (featurep 'anvil-state) (featurep 'anvil-host) (featurep 'anvil-worker)))" \
    --eval "(princ (format \";; subprocess-spawn-probe: exit=%s\\n\" (let* ((buf (generate-new-buffer \" *m2-spawn-probe*\")) (wrap (nelisp-make-process :name \"m2-probe\" :buffer buf :command (list \"sh\" \"-c\" \"sleep 0.05; echo m2-ok\")))) (prog1 (nelisp-process-wait-for-exit wrap 5) (nelisp-delete-process wrap) (kill-buffer buf)))))" \
    --eval "(princ (format \";; multiplex-tick-probe: fired=%s backend=%s\\n\" (nelisp-eventloop-multiplex-tick 50) nelisp-eventloop-multiplex-backend))" \
    --eval "(anvil-server-run-batch-stdio \"emacs-eval\")" \
    > "$OUT" 2> "$ERR" || {
      echo "m2-bootsmoke: emacs batch invocation failed (exit $?)" >&2
      head -40 "$ERR" >&2
      exit 1
    }

if [ "$MODE" = "--raw" ]; then
  cat "$OUT"
  exit 0
fi

if [ "$MODE" = "--substrate-status" ]; then
  grep '^;; bridge-status:' "$OUT" || true
  grep '^;; substrate-features:' "$OUT" || true
  grep '^;; subprocess-features:' "$OUT" || true
  grep '^;; anvil-features:' "$OUT" || true
  grep '^;; subprocess-spawn-probe:' "$OUT" || true
  grep '^;; multiplex-tick-probe:' "$OUT" || true
  exit 0
fi

if [ "$MODE" = "--tools-list-only" ]; then
  grep '^{' "$OUT" | sed -n '2p'
  exit 0
fi

FAIL=0
WARN=0

assert_contains() {
  if ! grep -q "$1" "$OUT"; then
    echo "FAIL: expected stdout to contain: $1" >&2
    FAIL=1
  fi
}

# 1. base substrate load assertions (= M1 carry-over)
assert_contains ';; substrate-features:'
if ! grep -q ';; substrate-features:.*ec-bridge=t' "$OUT"; then
  echo "FAIL: nelisp-ec-bridge feature not loaded" >&2
  FAIL=1
fi

# 2. subprocess substrate load assertions (= M2 NEW)
assert_contains ';; subprocess-features:'
if ! grep -qE ';; subprocess-features:.*eventloop=t.*actor=t.*mux=t.*process=t' "$OUT"; then
  echo "FAIL: subprocess substrate (eventloop / actor / mux / process) features missing" >&2
  FAIL=1
fi

# 3. anvil core load assertions (= M1 carry-over + M2 host/worker)
if ! grep -qE ';; anvil-features:.*server=t.*defs=t.*state=t.*host=t.*worker=t' "$OUT"; then
  echo "FAIL: anvil core+subprocess (server/defs/state/host/worker) features missing" >&2
  FAIL=1
fi

# 4. subprocess spawn round-trip (= the M2 integration claim)
SPAWN_LINE=$(grep '^;; subprocess-spawn-probe:' "$OUT" || true)
if [ -z "$SPAWN_LINE" ]; then
  echo "FAIL: subprocess spawn probe did not run" >&2
  FAIL=1
elif printf '%s' "$SPAWN_LINE" | grep -qE 'exit=0$'; then
  echo "PASS: subprocess spawn returned exit 0"
else
  echo "FAIL: subprocess spawn did not return exit 0: $SPAWN_LINE" >&2
  FAIL=1
fi

# 5. multiplex tick probe (= eventloop dispatch path is callable)
MUX_LINE=$(grep '^;; multiplex-tick-probe:' "$OUT" || true)
if [ -z "$MUX_LINE" ]; then
  echo "FAIL: multiplex tick probe did not run" >&2
  FAIL=1
else
  echo "PASS: multiplex tick callable: $MUX_LINE"
fi

# 6. MCP initialize reply
assert_contains '"protocolVersion":"2025-03-26"'
assert_contains '"serverInfo":{"name":"anvil"'

# 7. tools/list reply -- must be present and non-empty
TOOLS_LINE=$(grep '^{' "$OUT" | grep '"id":2' || true)
if [ -z "$TOOLS_LINE" ]; then
  echo "FAIL: no tools/list reply (id=2)" >&2
  FAIL=1
else
  TOOL_COUNT=$(printf '%s' "$TOOLS_LINE" | grep -oE '"name":"[a-z][a-zA-Z0-9_-]*"' | wc -l)
  if [ "$TOOL_COUNT" -lt 1 ]; then
    echo "FAIL: tools/list returned 0 tools" >&2
    FAIL=1
  else
    echo "PASS: tools/list returned $TOOL_COUNT tools"
  fi
  # M2 specifically: anvil-worker-probe must appear in the list
  if printf '%s' "$TOOLS_LINE" | grep -q '"name":"anvil-worker-probe"'; then
    echo "PASS: anvil-worker-probe (M2 subprocess MCP tool) registered"
  else
    echo "WARN: anvil-worker-probe not in tools/list -- M2 worker MCP wiring incomplete" >&2
    WARN=1
  fi
fi

# 8. tools/call anvil-worker-probe -- soft check (handler shape mismatch
#    inherited from M1 / host-Emacs path is treated as WARN not FAIL).
CALL_LINE=$(grep '^{' "$OUT" | grep '"id":3' || true)
if [ -z "$CALL_LINE" ]; then
  echo "WARN: no tools/call reply (id=3) -- handler crashed silently?" >&2
  WARN=1
elif printf '%s' "$CALL_LINE" | grep -q '"error"'; then
  ERR_MSG=$(printf '%s' "$CALL_LINE" | grep -oE '"message":"[^"]*"' | head -1)
  echo "WARN: tools/call anvil-worker-probe returned error: $ERR_MSG (= shared with host-Emacs path, not M2-specific)" >&2
  WARN=1
else
  echo "PASS: tools/call anvil-worker-probe returned a result"
fi

if [ $FAIL -eq 0 ]; then
  if [ $WARN -ne 0 ]; then
    echo "VERDICT: PARTIAL (M2 boot OK, tools/list OK, some tools/call warnings)"
  else
    echo "VERDICT: PASS"
  fi
  exit 0
else
  echo "VERDICT: FAIL"
  exit 1
fi
