#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export ANVIL_EL_DIR="${ANVIL_EL_DIR:-$(cd "$script_dir/.." && pwd)}"
if [[ -z "${ANVIL_NOTES_DIR:-}" && -d "$HOME/Cowork/Notes/dev/nelisp-system-one/lisp" ]]; then
    export ANVIL_NOTES_DIR="$HOME/Cowork/Notes"
fi
if [[ -n "${ANVIL_NOTES_DIR:-}" ]]; then
    export ANVIL_MEMORY_DB="${ANVIL_MEMORY_DB:-$ANVIL_NOTES_DIR/.anvil-memory/anvil-memory-index.db}"
    export ANVIL_WORKLOG_DB="${ANVIL_WORKLOG_DB:-$ANVIL_NOTES_DIR/.anvil-worklog/anvil-worklog-index.db}"
fi
if [[ -z "${HOSTNAME:-}" ]] && command -v hostname >/dev/null 2>&1; then
    HOSTNAME="$(hostname 2>/dev/null || true)"
fi
export HOSTNAME="${HOSTNAME:-${COMPUTERNAME:-${HOST:-localhost}}}"
nelisp_bin="${NELISP_BIN:-$(command -v nelisp)}"
export NELISP_BIN="$nelisp_bin"

if [[ -z "${ANVIL_STANDALONE_NO_IMAGE:-}${ANVIL_STANDALONE_CONTEXT_WORKER:-}${ANVIL_STANDALONE_TEST_NO_SERVER:-}" ]] && command -v python3 >/dev/null 2>&1; then
    exec python3 "$script_dir/anvil-standalone-image.py"
fi

exec "$nelisp_bin" --load "$script_dir/anvil-standalone-mcp.el"
