#!/usr/bin/env bash
# Probe every anvil-*.el module on the standalone NeLisp runtime, one fresh
# process per module.  Usage: standalone-coverage-census.sh OUT_DIR [MODULE...]
# Writes OUT_DIR/results.tsv and OUT_DIR/logs/MODULE.log.
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$script_dir/../.." && pwd)"
out="${1:?usage: $0 OUT_DIR [MODULE...]}"; shift
mkdir -p "$out/logs" "$out/work" || exit 1
out="$(cd "$out" && pwd)"
if [[ $# -gt 0 ]]; then mods=("$@"); else
  mods=($(cd "$repo" && ls anvil-*.el | sed 's/\.el$//'))
fi
nelisp_bin="${NELISP_BIN:-$(command -v nelisp)}"
probe_one() {
  local m="$1" log="$out/logs/$1.log" line
  ( ulimit -v 8388608
    cd "$out/work" || exit 1
    ANVIL_EL_DIR="$repo" ANVIL_NOTES_DIR="${ANVIL_NOTES_DIR:-$HOME/Cowork/Notes}" \
    ANVIL_MEMORY_DB="$out/work/memory.db" ANVIL_WORKLOG_DB="$out/work/worklog.db" \
    TMPDIR="$out/work" ANVIL_STANDALONE_TEST_NO_SERVER=1 ANVIL_COVERAGE_MODULE="$m" \
    timeout 120 "$nelisp_bin" --load "$repo/tools/anvil-standalone-mcp.el" \
      --load "$repo/tools/ai/standalone-coverage-probe.el" ) >"$log" 2>&1 </dev/null
  local rc=$?
  line="$(grep -a '^COVERAGE	' "$log" | tail -1)"
  if [[ -z "$line" ]]; then
    printf 'COVERAGE\t%s\tcrash\texit=%s %s\tn/a\t0\t\n' "$m" "$rc" \
      "$(grep -av '^$' "$log" | tail -1 | tr '\t' ' ' | cut -c1-200)"
  else
    printf '%s\n' "$line"
  fi
}
export -f probe_one; export out repo nelisp_bin
printf '%s\n' "${mods[@]}" | xargs -P "${CENSUS_JOBS:-4}" -I{} bash -c 'probe_one {}' \
  | sort > "$out/results.tsv"
wc -l < "$out/results.tsv"
