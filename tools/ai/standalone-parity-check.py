#!/usr/bin/env python3
"""Check standalone tool parity with the host emacs-eval server.

The host tool set is a fixed snapshot (standalone-host-tool-snapshot.txt);
standalone-parity-waves.tsv assigns every host-only tool to a wave (1, 2, 3)
or marks it host-bound (excluded: it observes or drives the host daemon).

  standalone-parity-check.py --wave 1
      Load the standalone loader without a server and exit 0 iff every
      wave-1 tool is registered.
  standalone-parity-check.py --wave 1 --calls CALLS.json
      Also run CALLS.json through standalone-mcp-call.py (the real launcher)
      and require an OK line for every wave-1 tool.

--wave accepts a comma list (for example 1,2,3); wave 0 is the snapshot tools
already shared when the snapshot was taken.  Missing tools are printed
one per line as "MISSING TOOL" or "UNCALLED TOOL".
"""

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent


def wave_tools(waves):
    """Return the tools of WAVES; wave 0 is the snapshot tools shared at 2026-11-03."""
    tools, assigned = [], set()
    for line in (HERE / "standalone-parity-waves.tsv").read_text().splitlines():
        name, _, wave = line.partition("\t")
        assigned.add(name)
        if wave in waves:
            tools.append(name)
    if "0" in waves:
        snapshot = (HERE / "standalone-host-tool-snapshot.txt").read_text().split()
        tools += [t for t in snapshot if t not in assigned]
    return tools


def registered_tools():
    with tempfile.TemporaryDirectory() as tmp:
        out = Path(tmp) / "tools.txt"
        probe = Path(tmp) / "probe.el"
        probe.write_text(
            '(let (r) (maphash (lambda (_sid tbl) (when (hash-table-p tbl)'
            ' (maphash (lambda (k _) (push k r)) tbl))) anvil-server--tools)'
            ' (with-temp-file "%s" (insert (mapconcat (quote identity) r "\\n") "\\n")))\n'
            % out)
        env = dict(os.environ)
        env.setdefault("ANVIL_NOTES_DIR", str(Path.home() / "Cowork" / "Notes"))
        env.update(ANVIL_EL_DIR=str(REPO), ANVIL_MEMORY_DB=str(Path(tmp) / "m.db"),
                   ANVIL_WORKLOG_DB=str(Path(tmp) / "w.db"), TMPDIR=tmp,
                   ANVIL_STANDALONE_TEST_NO_SERVER="1")
        cmd = ("ulimit -v 8388608; exec timeout 280 nelisp --load %s --load %s"
               % (REPO / "tools" / "anvil-standalone-mcp.el", probe))
        subprocess.run(["bash", "-c", cmd], cwd=tmp, env=env,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if not out.exists():
            sys.exit("parity-check: loader probe produced no tool list")
        return set(out.read_text().split())


def called_tools(calls):
    run = subprocess.run([sys.executable, str(HERE / "standalone-mcp-call.py"), calls],
                         capture_output=True, text=True, timeout=600)
    ok = set()
    for line in run.stdout.splitlines():
        if line.startswith("OK   "):
            ok.add(line[5:].split(" ", 1)[0])
    return run.returncode, ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--wave", required=True)
    ap.add_argument("--calls")
    args = ap.parse_args()
    tools = wave_tools(set(args.wave.split(",")))
    if not tools:
        sys.exit("parity-check: no tools in wave %s" % args.wave)
    have = registered_tools()
    missing = [t for t in tools if t not in have]
    for t in missing:
        print("MISSING", t)
    failed = bool(missing)
    if args.calls:
        rc, ok = called_tools(args.calls)
        uncalled = [t for t in tools if t not in ok]
        for t in uncalled:
            print("UNCALLED", t)
        failed = failed or rc != 0 or bool(uncalled)
    print("PARITY wave=%s tools=%d missing=%d%s"
          % (args.wave, len(tools), len(missing), "" if not args.calls else " calls-rc=%d" % rc))
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
