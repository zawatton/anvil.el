#!/usr/bin/env python3
"""Start a private anvil-standalone server through the real launcher and call tools.

Usage: standalone-mcp-call.py CALLS_JSON
CALLS_JSON is a JSON list of [tool-name, {arguments}].  The server runs with a
private XDG cache, scratch databases and cwd (never the shared daemon), under an
8 GiB address-space cap.  Prints "tools/list: N" and one OK/FAIL line per call;
exits 0 only when every call succeeds.
"""
import json, os, resource, subprocess, sys, tempfile

repo = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
work = tempfile.mkdtemp(prefix="anvil-mcp-call-")
# @REPO@ -> repo root, @TMP@ -> private work dir
calls = json.loads(open(sys.argv[1]).read().replace("@REPO@", repo).replace("@TMP@", work))
cache = os.environ.get("ANVIL_MCP_CALL_CACHE") or os.path.join(work, "xdg")
env = dict(os.environ, ANVIL_EL_DIR=repo, XDG_CACHE_HOME=cache, TMPDIR=work,
           ANVIL_MEMORY_DB=os.path.join(work, "m.db"), ANVIL_WORKLOG_DB=os.path.join(work, "w.db"),
           ANVIL_STANDALONE_CWD=work)
env.setdefault("ANVIL_NOTES_DIR", os.path.expanduser("~/Cowork/Notes"))
lim = lambda: resource.setrlimit(resource.RLIMIT_AS, (8 << 30, 8 << 30))
p = subprocess.Popen(["bash", os.path.join(repo, "tools/anvil-standalone-mcp.sh")], cwd=work, env=env,
                     stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=open(os.path.join(work, "stderr.log"), "w"), preexec_fn=lim)
msgs = [{"jsonrpc": "2.0", "id": 0, "method": "initialize", "params": {"protocolVersion": "2025-06-18",
         "capabilities": {}, "clientInfo": {"name": "standalone-mcp-call", "version": "0"}}},
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        {"jsonrpc": "2.0", "id": 1, "method": "tools/list"}]
msgs += [{"jsonrpc": "2.0", "id": i, "method": "tools/call", "params": {"name": n, "arguments": a}}
         for i, (n, a) in enumerate(calls, 2)]
try:
    out, _ = p.communicate("".join(json.dumps(m) + "\n" for m in msgs).encode(), timeout=int(os.environ.get("ANVIL_MCP_CALL_TIMEOUT", "240")))
except subprocess.TimeoutExpired:
    p.kill(); out, _ = p.communicate(); print("TIMEOUT")
ok = set()
for line in out.decode(errors="replace").splitlines():
    try:
        r = json.loads(line)
    except ValueError:
        continue
    if r.get("id") == 1:
        print("tools/list:", len(r["result"]["tools"]))
    elif isinstance(r.get("id"), int) and r["id"] >= 2:
        res = r.get("result") or {}
        good = not r.get("error") and not res.get("isError")
        if good:
            ok.add(r["id"])
        print(("OK   " if good else "FAIL ") + calls[r["id"] - 2][0],
              json.dumps(r.get("result") or r.get("error"), ensure_ascii=False)[:500])
sys.exit(0 if len(ok) == len(calls) else 1)
