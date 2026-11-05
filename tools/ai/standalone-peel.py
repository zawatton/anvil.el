#!/usr/bin/env python3
"""Peel missing-definition blockers on the anvil NeLisp standalone runtime.

Usage: standalone-peel.py SPEC.json
SPEC: {"form": "<elisp>"} or {"tool": "<mcp tool id>", "args": {...}}, plus
optional "max_fixes" (30), "vendor_dirs", "setup" (list of forms),
"result_chars" (500).  Relative vendor_dirs resolve against the repo root and
"@REPO@" expands to it.  Runs ONE fresh standalone process under an 8 GiB
address-space cap with the real anvil-standalone-mcp.el loaded (server not
started) and prints exactly one compact JSON object.  Fixes are never persisted.
"""
import json, os, resource, shutil, subprocess, sys, tempfile

repo = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def lit(v):
    """Python value -> elisp literal."""
    if isinstance(v, bool):
        return "t" if v else "nil"
    if v is None:
        return "nil"
    if isinstance(v, (int, float)):
        return repr(v)
    if isinstance(v, str):
        return '"' + v.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(v, list):
        return "(" + " ".join(lit(x) for x in v) + ")"
    if isinstance(v, dict):
        return "(" + " ".join("(%s . %s)" % (lit(k), lit(x)) for k, x in v.items()) + ")"
    raise TypeError(type(v))


def main():
    spec = json.loads(open(sys.argv[1]).read().replace("@REPO@", repo))
    work = tempfile.mkdtemp(prefix="standalone-peel-")
    parts = []
    if "tool" in spec:
        parts.append(":tool " + lit(spec["tool"]))
        parts.append(":args " + "'" + lit(spec.get("args") or {}))
    else:
        parts.append(":form " + lit(spec.get("form", "nil")))
    parts.append(":max-fixes %d" % int(spec.get("max_fixes", 30)))
    parts.append(":result-chars %d" % int(spec.get("result_chars", 500)))
    if spec.get("setup"):
        parts.append(":setup " + lit(spec["setup"]))
    if spec.get("vendor_dirs"):
        dirs = [d if os.path.isabs(d) else os.path.join(repo, d) for d in spec["vendor_dirs"]]
        parts.append(":vendor-dirs " + lit(dirs))
    spec_file = os.path.join(work, "spec.eld")
    # ":args '(...)" is read as (quote ...); unwrap it for the plist.
    text = "(" + " ".join(parts).replace(":args '", ":args ") + ")"
    open(spec_file, "w").write(text)
    nelisp = os.environ.get("NELISP_BIN") or shutil.which("nelisp")
    cache = os.path.join(work, "xdg")
    env = dict(os.environ, ANVIL_EL_DIR=repo, XDG_CACHE_HOME=cache, TMPDIR=work,
               ANVIL_MEMORY_DB=os.path.join(work, "m.db"), ANVIL_WORKLOG_DB=os.path.join(work, "w.db"),
               ANVIL_STANDALONE_CWD=work, ANVIL_STANDALONE_TEST_NO_SERVER="1",
               ANVIL_PEEL_SPEC=spec_file)
    env.setdefault("ANVIL_NOTES_DIR", os.path.expanduser("~/Cowork/Notes"))
    lim = lambda: resource.setrlimit(resource.RLIMIT_AS, (8 << 30, 8 << 30))
    timeout = int(spec.get("timeout", 240))
    out = b""
    try:
        p = subprocess.run([nelisp, "--load", os.path.join(repo, "tools/anvil-standalone-mcp.el"),
                            "--load", os.path.join(repo, "tools/ai/standalone-peel.el")],
                           cwd=work, env=env, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                           stderr=subprocess.STDOUT, preexec_fn=lim, timeout=timeout)
        out, rc = p.stdout, p.returncode
    except subprocess.TimeoutExpired as e:
        out, rc = e.stdout or b"", "timeout"
    except (OSError, TypeError) as e:
        print(json.dumps({"status": "harness-error", "error": {"class": "spawn", "data": str(e)[:300]}}))
        return 2
    finally:
        shutil.rmtree(work, ignore_errors=True)
    for line in out.decode(errors="replace").splitlines():
        if line.startswith("PEEL-RESULT "):
            try:
                print(json.dumps(json.loads(line[len("PEEL-RESULT "):]), separators=(",", ":")))
                return 0
            except ValueError:
                print(json.dumps({"status": "harness-error", "error": {"class": "bad-json",
                      "data": line[:300]}}, separators=(",", ":")))
                return 2
    tail = [l for l in out.decode(errors="replace").splitlines() if l.strip()][-3:]
    print(json.dumps({"status": "harness-error", "error": {"class": "no-result",
          "data": ("rc=%s " % rc + " | ".join(tail))[:300]}}, separators=(",", ":")))
    return 2


sys.exit(main())
