#!/usr/bin/env python3
"""Deterministic readiness and atomic-dispatch regressions for anvil-stdio."""

from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor, TimeoutError as FutureTimeoutError
import json
import os
import signal
import stat
import subprocess
import sys
import tempfile
import time
from pathlib import Path
import re


def make_executable(path: Path, source: str) -> None:
    path.write_text(source, encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR)


def percent_wire(document: dict[str, object]) -> str:
    return "".join(
        f"%{byte:02X}"
        for byte in json.dumps(
            document, separators=(",", ":"), ensure_ascii=False
        ).encode("utf-8")
    )


def read_count(path: Path) -> int:
    try:
        return int(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        return 0


def process_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def wait_process_dead(pid: int, timeout: float = 2.0) -> bool:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if not process_alive(pid):
            return True
        time.sleep(0.02)
    return not process_alive(pid)


def write_fake_emacsclient(path: Path) -> None:
    source = r"""#!__PYTHON__
import base64
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys


def bump(name):
    path = Path(os.environ[name])
    try:
        value = int(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        value = 0
    value += 1
    path.write_text(str(value), encoding="utf-8")
    return value


expression = sys.argv[-1]
if os.environ.get("FAKE_EXPRESSION"):
    Path(os.environ["FAKE_EXPRESSION"]).write_text(expression, encoding="utf-8")
ready_prefix = (
    "(if (and (fboundp 'anvil-headless--ready-p) "
    '(anvil-headless--ready-p "anvil")) '
)
sentinel_suffix = '"anvil-mcp-headless-not-ready")'


def guarded_true_branch():
    if not expression.startswith(ready_prefix):
        return None
    tail = expression[len(ready_prefix) :]
    if tail.endswith(sentinel_suffix):
        branch = tail[: -len(sentinel_suffix)]
        return branch[:-1] if branch.endswith(" ") else None

    cleanup_marker = (
        " (progn (let ((delete-by-moving-to-trash nil)) "
        "(delete-file "
    )
    cleanup_start = tail.rfind(cleanup_marker)
    if cleanup_start < 0 or not tail.endswith("))"):
        return None
    cleanup_branch = tail[cleanup_start + 1 : -1]
    cleanup_pattern = (
        r"\(progn \(let \(\(delete-by-moving-to-trash nil\)\) "
        r"\(delete-file \(decode-coding-string "
        r"\(base64-decode-string \"[A-Za-z0-9+/=]+\"\) "
        r"'utf-8 t\)\)\) "
        r"\"anvil-mcp-headless-not-ready\"\)"
    )
    if re.fullmatch(cleanup_pattern, cleanup_branch) is None:
        return None
    return tail[:cleanup_start]


def execute_file_expression(*, ready=True):
    real_emacs = os.environ["FAKE_REAL_EMACS"]
    response_payload = base64.b64encode(
        os.environ["FAKE_RESPONSE_JSON"].encode("utf-8")
    ).decode("ascii")
    delay = float(os.environ.get("FAKE_REAL_EMACS_DELAY_SEC", "0"))
    program = (
        "(progn "
        + (f"(sleep-for {delay!r}) " if delay else "")
        + f"(fset 'anvil-headless--ready-p (lambda (_server) {'t' if ready else 'nil'})) "
        "(fset 'anvil-server-process-jsonrpc "
        "(lambda (_request _server) "
        f"(decode-coding-string (base64-decode-string "
        f"{json.dumps(response_payload)}) 'utf-8 t))) "
        f"(prin1 {expression}))"
    )
    root = Path(os.environ["FAKE_ANVIL_ROOT"])
    completed = subprocess.run(
        [
            real_emacs,
            "--batch",
            "-Q",
            "-L",
            str(root),
            "-l",
            str(root / "anvil-server.el"),
            "--eval",
            program,
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        sys.stderr.write(completed.stderr)
        raise SystemExit(completed.returncode)
    sys.stdout.write(completed.stdout)


def spawn_stubborn_descendant():
    descendant = subprocess.Popen(
        [
            sys.executable,
            "-I",
            "-S",
            "-c",
            (
                "import signal; "
                "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                "signal.signal(signal.SIGHUP, signal.SIG_IGN); "
                "signal.signal(signal.SIGINT, signal.SIG_IGN); "
                "signal.signal(signal.SIGUSR1, signal.SIG_IGN); "
                "signal.pause()"
            ),
        ],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    Path(os.environ["FAKE_DESCENDANT_PID"]).write_text(
        str(descendant.pid), encoding="utf-8"
    )


def atomically_guards(body):
    branch = guarded_true_branch()
    if branch is None:
        return False
    if body != "anvil-server-process-jsonrpc":
        return branch == f'(progn {body} "anvil-mcp-lifecycle-complete")'
    staged_call = "(anvil-server-process-jsonrpc-to-file "
    return (
        branch.count(staged_call) == 1
        and (
            branch.startswith(staged_call)
            or (
                branch.startswith("(let* ")
                and "(anvil-server-process-jsonrpc-to-file anvil-request " in branch
            )
        )
    )


if "anvil-server-process-jsonrpc" in expression:
    if os.environ.get("FAKE_ATOMIC_NOT_READY") == "1":
        if not atomically_guards("anvil-server-process-jsonrpc"):
            bump("FAKE_DISPATCH_COUNT")
            Path(os.environ["FAKE_EARLY_DISPATCH"]).write_text(
                "atomic wrapper missing\n", encoding="utf-8"
            )
            print(f'"{os.environ["FAKE_RESPONSE_WIRE"]}"')
        else:
            Path(os.environ["FAKE_GUARD_OBSERVED"]).write_text(
                "jsonrpc\n", encoding="utf-8"
            )
            execute_file_expression(ready=False)
        raise SystemExit(0)
    probes = int(Path(os.environ["FAKE_PROBE_COUNT"]).read_text())
    if probes <= int(os.environ.get("FAKE_NIL_BEFORE", "0")):
        Path(os.environ["FAKE_EARLY_DISPATCH"]).write_text(
            "dispatch before exact readiness\n", encoding="utf-8"
        )
        raise SystemExit(70)
    bump("FAKE_DISPATCH_COUNT")
    if os.environ.get("FAKE_HANG_DISPATCH") == "1":
        if os.environ.get("FAKE_TERM_COUNT"):
            signal.signal(
                signal.SIGTERM,
                lambda _signum, _frame: bump("FAKE_TERM_COUNT"),
            )
        elif os.environ.get("FAKE_IGNORE_TERM") == "1":
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
        if os.environ.get("FAKE_DESCENDANT_PID"):
            spawn_stubborn_descendant()
        Path(os.environ["FAKE_HANG_PID"]).write_text(
            str(os.getpid()), encoding="utf-8"
        )
        if os.environ.get("FAKE_RUNNER_PID"):
            Path(os.environ["FAKE_RUNNER_PID"]).write_text(
                str(os.getppid()), encoding="utf-8"
            )
        while True:
            signal.pause()
    if os.environ.get("FAKE_EXIT_DESCENDANT") == "1":
        spawn_stubborn_descendant()
    if os.environ.get("FAKE_DISPATCH_ERROR") == "1":
        raise SystemExit(70)
    if os.environ.get("FAKE_MALFORMED_OUTPUT") == "1":
        print('"')
    else:
        execute_file_expression()
elif "(test-init)" in expression or "(test-stop)" in expression:
    kind = "init" if "(test-init)" in expression else "stop"
    count_name = "FAKE_INIT_COUNT" if kind == "init" else "FAKE_STOP_COUNT"
    not_ready = os.environ.get(f"FAKE_{kind.upper()}_NOT_READY") == "1"
    malformed = os.environ.get(f"FAKE_{kind.upper()}_MALFORMED") == "1"
    split = os.environ.get(f"FAKE_{kind.upper()}_SPLIT_SENTINEL") == "1"
    guarded = atomically_guards(f"(test-{kind})")
    if not_ready:
        if not guarded:
            bump(count_name)
            print('"anvil-mcp-lifecycle-complete"')
        else:
            Path(os.environ["FAKE_GUARD_OBSERVED"]).write_text(
                f"{kind}\n", encoding="utf-8"
            )
            execute_file_expression(ready=False)
    elif malformed or split:
        if guarded:
            Path(os.environ["FAKE_GUARD_OBSERVED"]).write_text(
                f"{kind}\n", encoding="utf-8"
            )
        bump(count_name)
        if split:
            sys.stdout.write('"anvil-mcp-lifecycle-\ncomplete"\n')
        else:
            print('"')
    else:
        bump(count_name)
        print('"anvil-mcp-lifecycle-complete"')
elif "anvil-headless--ready-p" in expression:
    attempt = bump("FAKE_PROBE_COUNT")
    if os.environ.get("FAKE_INVALID_OUTPUT") == "1":
        output = "t-garbage"
    elif (
        os.environ.get("FAKE_ALWAYS_NIL") == "1"
        or attempt <= int(os.environ.get("FAKE_NIL_BEFORE", "0"))
    ):
        output = "nil"
    else:
        output = "t"
    ending = "\r\n" if os.environ.get("FAKE_CRLF") == "1" else "\n"
    sys.stdout.write(output + ending)
else:
    print(f"unexpected expression: {expression}", file=sys.stderr)
    raise SystemExit(64)
""".replace("__PYTHON__", sys.executable)
    make_executable(path, source)


def strict_equal(actual: object, expected: object) -> bool:
    if type(actual) is not type(expected):
        return False
    if isinstance(expected, dict):
        return actual.keys() == expected.keys() and all(
            strict_equal(actual[key], value) for key, value in expected.items()
        )
    if isinstance(expected, list):
        return len(actual) == len(expected) and all(
            strict_equal(left, right) for left, right in zip(actual, expected)
        )
    return actual == expected


def run_bridge_while_open(
    command: list[str],
    request: str,
    environment: dict[str, str],
    root: Path,
    *,
    expect_exit_after_reply: bool = False,
) -> tuple[str, str]:
    process = subprocess.Popen(
        command,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=environment,
        text=True,
        bufsize=1,
    )
    try:
        if process.stdin is None or process.stdout is None or process.stderr is None:
            raise AssertionError("bridge pipes were not created")
        process.stdin.write(request)
        process.stdin.flush()

        executor = ThreadPoolExecutor(max_workers=1)
        reply_future = executor.submit(process.stdout.readline)
        try:
            try:
                first_line = reply_future.result(timeout=15)
            except FutureTimeoutError as error:
                process.kill()
                raise AssertionError("bridge did not return a bounded reply") from error
        finally:
            executor.shutdown(wait=True)

        if not first_line:
            raise AssertionError(
                f"bridge exited before replying with rc={process.poll()}"
            )
        if not expect_exit_after_reply and process.poll() is not None:
            raise AssertionError(
                f"bridge did not remain available after one request: "
                f"rc={process.returncode}"
            )
        if expect_exit_after_reply and process.poll() is None:
            try:
                process.wait(timeout=15)
            except subprocess.TimeoutExpired as error:
                process.kill()
                raise AssertionError(
                    "ambiguous dispatch did not close the bridge"
                ) from error
        transaction_paths = list(root.glob("anvil-mcp.*"))
        staged = {
            path.name: (
                sorted(child.name for child in path.iterdir())
                if path.is_dir()
                else ["<not-a-directory>"]
            )
            for path in transaction_paths
        }
        transaction_ok = len(transaction_paths) <= 1
        if len(transaction_paths) == 1:
            directory = transaction_paths[0]
            children = list(directory.iterdir()) if directory.is_dir() else []

            def generation(child: Path, prefix: str) -> str | None:
                match = re.fullmatch(
                    rf"{re.escape(prefix)}\.([0-9]+)\.json",
                    child.name,
                )
                return match.group(1) if match is not None else None

            def stage_generation(child: Path) -> str | None:
                match = re.fullmatch(
                    r"\.response-tmp\.([0-9]+)\..+",
                    child.name,
                )
                return match.group(1) if match is not None else None

            responses = [
                child for child in children if generation(child, "response") is not None
            ]
            proofs = [
                child for child in children if generation(child, "proof") is not None
            ]
            requests = [
                child for child in children if generation(child, "request") is not None
            ]
            stages = [
                child for child in children if stage_generation(child) is not None
            ]
            directory_info = directory.stat()
            transaction_ok = (
                directory.is_dir()
                and directory_info.st_uid == os.geteuid()
                and stat.S_IMODE(directory_info.st_mode) == 0o700
                and len(requests) <= 1
            )
            unpublished = (
                len(stages) == 1
                and not responses
                and not proofs
                and len(children) == len(stages) + len(requests)
            )
            published = (
                not stages
                and len(responses) == 1
                and len(proofs) == 1
                and len(children) == len(responses) + len(proofs) + len(requests)
            )
            state_generation = (
                stage_generation(stages[0])
                if unpublished
                else generation(responses[0], "response") if published else None
            )
            if published:
                response_info = responses[0].lstat()
                proof_info = proofs[0].lstat()
                published = (
                    generation(proofs[0], "proof") == state_generation
                    and response_info.st_dev == proof_info.st_dev
                    and response_info.st_ino == proof_info.st_ino
                    and response_info.st_nlink == 2
                    and proof_info.st_nlink == 2
                )
            transaction_ok = transaction_ok and (unpublished or published)
            if requests:
                transaction_ok = (
                    transaction_ok
                    and generation(requests[0], "request") == state_generation
                )
            for child in children:
                info = child.lstat()
                child_ok = (
                    child.is_file()
                    and not child.is_symlink()
                    and info.st_uid == os.geteuid()
                    and stat.S_IMODE(info.st_mode) == 0o600
                )
                if generation(child, "request") is not None:
                    child_ok = child_ok and info.st_nlink == 1 and info.st_size <= 16_777_216
                elif stage_generation(child) is not None:
                    child_ok = child_ok and info.st_nlink == 1 and info.st_size == 0
                else:
                    child_ok = child_ok and info.st_nlink == 2 and info.st_size <= 33_554_432
                transaction_ok = transaction_ok and child_ok
        if transaction_paths and not transaction_ok:
            debug_log = root / "debug.log"
            debug = (
                debug_log.read_text(encoding="utf-8")
                if debug_log.exists()
                else "<no debug log>"
            )
            expression_file = root / "expression"
            expression = (
                expression_file.read_text(encoding="utf-8")
                if expression_file.exists()
                else "<no expression>"
            )
            guard_file = root / "guard-observed"
            early_file = root / "early-dispatch"
            guard = (
                guard_file.read_text(encoding="utf-8") if guard_file.exists() else ""
            )
            early = (
                early_file.read_text(encoding="utf-8") if early_file.exists() else ""
            )
            raise AssertionError(
                "bridge retained unsafe generation custody after replying: "
                f"{staged!r}; response={first_line!r}; "
                f"guard={guard!r}; early={early!r}; "
                f"expression-head={expression[:240]!r}; "
                f"expression-tail={expression[-240:]!r}; debug={debug!r}"
            )

        try:
            process.stdin.close()
        except BrokenPipeError:
            pass
        process.stdin = None
        try:
            process.wait(timeout=8)
        except subprocess.TimeoutExpired as error:
            process.kill()
            raise AssertionError("bridge did not exit after stdin EOF") from error
        remainder = process.stdout.read()
        stderr = process.stderr.read()
        stdout = first_line + remainder
        expected_returncode = 74 if expect_exit_after_reply else 0
        if process.returncode != expected_returncode:
            raise AssertionError(
                f"bridge exited {process.returncode}, expected "
                f"{expected_returncode}: stdout={stdout!r} stderr={stderr!r}"
            )
        remaining_transactions = list(root.glob("anvil-mcp.*"))
        if remaining_transactions:
            raise AssertionError(
                "bridge exit retained response transaction custody: "
                f"{[path.name for path in remaining_transactions]!r}"
            )
        return stdout, stderr
    finally:
        if process.poll() is None:
            process.kill()
            process.wait(timeout=2)


def run_case(
    stdio: Path,
    bash: str,
    *,
    nil_before: int = 0,
    always_nil: bool = False,
    atomic_not_ready: bool = False,
    dispatch_error: bool = False,
    malformed_output: bool = False,
    invalid_output: bool = False,
    crlf: bool = False,
    readiness_timeout: int = 5,
    probe_timeout: int = 5,
    retry_delay_ms: int = 50,
    retry_max: int = 5,
    dispatch_timeout: int = 10,
    real_emacs_delay_sec: float = 0,
    large_request: bool = False,
    exit_descendant: bool = False,
) -> tuple[dict[str, object], int, int, float, str, str]:
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-readiness-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        fake = binary / "emacsclient"
        write_fake_emacsclient(fake)
        probe_count = root / "probe-count"
        dispatch_count = root / "dispatch-count"
        early_dispatch = root / "early-dispatch"
        guard_observed = root / "guard-observed"
        expected = {"jsonrpc": "2.0", "id": 17, "result": {"ready": True}}
        request: dict[str, object] = {
            "jsonrpc": "2.0",
            "id": 17,
            "method": "tools/call",
        }
        if large_request:
            request["params"] = {"payload": "x" * 20000}
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "FAKE_PROBE_COUNT": str(probe_count),
                "FAKE_DISPATCH_COUNT": str(dispatch_count),
                "FAKE_EARLY_DISPATCH": str(early_dispatch),
                "FAKE_GUARD_OBSERVED": str(guard_observed),
                "FAKE_EXPRESSION": str(root / "expression"),
                "FAKE_INIT_COUNT": str(root / "init-count"),
                "FAKE_STOP_COUNT": str(root / "stop-count"),
                "FAKE_RESPONSE_WIRE": percent_wire(expected),
                "FAKE_RESPONSE_JSON": json.dumps(
                    expected, separators=(",", ":"), ensure_ascii=False
                ),
                "FAKE_REAL_EMACS": os.environ["ANVIL_TEST_REAL_EMACS"],
                "FAKE_REAL_EMACS_DELAY_SEC": str(real_emacs_delay_sec),
                "FAKE_ANVIL_ROOT": str(stdio.parent),
                "FAKE_NIL_BEFORE": str(nil_before),
                "FAKE_ALWAYS_NIL": "1" if always_nil else "0",
                "FAKE_ATOMIC_NOT_READY": "1" if atomic_not_ready else "0",
                "FAKE_DISPATCH_ERROR": "1" if dispatch_error else "0",
                "FAKE_MALFORMED_OUTPUT": "1" if malformed_output else "0",
                "FAKE_HANG_DISPATCH": "0",
                "FAKE_IGNORE_TERM": "0",
                "FAKE_HANG_PID": str(root / "hang-pid"),
                "FAKE_DESCENDANT_PID": str(root / "descendant-pid"),
                "FAKE_EXIT_DESCENDANT": "1" if exit_descendant else "0",
                "FAKE_INVALID_OUTPUT": "1" if invalid_output else "0",
                "FAKE_CRLF": "1" if crlf else "0",
                "ANVIL_MCP_READINESS_MODE": "headless",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": str(probe_timeout),
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": str(readiness_timeout),
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": str(dispatch_timeout),
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": str(retry_max),
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": str(retry_delay_ms),
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "10",
            }
        )
        started = time.monotonic()
        stdout, stderr = run_bridge_while_open(
            [
                bash,
                str(stdio),
                "--socket=/tmp/anvil-readiness-test",
                "--server-id=anvil",
            ],
            json.dumps(request, separators=(",", ":")) + "\n",
            environment,
            root,
        )
        if exit_descendant:
            descendant_path = root / "descendant-pid"
            if not descendant_path.exists():
                raise AssertionError(
                    "successful dispatch did not spawn its stubborn descendant"
                )
            descendant_pid = int(descendant_path.read_text(encoding="utf-8"))
            if not wait_process_dead(descendant_pid):
                os.kill(descendant_pid, signal.SIGKILL)
                wait_process_dead(descendant_pid)
                raise AssertionError(
                    "successful direct child left a same-group descendant alive"
                )
        elapsed = time.monotonic() - started
        lines = [line for line in stdout.splitlines() if line]
        if len(lines) != 1:
            raise AssertionError(f"bridge returned {len(lines)} replies: {stdout!r}")
        response = json.loads(lines[0])
        if not isinstance(response, dict):
            raise AssertionError(f"bridge returned non-object: {response!r}")
        if early_dispatch.exists():
            raise AssertionError(early_dispatch.read_text(encoding="utf-8"))
        guard = (
            guard_observed.read_text(encoding="utf-8")
            if guard_observed.exists()
            else ""
        )
        debug_log = root / "debug.log"
        debug = debug_log.read_text(encoding="utf-8") if debug_log.exists() else ""
        diagnostics = stderr + ("\n" + debug if debug else "")
        return (
            response,
            read_count(probe_count),
            read_count(dispatch_count),
            elapsed,
            diagnostics,
            guard,
        )


def assert_readiness_error(response: dict[str, object], rc: int = 75) -> None:
    error = response.get("error")
    data = error.get("data") if isinstance(error, dict) else None
    expected = {
        "phase": "readiness",
        "dispatched": False,
        "replayed": False,
        "emacsclientRc": rc,
    }
    if response.get("id") != 17 or not strict_equal(data, expected):
        raise AssertionError(f"wrong readiness failure: {response!r}")


def assert_dispatch_error(response: dict[str, object], rc: int = 70) -> None:
    error = response.get("error")
    data = error.get("data") if isinstance(error, dict) else None
    expected = {
        "phase": "dispatch",
        "dispatched": True,
        "replayed": False,
        "emacsclientRc": rc,
    }
    if response.get("id") != 17 or not strict_equal(data, expected):
        raise AssertionError(f"wrong dispatch failure: {response!r}")


def assert_lifecycle_guard(
    stdio: Path,
    bash: str,
    kind: str,
    *,
    malformed: bool = False,
    split_sentinel: bool = False,
) -> None:
    with tempfile.TemporaryDirectory(prefix=f"anvil-stdio-{kind}-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        fake = binary / "emacsclient"
        write_fake_emacsclient(fake)
        probe_count = root / "probe-count"
        init_count = root / "init-count"
        stop_count = root / "stop-count"
        guard_observed = root / "guard-observed"
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "FAKE_PROBE_COUNT": str(probe_count),
                "FAKE_DISPATCH_COUNT": str(root / "dispatch-count"),
                "FAKE_EARLY_DISPATCH": str(root / "early-dispatch"),
                "FAKE_GUARD_OBSERVED": str(guard_observed),
                "FAKE_INIT_COUNT": str(init_count),
                "FAKE_STOP_COUNT": str(stop_count),
                f"FAKE_{kind.upper()}_NOT_READY": (
                    "0" if malformed or split_sentinel else "1"
                ),
                f"FAKE_{kind.upper()}_MALFORMED": "1" if malformed else "0",
                f"FAKE_{kind.upper()}_SPLIT_SENTINEL": ("1" if split_sentinel else "0"),
                "ANVIL_MCP_READINESS_MODE": "headless",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "2",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        completed = subprocess.run(
            [
                bash,
                str(stdio),
                f"--{kind}-function=test-{kind}",
                "--socket=/tmp/anvil-readiness-test",
                "--server-id=anvil",
            ],
            input="",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            timeout=8,
            check=False,
        )
        count = read_count(init_count if kind == "init" else stop_count)
        guard = (
            guard_observed.read_text(encoding="utf-8")
            if guard_observed.exists()
            else ""
        )
        debug_log = root / "debug.log"
        debug = debug_log.read_text(encoding="utf-8") if debug_log.exists() else ""
        expected_count = 1 if malformed or split_sentinel else 0
        expected_debug = f"MCP-{kind.upper()}-RC: 70"
        if (
            completed.returncode != 0
            or read_count(probe_count) != 1
            or count != expected_count
            or guard != f"{kind}\n"
            or "substring expression" in completed.stderr
            or ((malformed or split_sentinel) and expected_debug not in debug)
        ):
            if split_sentinel:
                mode = "split-sentinel"
            elif malformed:
                mode = "malformed"
            else:
                mode = "not-ready"
            raise AssertionError(
                f"{kind} {mode} lifecycle guard failed: "
                f"rc={completed.returncode} probes={read_count(probe_count)} "
                f"calls={count} guard={guard!r} stdout={completed.stdout!r} "
                f"stderr={completed.stderr!r} debug={debug!r}"
            )


def assert_signal_cleanup(
    stdio: Path,
    bash: str,
    *,
    repeat: bool,
) -> None:
    if os.name != "posix":
        return

    label = "repeat" if repeat else "single"
    with tempfile.TemporaryDirectory(prefix=f"anvil-stdio-signal-{label}-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        fake = binary / "emacsclient"
        write_fake_emacsclient(fake)
        hang_pid_path = root / "hang-pid"
        descendant_pid_path = root / "descendant-pid"
        runner_pid_path = root / "runner-pid"
        term_count_path = root / "term-count"
        request = {
            "jsonrpc": "2.0",
            "id": 29,
            "method": "tools/call",
            "params": {"payload": "x" * 20000},
        }
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "FAKE_PROBE_COUNT": str(root / "probe-count"),
                "FAKE_DISPATCH_COUNT": str(root / "dispatch-count"),
                "FAKE_EARLY_DISPATCH": str(root / "early-dispatch"),
                "FAKE_GUARD_OBSERVED": str(root / "guard-observed"),
                "FAKE_EXPRESSION": str(root / "expression"),
                "FAKE_INIT_COUNT": str(root / "init-count"),
                "FAKE_STOP_COUNT": str(root / "stop-count"),
                "FAKE_RESPONSE_WIRE": percent_wire(
                    {"jsonrpc": "2.0", "id": 29, "result": True}
                ),
                "FAKE_NIL_BEFORE": "0",
                "FAKE_ALWAYS_NIL": "0",
                "FAKE_ATOMIC_NOT_READY": "0",
                "FAKE_DISPATCH_ERROR": "0",
                "FAKE_MALFORMED_OUTPUT": "0",
                "FAKE_HANG_DISPATCH": "1",
                "FAKE_IGNORE_TERM": "0",
                "FAKE_TERM_COUNT": str(term_count_path),
                "FAKE_HANG_PID": str(hang_pid_path),
                "FAKE_RUNNER_PID": str(runner_pid_path),
                "FAKE_DESCENDANT_PID": str(descendant_pid_path),
                "FAKE_EXIT_DESCENDANT": "0",
                "FAKE_INVALID_OUTPUT": "0",
                "FAKE_CRLF": "0",
                "ANVIL_MCP_READINESS_MODE": "headless",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "2",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        process = subprocess.Popen(
            [
                bash,
                str(stdio),
                "--socket=/tmp/anvil-readiness-test",
                "--server-id=anvil",
            ],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            start_new_session=True,
        )
        fake_pid: int | None = None
        runner_pid: int | None = None
        descendant_pid: int | None = None
        try:
            if (
                process.stdin is None
                or process.stdout is None
                or process.stderr is None
            ):
                raise AssertionError("signal regression pipes were not created")
            process.stdin.write(json.dumps(request, separators=(",", ":")) + "\n")
            process.stdin.flush()

            deadline = time.monotonic() + 5
            while time.monotonic() < deadline:
                if process.poll() is not None:
                    break
                if (
                    hang_pid_path.exists()
                    and runner_pid_path.exists()
                    and descendant_pid_path.exists()
                    and list(root.glob("anvil-mcp.*"))
                ):
                    fake_pid = int(hang_pid_path.read_text(encoding="utf-8"))
                    runner_pid = int(
                        runner_pid_path.read_text(encoding="utf-8")
                    )
                    descendant_pid = int(
                        descendant_pid_path.read_text(encoding="utf-8")
                    )
                    break
                time.sleep(0.02)
            if (
                fake_pid is None
                or runner_pid is None
                or descendant_pid is None
            ):
                raise AssertionError(
                    "large request did not reach its staged hanging dispatch "
                    "with a same-group descendant"
                )

            if repeat:
                os.kill(runner_pid, signal.SIGTERM)
                term_deadline = time.monotonic() + 1
                while (
                    read_count(term_count_path) != 1
                    and time.monotonic() < term_deadline
                ):
                    time.sleep(0.01)
                if read_count(term_count_path) != 1:
                    raise AssertionError(
                        "direct runner TERM did not reach its exact child"
                    )
                time.sleep(0.1)
                if process.poll() is not None or not process_alive(runner_pid):
                    raise AssertionError(
                        "runner exited before bridge termination was exercised"
                    )
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired as error:
                raise AssertionError(
                    "direct bridge SIGTERM did not produce a bounded exit"
                ) from error

            stdout = process.stdout.read()
            stderr = process.stderr.read()
            deadline = time.monotonic() + 2
            child_alive = True
            runner_alive = True
            descendant_alive = True
            group_alive = True
            staged = list(root.glob("anvil-mcp.*"))
            while time.monotonic() < deadline:
                try:
                    os.kill(fake_pid, 0)
                except ProcessLookupError:
                    child_alive = False
                except PermissionError:
                    child_alive = True
                else:
                    child_alive = True
                runner_alive = process_alive(runner_pid)
                descendant_alive = process_alive(descendant_pid)
                try:
                    os.killpg(process.pid, 0)
                except ProcessLookupError:
                    group_alive = False
                except PermissionError:
                    group_alive = True
                else:
                    group_alive = True
                staged = list(root.glob("anvil-mcp.*"))
                if (
                    not child_alive
                    and not runner_alive
                    and not descendant_alive
                    and not group_alive
                    and not staged
                ):
                    break
                time.sleep(0.02)
            if (
                process.returncode != 143
                or read_count(term_count_path) != 1
                or child_alive
                or runner_alive
                or descendant_alive
                or group_alive
                or staged
                or stdout
                or "substring expression" in stderr
            ):
                raise AssertionError(
                    "direct bridge SIGTERM failed custody: "
                    f"rc={process.returncode} terms={read_count(term_count_path)} "
                    f"child_alive={child_alive} runner_alive={runner_alive} "
                    f"descendant_alive={descendant_alive} group_alive={group_alive} "
                    f"staged={[path.name for path in staged]!r} "
                    f"stdout={stdout!r} stderr={stderr!r}"
                )
        finally:
            if process.stdin is not None:
                try:
                    process.stdin.close()
                except BrokenPipeError:
                    pass
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=2)
            else:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if fake_pid is not None:
                try:
                    os.kill(fake_pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if runner_pid is not None:
                try:
                    os.kill(runner_pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if descendant_pid is not None:
                try:
                    os.kill(descendant_pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass


def assert_signal_publication(stdio: Path, bash: str) -> None:
    if os.name != "posix":
        return

    with tempfile.TemporaryDirectory(prefix="anvil-stdio-publication-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        make_executable(
            binary / "emacsclient",
            f"#!{bash}\nprintf 't\\n'\n",
        )
        real_python = sys.executable
        python_wrapper = f"""#!{real_python}
import os
import signal
import sys

once = os.environ["FAKE_PUBLICATION_ONCE"]
ready = os.environ["FAKE_PUBLICATION_READY"]
try:
    descriptor = os.open(once, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
except FileExistsError:
    os.execv({real_python!r}, [{real_python!r}, *sys.argv[1:]])
else:
    os.close(descriptor)
    with open(ready, "w", encoding="utf-8") as stream:
        stream.write(str(os.getpid()))
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    while True:
        signal.pause()
"""
        make_executable(binary / "python3", python_wrapper)

        bridge = root / "anvil-stdio-publication.sh"
        source = stdio.read_text(encoding="utf-8")
        needle = "\trunner=$!\n\tANVIL_MCP_ACTIVE_RUNNER=$runner\n"
        injected = (
            '\tif [ -n "${ANVIL_TEST_SIGNAL_PUBLICATION:-}" ]; then\n'
            "\t\tunset ANVIL_TEST_SIGNAL_PUBLICATION\n"
            '\t\twhile [ ! -s "$FAKE_PUBLICATION_READY" ]; do :; done\n'
            '\t\tkill -TERM "$$"\n'
            "\tfi\n" + needle
        )
        if source.count(needle) != 1:
            raise AssertionError("runner publication injection point drifted")
        make_executable(bridge, source.replace(needle, injected, 1))

        ready_path = root / "publication-ready"
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "EMACS_MCP_DEBUG_LOG": str(root / "debug.log"),
                "ANVIL_TEST_SIGNAL_PUBLICATION": "1",
                "FAKE_PUBLICATION_ONCE": str(root / "publication-once"),
                "FAKE_PUBLICATION_READY": str(ready_path),
                "ANVIL_MCP_READINESS_MODE": "emacs",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "1",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        process = subprocess.Popen(
            [bash, str(bridge), "--server-id=anvil"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            start_new_session=True,
        )
        helper_pid: int | None = None
        try:
            if process.stdin is None:
                raise AssertionError("publication regression stdin is unavailable")
            process.stdin.write('{"jsonrpc":"2.0","id":41,"method":"tools/call"}\n')
            process.stdin.flush()

            deadline = time.monotonic() + 5
            while time.monotonic() < deadline:
                if ready_path.exists():
                    helper_pid = int(ready_path.read_text(encoding="utf-8"))
                    break
                if process.poll() is not None:
                    break
                time.sleep(0.02)
            if helper_pid is None:
                raise AssertionError(
                    "stubborn prepublication helper was never observed"
                )
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired as error:
                raise AssertionError(
                    "signal during runner publication did not converge"
                ) from error

            deadline = time.monotonic() + 2
            helper_alive = True
            group_alive = True
            while time.monotonic() < deadline:
                try:
                    os.kill(helper_pid, 0)
                except ProcessLookupError:
                    helper_alive = False
                except PermissionError:
                    helper_alive = True
                else:
                    helper_alive = True
                try:
                    os.killpg(process.pid, 0)
                except ProcessLookupError:
                    group_alive = False
                except PermissionError:
                    group_alive = True
                else:
                    group_alive = True
                if not helper_alive and not group_alive:
                    break
                time.sleep(0.02)
            if process.returncode != 143 or helper_alive or group_alive:
                raise AssertionError(
                    "runner publication signal lost custody: "
                    f"rc={process.returncode} helper_alive={helper_alive} "
                    f"group_alive={group_alive}"
                )
        finally:
            if process.stdin is not None:
                try:
                    process.stdin.close()
                except BrokenPipeError:
                    pass
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            if helper_pid is not None:
                try:
                    os.kill(helper_pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if process.poll() is None:
                process.wait(timeout=2)


def assert_exit_without_staging(stdio: Path, bash: str) -> None:
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-exit-") as raw:
        root = Path(raw)
        binary = root / "bin"
        binary.mkdir()
        cleanup_marker = root / "cleanup-helper"
        real_python = sys.executable
        python_wrapper = f"""#!{real_python}
import os
from pathlib import Path
import sys

code = "\\n".join(sys.argv[1:])
if (
    "for name in os.listdir(directory):" in code
    and "os.rmdir(directory)" in code
):
    Path(os.environ["FAKE_CLEANUP_INVOKED"]).write_text(
        "cleanup helper ran\\n", encoding="utf-8"
    )
os.execv({real_python!r}, [{real_python!r}, *sys.argv[1:]])
"""
        make_executable(binary / "python3", python_wrapper)
        make_executable(
            binary / "emacsclient",
            f"#!{bash}\nprintf 't\\n'\n",
        )
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "TMPDIR": str(root),
                "FAKE_CLEANUP_INVOKED": str(cleanup_marker),
                "ANVIL_MCP_READINESS_MODE": "emacs",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "1",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
            }
        )
        completed = subprocess.run(
            [bash, str(stdio), "--server-id=anvil"],
            input="",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            timeout=5,
            check=False,
        )
        staged = list(root.glob("anvil-mcp.*"))
        if completed.returncode != 0 or cleanup_marker.exists() or staged:
            raise AssertionError(
                "stage-free bridge exit launched cleanup work: "
                f"rc={completed.returncode} marker={cleanup_marker.exists()} "
                f"staged={[path.name for path in staged]!r} "
                f"stdout={completed.stdout!r} stderr={completed.stderr!r}"
            )


def assert_invalid_configuration(
    stdio: Path,
    bash: str,
    name: str,
    value: str,
    *,
    expected_fragment: str | None = None,
    server_id: str = "anvil",
) -> None:
    with tempfile.TemporaryDirectory(prefix="anvil-stdio-config-") as raw:
        binary = Path(raw) / "bin"
        binary.mkdir()
        make_executable(binary / "emacsclient", f"#!{bash}\nexit 0\n")
        environment = os.environ.copy()
        environment.pop("ANVIL_MCP_PARENT_GUARD", None)
        environment.pop("ANVIL_MCP_PARENT_GUARD_PYTHON", None)
        environment.update(
            {
                "PATH": f"{binary}{os.pathsep}{environment['PATH']}",
                "ANVIL_MCP_READINESS_MODE": "emacs",
                "ANVIL_EMACSCLIENT_PROBE_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_READINESS_TIMEOUT": "5",
                "ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT": "10",
                "ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT": "1",
                "ANVIL_EMACSCLIENT_RETRY_MAX": "2",
                "ANVIL_EMACSCLIENT_RETRY_DELAY_MS": "0",
                "ANVIL_MCP_REQUEST_PARSE_TIMEOUT": "2",
                "ANVIL_MCP_FRAME_READ_TIMEOUT": "2",
                name: value,
            }
        )
        completed = subprocess.run(
            [bash, str(stdio), f"--server-id={server_id}"],
            input="",
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=environment,
            text=True,
            timeout=3,
            check=False,
        )
        fragment = expected_fragment or name
        if completed.returncode != 64 or fragment not in completed.stderr:
            raise AssertionError(
                f"invalid {name}={value!r} was not rejected cleanly: "
                f"rc={completed.returncode} stderr={completed.stderr!r}"
            )


def main() -> int:
    if len(sys.argv) != 4:
        raise SystemExit(f"usage: {Path(sys.argv[0]).name} ANVIL_STDIO BASH EMACS")
    stdio = Path(sys.argv[1]).resolve()
    bash = str(Path(sys.argv[2]).resolve())
    real_emacs = str(Path(sys.argv[3]).resolve())
    if not os.access(real_emacs, os.X_OK):
        raise AssertionError(f"real Emacs is not executable: {real_emacs}")
    os.environ["ANVIL_TEST_REAL_EMACS"] = real_emacs
    expected = {"jsonrpc": "2.0", "id": 17, "result": {"ready": True}}

    success, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, nil_before=2
    )
    if not strict_equal(success, expected) or probes != 3 or dispatches != 1:
        raise AssertionError(
            f"nil-to-ready case failed: response={success!r} "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    delayed_success, probes, dispatches, elapsed, stderr, _guard = run_case(
        stdio,
        bash,
        real_emacs_delay_sec=5.1,
        dispatch_timeout=20,
    )
    if (
        not strict_equal(delayed_success, expected)
        or probes != 1
        or dispatches != 1
        or not 5 < elapsed < 20
    ):
        raise AssertionError(
            "real Emacs was preempted by a nested harness timeout: "
            f"response={delayed_success!r} probes={probes} "
            f"dispatches={dispatches} elapsed={elapsed:.3f} "
            f"stderr={stderr!r}"
        )

    large_success, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, large_request=True
    )
    if not strict_equal(large_success, expected) or probes != 1 or dispatches != 1:
        raise AssertionError(
            f"large consumed-marker case failed: response={large_success!r} "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    group_success, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, exit_descendant=True
    )
    if not strict_equal(group_success, expected) or probes != 1 or dispatches != 1:
        raise AssertionError(
            f"same-group descendant cleanup failed: response={group_success!r} "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    crlf, probes, dispatches, _elapsed, _stderr, _guard = run_case(
        stdio, bash, crlf=True, nil_before=2
    )
    if not strict_equal(crlf, expected) or probes != 3 or dispatches != 1:
        raise AssertionError(
            f"CRLF readiness was not accepted: response={crlf!r} "
            f"probes={probes} dispatches={dispatches}"
        )

    exhausted, probes, dispatches, elapsed, diagnostics, _guard = run_case(
        stdio, bash, always_nil=True, readiness_timeout=1, retry_max=1
    )
    assert_readiness_error(exhausted)
    if (
        probes < 1
        or dispatches != 0
        or not 0 < elapsed < 2.5
        or "MCP-PROBE-WAIT-EXHAUSTED" not in diagnostics
    ):
        raise AssertionError(
            "nil deadline was not bounded and fail-closed: "
            f"probes={probes} dispatches={dispatches} elapsed={elapsed:.3f} "
            f"diagnostics={diagnostics!r}"
        )

    delayed, probes, dispatches, elapsed, diagnostics, _guard = run_case(
        stdio,
        bash,
        always_nil=True,
        readiness_timeout=1,
        retry_delay_ms=5000,
        retry_max=1,
    )
    assert_readiness_error(delayed)
    if (
        probes < 1
        or dispatches != 0
        or not 0 < elapsed < 2.5
        or "MCP-PROBE-WAIT-EXHAUSTED" not in diagnostics
    ):
        raise AssertionError(
            "retry delay exceeded the readiness budget: "
            f"probes={probes} dispatches={dispatches} elapsed={elapsed:.3f} "
            f"diagnostics={diagnostics!r}"
        )

    raced, probes, dispatches, _elapsed, _stderr, guard = run_case(
        stdio, bash, atomic_not_ready=True
    )
    assert_readiness_error(raced)
    if probes != 1 or dispatches != 0 or guard != "jsonrpc\n":
        raise AssertionError(
            "atomic JSON-RPC guard was not observed before dispatch: "
            f"probes={probes} dispatches={dispatches} guard={guard!r}"
        )

    large_raced, probes, dispatches, _elapsed, _stderr, guard = run_case(
        stdio, bash, atomic_not_ready=True, large_request=True
    )
    assert_readiness_error(large_raced)
    if probes != 1 or dispatches != 0 or guard != "jsonrpc\n":
        raise AssertionError(
            "large-request atomic guard was not observed: "
            f"probes={probes} dispatches={dispatches} guard={guard!r}"
        )

    failed, probes, dispatches, _elapsed, _stderr, _guard = run_case(
        stdio, bash, dispatch_error=True, large_request=True
    )
    assert_dispatch_error(failed)
    if probes != 1 or dispatches != 1:
        raise AssertionError(
            "large request dispatch failure was not singular: "
            f"probes={probes} dispatches={dispatches}"
        )

    malformed, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, malformed_output=True
    )
    assert_dispatch_error(malformed)
    if probes != 1 or dispatches != 1 or "substring expression" in stderr:
        raise AssertionError(
            "one-character dispatch output did not fail closed: "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    large_malformed, probes, dispatches, _elapsed, stderr, _guard = run_case(
        stdio, bash, malformed_output=True, large_request=True
    )
    assert_dispatch_error(large_malformed)
    if probes != 1 or dispatches != 1 or "substring expression" in stderr:
        raise AssertionError(
            "large malformed dispatch did not cleanly fail: "
            f"probes={probes} dispatches={dispatches} stderr={stderr!r}"
        )

    invalid, probes, dispatches, _elapsed, _stderr, _guard = run_case(
        stdio, bash, invalid_output=True
    )
    assert_readiness_error(invalid)
    if probes != 1 or dispatches != 0:
        raise AssertionError(
            f"invalid probe output was accepted: probes={probes} "
            f"dispatches={dispatches}"
        )

    assert_lifecycle_guard(stdio, bash, "init")
    assert_lifecycle_guard(stdio, bash, "stop")
    assert_lifecycle_guard(stdio, bash, "init", malformed=True)
    assert_lifecycle_guard(stdio, bash, "stop", malformed=True)
    assert_lifecycle_guard(stdio, bash, "init", split_sentinel=True)
    assert_lifecycle_guard(stdio, bash, "stop", split_sentinel=True)
    assert_signal_publication(stdio, bash)
    assert_signal_cleanup(stdio, bash, repeat=False)
    assert_signal_cleanup(stdio, bash, repeat=True)
    assert_exit_without_staging(stdio, bash)
    assert_invalid_configuration(stdio, bash, "ANVIL_EMACSCLIENT_RETRY_MAX", "09")
    assert_invalid_configuration(
        stdio, bash, "ANVIL_EMACSCLIENT_READINESS_TIMEOUT", "09"
    )
    assert_invalid_configuration(stdio, bash, "ANVIL_EMACSCLIENT_RETRY_MAX", "9" * 200)
    assert_invalid_configuration(
        stdio,
        bash,
        "ANVIL_EMACSCLIENT_READINESS_TIMEOUT",
        "9" * 200,
    )
    assert_invalid_configuration(
        stdio,
        bash,
        "ANVIL_MCP_READINESS_MODE",
        "unsupported",
        expected_fragment="unsupported readiness mode",
    )
    assert_invalid_configuration(
        stdio,
        bash,
        "ANVIL_MCP_READINESS_MODE",
        "headless",
        expected_fragment="unsafe server id",
        server_id="unsafe id",
    )

    print(f"stdio-readiness-ok bash={bash}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
