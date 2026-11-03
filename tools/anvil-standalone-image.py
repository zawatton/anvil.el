#!/usr/bin/env python3
"""Validated Anvil heap launcher for POSIX, native Windows, and Wine tests.

Native Windows: python anvil-standalone-image.py --binary C:/.../nelisp.exe
Wine: NELISP_RUNNER=wine NELISP_BIN=/.../nelisp.exe python3 this-file.py
If construction/validation fails, load the source profile instead.
"""
import argparse
import contextlib
import hashlib
import json
import mmap
import os
import queue
from pathlib import Path
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import threading
import time

if os.name == "nt":
    import ctypes
    from ctypes import wintypes

    class _FILE_BASIC_INFO(ctypes.Structure):
        _fields_ = [(name, ctypes.c_longlong) for name in
                    ("CreationTime", "LastAccessTime", "LastWriteTime", "ChangeTime")]
        _fields_.append(("FileAttributes", wintypes.DWORD))

    _CreateFileW = ctypes.WinDLL("kernel32", use_last_error=True).CreateFileW
    _CreateFileW.argtypes = (wintypes.LPCWSTR, wintypes.DWORD, wintypes.DWORD,
                             wintypes.LPVOID, wintypes.DWORD, wintypes.DWORD,
                             wintypes.HANDLE)
    _CreateFileW.restype = wintypes.HANDLE
    _GetFileInformationByHandleEx = ctypes.WinDLL(
        "kernel32", use_last_error=True).GetFileInformationByHandleEx
    _GetFileInformationByHandleEx.argtypes = (wintypes.HANDLE, ctypes.c_int,
                                               wintypes.LPVOID, wintypes.DWORD)
    _GetFileInformationByHandleEx.restype = wintypes.BOOL
    _CloseHandle = ctypes.WinDLL("kernel32", use_last_error=True).CloseHandle
    _CloseHandle.argtypes = (wintypes.HANDLE,)
    _CloseHandle.restype = wintypes.BOOL

VERSION = 3
SCRIPT = Path(__file__).resolve()
PROFILE = SCRIPT.with_name("anvil-standalone-mcp.el")
MARKER = bytes([127]) + b"NELISP-BLDDGST" + bytes([1])


def digest(path):
    with Path(path).open("rb") as stream:
        result = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024*1024), b""):
            result.update(chunk)
        return result.hexdigest()


def key(inputs):
    return hashlib.sha256(json.dumps(inputs, sort_keys=True).encode()).hexdigest()


def build_digest(binary):
    with binary.open("rb") as stream, mmap.mmap(
            stream.fileno(), 0, access=mmap.ACCESS_READ) as data:
        offset = data.find(MARKER)
        if offset < 0 or data.find(MARKER, offset+1) >= 0:
            raise ValueError("runtime has no unique build digest")
        result = data[offset+len(MARKER):offset+len(MARKER)+32]
    if len(result) != 32 or not any(result):
        raise ValueError("runtime build digest is unstamped")
    return result.hex()


def change_time_ns(path, status):
    if os.name != "nt":
        return status.st_ctime_ns
    # Python exposes Windows creation time as st_ctime. NTFS ChangeTime is the
    # metadata clock that detects an in-place write followed by mtime restore.
    handle = _CreateFileW(str(path), 0x80, 0x7, None, 3, 0x02000000, None)
    if handle == wintypes.HANDLE(-1).value:
        return None
    try:
        info = _FILE_BASIC_INFO()
        if not _GetFileInformationByHandleEx(
                handle, 0, ctypes.byref(info), ctypes.sizeof(info)):
            return None
        return info.ChangeTime*100
    finally:
        _CloseHandle(handle)


def file_stamp(path):
    """Return the cheap direct-mode identity for one filesystem object."""
    path = Path(path)
    try:
        status = path.stat()
    except OSError:
        return None
    return dict(path=str(path), size=status.st_size, mtime_ns=status.st_mtime_ns,
                device=status.st_dev, inode=status.st_ino,
                ctime_ns=change_time_ns(path, status))


def direct_stamps_safe(value):
    """Whether every present object has a trustworthy content-change clock."""
    if isinstance(value, dict):
        if "ctime_ns" in value and value["ctime_ns"] is None:
            return False
        return all(direct_stamps_safe(item) for item in value.values())
    if isinstance(value, list):
        return all(direct_stamps_safe(item) for item in value)
    return True


@contextlib.contextmanager
def locked(path):
    # OS-held locks recover automatically when a builder dies.
    with path.open("a+b") as stream:
        if os.name == "nt":
            import msvcrt
            stream.seek(0)
            if not stream.read(1):
                stream.write(b"\0")
                stream.flush()
            stream.seek(0)
            msvcrt.locking(stream.fileno(), msvcrt.LK_LOCK, 1)
        else:
            import fcntl
            fcntl.flock(stream, fcntl.LOCK_EX)
        try:
            yield
        finally:
            if os.name == "nt":
                stream.seek(0)
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)


class Launcher:
    def __init__(self, binary, runner=None):
        self.binary = Path(shutil.which(str(binary)) or binary).resolve()
        self.runner = runner
        self.env = dict(os.environ)
        root = str(PROFILE.parent.parent)
        self.env.setdefault("ANVIL_EL_DIR", root)
        notes = Path.home()/"Cowork/Notes"
        if (notes/"dev/nelisp-system-one/lisp").is_dir():
            self.env.setdefault("ANVIL_NOTES_DIR", str(notes))
        if self.env.get("ANVIL_NOTES_DIR"):
            for kind in ("MEMORY", "WORKLOG"):
                self.env.setdefault("ANVIL_"+kind+"_DB", str(
                    Path(self.env["ANVIL_NOTES_DIR"])/(".anvil-"+kind.lower())/
                    ("anvil-"+kind.lower()+"-index.db")))
        self.env["NELISP_BIN"] = self.native(self.binary)
        self.env["ANVIL_STANDALONE_CWD"] = self.native(Path.cwd())
        self.cache_root = Path(os.environ.get("XDG_CACHE_HOME", str(Path.home()/".cache")))

    def configure(self):
        # DB paths are resolved by their modules after restore, never baked in.
        excluded = {"ANVIL_MEMORY_DB", "ANVIL_WORKLOG_DB", "ANVIL_STANDALONE_DEBUG",
                    "ANVIL_STANDALONE_CWD", "ANVIL_STANDALONE_BUILD_IMAGE",
                    "ANVIL_STANDALONE_TIMINGS"}
        self.config = dict(version=VERSION, build_digest=build_digest(self.binary),
            binary=str(self.binary), launcher=str(SCRIPT), profile=str(PROFILE),
            environment={k:v for k,v in self.env.items() if k not in excluded and (
                k.startswith(("ANVIL_", "NELISP_", "NSO_")) or k in (
                    "HOME", "USERPROFILE", "HOSTNAME", "COMPUTERNAME", "PATH"))},
            runner=self.runner)
        self.cache = self.cache_root/"anvil-standalone"/key(self.config)

    def runtime_identity(self):
        return dict(build_digest=build_digest(self.binary),
            runtime={str(p):digest(p) for p in (self.binary,
                Path(str(self.binary)+".cold"), Path(str(self.binary)+".native-startup.el"))
                if p.is_file()})

    def runtime_stamps(self):
        return {str(p):file_stamp(p) for p in (self.binary,
            Path(str(self.binary)+".cold"), Path(str(self.binary)+".native-startup.el"))
            if p.is_file()}

    def native(self, path):
        path = str(path)
        return "Z:"+path if self.runner and path.startswith("/") else path

    def host(self, path):
        path = path.replace("\\", "/")
        if self.runner and path[:3].lower() == "z:/":
            return str(Path(path[2:]).resolve())
        if self.runner and len(path) > 2 and path[1] == ":":
            prefix = Path(self.env.get("WINEPREFIX", str(Path.home()/".wine")))
            drive = prefix/"dosdevices"/(path[0].lower()+":")
            return str((drive.resolve()/path[3:]).resolve())
        return str(Path(path).resolve())

    def command(self, *args):
        return ([self.runner] if self.runner else [])+[str(self.binary), *args]

    def child_env(self, build=False):
        env = dict(self.env)
        if build:
            env["ANVIL_STANDALONE_BUILD_IMAGE"] = "1"
            for name in ("ANVIL_MEMORY_DB", "ANVIL_WORKLOG_DB", "ANVIL_STANDALONE_DEBUG"):
                env.pop(name, None)
        if self.runner:
            for name in ("ANVIL_EL_DIR", "ANVIL_NOTES_DIR", "NELISP_EMACS_LIB_DIR",
                         "NSO_LISP_DIR", "ANVIL_MEMORY_DB", "ANVIL_WORKLOG_DB",
                         "XDG_CACHE_HOME", "HOME"):
                if env.get(name):
                    env[name] = self.native(env[name])
        return env

    def inventory(self, meta):
        # Hash every loaded source. Also hash potential providers so an .elc,
        # new file, or different provider selected by load-path invalidates.
        sources = {p:digest(p) for p in meta["sources"]}
        directories = {}
        for name in meta["directories"]:
            directory = Path(name)
            directories[name] = ({str(p):digest(p) for p in directory.iterdir()
                if p.suffix in (".el", ".elc") and p.is_file()}
                if directory.is_dir() else None)
        return dict(config=self.config, runtime=self.runtime_identity(),
                    sources=sources, directories=directories)

    def dependency_stamps(self, meta):
        """Describe inputs cheaply; ctime catches restored-mtime edits on Linux.

        A filesystem with coarse or creation-only ctime can still alias a
        same-size edit whose mtime is restored; the SHA layer cannot run when
        every available stamp aliases. Linux stat ctime_ns covers that control.
        """
        sources = {name:file_stamp(name) for name in meta["sources"]}
        directories = {}
        for name in meta["directories"]:
            directory = Path(name)
            directories[name] = (dict(stamp=file_stamp(directory), files={
                str(path):file_stamp(path) for path in directory.iterdir()
                if path.suffix in (".el", ".elc") and path.is_file()})
                if directory.is_dir() else None)
        return dict(runtime=self.runtime_stamps(), sources=sources,
                    directories=directories)

    def stamps(self, meta, image):
        result = self.dependency_stamps(meta)
        result["image"] = file_stamp(image)
        return result

    def write_manifest(self, manifest, meta):
        staged = self.cache/(".manifest-%d.tmp" % os.getpid())
        staged.write_text(json.dumps(meta, sort_keys=True, indent=2)+"\n")
        os.replace(staged, manifest)

    def probe(self, tmp, dump=False):
        history, paths = tmp/"sources.txt", tmp/"paths.txt"
        image = tmp/"heap.flat"
        quote = lambda p:json.dumps(self.native(p))
        initialize = json.dumps(json.dumps(dict(jsonrpc="2.0", id=1,
            method="initialize", params=dict(protocolVersion="2025-06-18",
            capabilities={}, clientInfo=dict(name="fast-start-bench", version="2"))),
            separators=(",", ":")))
        form = ("(let ((anvil-image-base-history (copy-tree load-history)) "
            "(coding-system-for-write 'utf-8)) (load "+quote(PROFILE)+" nil t) "
            '(anvil-server--handle-tools-list 0 "emacs-eval") '
            '(anvil-server-start) '
            '(anvil-server-process-jsonrpc '+initialize+' "emacs-eval") '
            '(let* ((sample (list :rows (list '
            '(list :file "alpha.org" :type \'feedback '
            ':snippet "ASCII 日本語\\nline\\t\\\"quoted\\\" \\\\slash" :rank -0.25) '
            '(list :file "beta.org" :type \'project :snippet nil :rank 2)))) '
            '(anvil-standalone--cache-response t) '
            '(text (anvil-server-encode-for-mcp sample)) '
            '(result (list (cons \'content (vector (list (cons \'type "text") '
            '(cons \'text text)))) (cons \'isError :json-false)))) '
            '(anvil-server--jsonrpc-response 777 result)) '
            '(setq anvil-standalone--encoding-cache nil '
            'anvil-standalone--response-cache nil) '
            '(setq anvil-server--running nil) '
            '(clrhash anvil-server-metrics--table) '
            "(with-temp-file "+quote(history)+" (dolist (x load-history) "
            '(when (and (stringp (car x)) (not (member x anvil-image-base-history))) '
            '(insert (car x) "\\n")))) '
            "(with-temp-file "+quote(paths)+" (dolist (x load-path) "
            '(when (stringp x) (insert x "\\n")))) ')
        if dump:
            form += ("(garbage-collect) (unless (> (nelisp--arena-dump-image-stream "+
                     quote(image)+") 0) (error \"Anvil heap dump failed\")) ")
        form += '(nelisp--write-stderr-line "ANVIL-IMAGE-PREPARED"))'
        run = subprocess.run(self.command("--eval", form), env=self.child_env(True),
            stdin=subprocess.DEVNULL, capture_output=True, timeout=180)
        if run.returncode or b"ANVIL-IMAGE-PREPARED" not in run.stderr:
            raise RuntimeError("heap preparation failed: "+run.stderr.decode(
                errors="replace")[-1600:])
        sources = sorted({self.host(p) for p in history.read_text(encoding="utf-8").splitlines()} |
                         {str(PROFILE), str(SCRIPT)})
        directories = sorted({self.host(p) for p in paths.read_text(encoding="utf-8").splitlines()})
        return dict(sources=sources, directories=directories), image

    def smoke(self, path):
        """Never publish an image that this exact runtime cannot restore."""
        # Tool count: 110 committed tools plus each optional context module that
        # loaded; a lower bound, so tools added by work in progress pass.
        form = ('(unless (and (fboundp \'anvil-standalone-start) '
                '(>= (hash-table-count (gethash "emacs-eval" anvil-server--tools)) '
                '(+ 115 (if (featurep \'anvil-context-batch) 1 0) '
                '(if (featurep \'anvil-context-dependencies) 1 0) '
                '(if (featurep \'anvil-context-project) 1 0))) '
                '(null anvil-memory--db) (null anvil-worklog--db) '
                '(null anvil-standalone--encoding-cache) '
                '(null anvil-standalone--response-cache) '
                '(null anvil-server--running) '
                '(= (hash-table-count anvil-server-metrics--table) 0)) '
                '(error "Invalid Anvil heap")) '
                '(nelisp--write-stderr-line "ANVIL-IMAGE-RESTORED")')
        run = subprocess.run(self.command("--cold-load-from", self.native(path),
            "--eval", "(progn " + form + ")"), env=self.child_env(),
            stdin=subprocess.DEVNULL, capture_output=True, timeout=30)
        if run.returncode or b"ANVIL-IMAGE-RESTORED" not in run.stderr:
            raise RuntimeError("heap restore smoke failed: " +
                               run.stderr.decode(errors="replace")[-1200:])

    def compatible(self, path):
        """Check the runtime's documented 48-byte build identity trailer."""
        with path.open("rb") as stream:
            stream.seek(-48, os.SEEK_END)
            trailer = stream.read(48)
        return trailer == (struct.pack("<QQ", 1195655758, 1) +
                           bytes.fromhex(self.config["build_digest"]))

    def image(self):
        started = time.perf_counter()
        self.configure()
        self.timings = dict(runtime_identity=time.perf_counter()-started)
        self.cache.mkdir(parents=True, exist_ok=True, mode=0o700)
        manifest = self.cache/"manifest.json"
        with locked(self.cache/".lock"):
            try:
                meta = json.loads(manifest.read_text(encoding="utf-8"))
                tick = time.perf_counter()
                path = self.cache/(meta["key"]+".flat")
                direct = self.stamps(meta, path)
                self.timings["sources_and_providers"] = time.perf_counter()-tick
                tick = time.perf_counter()
                valid_path = (not path.is_symlink() and path.is_file()
                              and self.compatible(path))
                if (meta["stamps"] == direct and direct_stamps_safe(direct)
                        and valid_path):
                    print("anvil-standalone: image hit "+meta["key"], file=sys.stderr)
                    self.timings["image_validation"] = time.perf_counter()-tick
                    self.timings["image_total"] = time.perf_counter()-started
                    return path
                # A changed stamp selects the authoritative SHA-256 layer.
                inputs = self.inventory(meta)
                identity = key(inputs)
                after = self.stamps(meta, path)
                if (direct == after and meta["key"] == identity
                        and meta["inputs"] == inputs and valid_path
                        and digest(path) == meta["image_sha256"]):
                    meta["stamps"] = after
                    self.write_manifest(manifest, meta)
                    print("anvil-standalone: image hit "+identity, file=sys.stderr)
                    self.timings["image_validation"] = time.perf_counter()-tick
                    self.timings["image_total"] = time.perf_counter()-started
                    return path
            except (OSError, ValueError, KeyError, TypeError):
                pass
            # First discover dependency closure; then snapshot BEFORE the
            # actual build and compare again AFTER, including provider dirs.
            for attempt in range(2):
                with tempfile.TemporaryDirectory(prefix=".build-", dir=self.cache) as name:
                    tmp = Path(name)
                    discovered, _ = self.probe(tmp)
                    stamp_before = self.dependency_stamps(discovered)
                    before = self.inventory(discovered)
                    if stamp_before != self.dependency_stamps(discovered):
                        continue
                    actual, image = self.probe(tmp, True)
                    stamp_after = self.dependency_stamps(actual)
                    after = self.inventory(actual)
                    if (stamp_before != stamp_after
                            or stamp_after != self.dependency_stamps(actual)):
                        continue
                    if actual != discovered or before != after:
                        continue
                    identity = key(after)
                    path = self.cache/(identity+".flat")
                    self.smoke(image)
                    image_sha256 = digest(image)
                    if stamp_after != self.dependency_stamps(actual):
                        continue
                    actual.update(key=identity, inputs=after, image_sha256=image_sha256)
                    os.replace(image, path)
                    # Pair the hashed inputs with the stamps observed around
                    # that hash. A later mutation then misses direct mode.
                    actual["stamps"] = dict(stamp_after, image=file_stamp(path))
                    self.write_manifest(manifest, actual)
                    print("anvil-standalone: built image "+identity, file=sys.stderr)
                    return path
            raise RuntimeError("sources changed during both build attempts")

    def serve(self):
        try:
            if self.env.get("ANVIL_STANDALONE_NO_IMAGE"):
                raise RuntimeError("image explicitly disabled")
            image = self.image()
            args = ["--cold-load-from", self.native(image),
                    "--eval", '(progn (nelisp--write-stderr-line "ANVIL-IMAGE-READY") '
                              '(anvil-standalone-start))']
        except (OSError, ValueError, RuntimeError, subprocess.TimeoutExpired) as exc:
            print("anvil-standalone: source fallback: "+str(exc), file=sys.stderr)
            args = ["--load", self.native(PROFILE)]
        env = self.child_env()
        if os.name != "nt":
            import resource
            soft, hard = resource.getrlimit(resource.RLIMIT_STACK)
            resource.setrlimit(resource.RLIMIT_STACK, (hard, hard))
        return self.supervise(args, env)

    def supervise(self, args, env):
        """Gate a single input writer on readiness; never replay executed input."""
        pending = queue.Queue(maxsize=8)
        active = [None]

        def stop(signum, _frame):
            if active[0] is not None:
                active[0].terminate()
                active[0].wait()
            raise SystemExit(128+signum)

        if os.name != "nt":
            signal.signal(signal.SIGTERM, stop)
            signal.signal(signal.SIGINT, stop)

        def read_input():
            while True:
                try:
                    data = os.read(sys.stdin.fileno(), 65536)
                except OSError:
                    data = b""
                pending.put(data)
                if not data:
                    return

        threading.Thread(target=read_input, daemon=True).start()
        for attempt in range(2):
            launched = time.perf_counter()
            image_attempt = "--cold-load-from" in args
            try:
                child = subprocess.Popen(self.command(*args), env=env,
                    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            except OSError as error:
                print("anvil-standalone: launch failed: "+str(error), file=sys.stderr)
                if attempt or not image_attempt:
                    return 1
                args = ["--load", self.native(PROFILE)]
                continue
            active[0] = child
            ready, done = threading.Event(), threading.Event()
            output_seen = False
            self.timings = getattr(self, "timings", {})

            def errors(child=child, ready=ready, launched=launched):
                for line in child.stderr:
                    if line.rstrip() == b"ANVIL-IMAGE-READY":
                        self.timings["runtime_to_entry"] = time.perf_counter()-launched
                        # Irrevocable before the writer can consume any input.
                        ready.set()
                    else:
                        try:
                            sys.stderr.buffer.write(line); sys.stderr.buffer.flush()
                        except OSError:
                            return

            def write_input(child=child, ready=ready, done=done):
                while not ready.wait(.1):
                    if done.is_set():
                        return
                while not done.is_set():
                    try:
                        data = pending.get(timeout=.1)
                    except queue.Empty:
                        continue
                    try:
                        if not data:
                            child.stdin.close()
                            return
                        child.stdin.write(data); child.stdin.flush()
                    except (BrokenPipeError, OSError):
                        return

            error_thread = threading.Thread(target=errors, daemon=True)
            writer_thread = threading.Thread(target=write_input, daemon=True)
            error_thread.start()
            writer_thread.start()
            if not image_attempt:
                ready.set()
            try:
                while True:
                    data = os.read(child.stdout.fileno(), 65536)
                    if not data:
                        break
                    first = not output_seen
                    output_seen = True
                    if first and self.env.get("ANVIL_STANDALONE_TIMINGS"):
                        self.timings["runtime_to_first_response"] = time.perf_counter()-launched
                        print("ANVIL-START-TIMINGS "+json.dumps(self.timings), file=sys.stderr)
                    sys.stdout.buffer.write(data); sys.stdout.buffer.flush()
                status = child.wait()
            except OSError as error:
                # An established protocol may have executed requests. A client
                # pipe failure is terminal, never a reason for source replay.
                print("anvil-standalone: transport failed: "+str(error), file=sys.stderr)
                status = 1
                ready.set()
                with contextlib.suppress(OSError):
                    sys.stdout.close()
            finally:
                if child.poll() is None:
                    child.terminate(); child.wait()
                done.set()
                writer_thread.join(timeout=1)
                error_thread.join(timeout=1)
                child.stdout.close()
                child.stderr.close()
                if not child.stdin.closed:
                    with contextlib.suppress(OSError):
                        child.stdin.close()
                active[0] = None
            if status == 0 or ready.is_set() or output_seen or attempt or not image_attempt:
                return status
            print("anvil-standalone: image startup failed; source fallback", file=sys.stderr)
            args = ["--load", self.native(PROFILE)]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default=os.environ.get("NELISP_BIN", "nelisp"))
    parser.add_argument("--runner", default=os.environ.get("NELISP_RUNNER"))
    args = parser.parse_args()
    return Launcher(args.binary, args.runner).serve()


if __name__ == "__main__":
    raise SystemExit(main())
