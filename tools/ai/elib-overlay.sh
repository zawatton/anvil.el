#!/bin/sh
# Compose a nelisp-emacs-lib src/ for testing: the live checkout's src/
# (with its .elc) plus the files where a branch worktree differs from the
# live checkout's HEAD, recompiled.
# Usage: elib-overlay.sh LIVE_REPO BRANCH_WORKTREE OUT_DIR  -> OUT_DIR/src
live=$1 wt=$2 out=$3
[ -d "$live/src" ] && [ -d "$wt/src" ] && [ -n "$out" ] || { echo "usage: $0 LIVE WT OUT" >&2; exit 2; }
rm -rf "$out/src" "$out/vendor" && mkdir -p "$out" && cp -r "$live/src" "$out/src" || exit 1
# Vendor sources are located relative to the repo root; without them the
# vendored Org parser load is silently skipped.
ln -s "$live/vendor" "$out/vendor" || exit 1
# Files where the worktree differs from the live checkout's HEAD commit.
base=$(git -C "$live" rev-parse HEAD) || exit 1
files=$(cd "$wt" && git diff --name-only "$base" -- 'src/*.el') || exit 1
[ -n "$files" ] || exit 0
for f in $files; do cp "$wt/$f" "$out/$f" || exit 1; done
cd "$out" && emacs -Q --batch -L src -f batch-byte-compile $files >/dev/null 2>&1 || { echo "compile failed" >&2; exit 1; }
# The loader may prefer the newer of .el/.elc; make the fresh .elc newer.
sleep 1; for f in $files; do touch "${f}c"; done
