#!/usr/bin/env bash
# anvil-stdio.sh - Connect to Anvil MCP server via stdio transport
#
# Copyright (C) 2025 Laurynas Biveinis
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

set -eu -o pipefail

# A transport failure must remain a failure.  In particular, never let
# emacsclient reinterpret an Elisp expression as a file for another editor.
unset ALTERNATE_EDITOR

# Default values
INIT_FUNCTION=""
STOP_FUNCTION=""
SOCKET=""
SERVER_ID=""
EMACS_MCP_DEBUG_LOG=${EMACS_MCP_DEBUG_LOG:-""}
ANVIL_MCP_PARENT_GUARD=${ANVIL_MCP_PARENT_GUARD:-""}
ANVIL_MCP_PARENT_GUARD_PYTHON=${ANVIL_MCP_PARENT_GUARD_PYTHON:-""}

# Keep a restrictive mask for a caller-selected debug log.  Stateful stderr is
# discarded through a sink opened once at startup; creating and later cleaning
# one pathname per request would reintroduce loader-dependent helpers.
umask 077
if ! exec 8>/dev/null; then
	echo "anvil-mcp: cannot open dispatch stderr sink" >&2
	exit 70
fi
ANVIL_MCP_RESPONSE_PENDING=0

# Bash 4.2+ can format wall-clock time without execing `date'.  Older Bash
# releases (notably macOS Bash 3.2) retain a process-free relative timestamp.
_anvil_printf_time=0
if printf -v _anvil_timestamp_probe '%(%Y)T' -1 2>/dev/null; then
	_anvil_printf_time=1
fi
unset _anvil_timestamp_probe

# Debug logging setup.  Open one regular, non-symlink destination before any
# request.  During a stateful dispatch logging is suppressed completely, so
# diagnostics can never delay delivery of an already-executed response.
if [ -n "$EMACS_MCP_DEBUG_LOG" ]; then
	if [ -L "$EMACS_MCP_DEBUG_LOG" ] \
		|| { [ -e "$EMACS_MCP_DEBUG_LOG" ] && [ ! -f "$EMACS_MCP_DEBUG_LOG" ]; }; then
		echo "anvil-mcp: debug log must be a regular non-symlink file" >&2
		exit 64
	fi
	if ! : >>"$EMACS_MCP_DEBUG_LOG" || [ ! -f "$EMACS_MCP_DEBUG_LOG" ]; then
		echo "anvil-mcp: cannot create debug log: $EMACS_MCP_DEBUG_LOG" >&2
		exit 70
	fi
	if ! exec 9>>"$EMACS_MCP_DEBUG_LOG"; then
		echo "anvil-mcp: cannot open debug log: $EMACS_MCP_DEBUG_LOG" >&2
		exit 70
	fi

	mcp_debug_log() {
		local direction="$1" message="$2" timestamp
		[ "$ANVIL_MCP_RESPONSE_PENDING" -eq 0 ] || return 0
		message="${message:0:2048}"
		if [ "$_anvil_printf_time" -eq 1 ]; then
			printf -v timestamp '%(%Y-%m-%d %H:%M:%S)T' -1
		else
			printf -v timestamp '+%ss' "$SECONDS"
		fi
		printf '[%s] [%s] MCP-%s: %s\n' \
			"$timestamp" "$$" "$direction" "$message" >&9 2>/dev/null || true
	}

	mcp_debug_log "INFO" "Debug logging enabled"

	# Log the path and resolved steady-state tools without starting diagnostic
	# helpers.  A debug-only git/stat/dirname freeze must not block MCP startup.
	_anvil_script_path="$0"
	mcp_debug_log "INFO" "anvil-stdio.sh path=$_anvil_script_path"
	mcp_debug_log "INFO" \
		"tooling bash=$(command -v bash || printf '?') python3=$(command -v python3 || printf '?') emacsclient=$(command -v emacsclient || printf '?')"
else
	mcp_debug_log() {
		:
	}
fi

# Probe diagnostics are captured before dispatch and may therefore share the
# readiness command's combined output.  No capture pathname is needed.
anvil_mcp_log_probe_stderr() {
	local direction="$1" text="$2"
	[ -n "$text" ] || return 0
	text="${text//$'\n'/ }"
	mcp_debug_log "$direction" "${text:0:4096}"
}

# FD 8 is a process-lifetime stderr sink opened before any stateful dispatch.
# These hooks remain explicit so capture setup failure is still distinguishable
# from an ambiguous post-dispatch failure, without per-request files or reads.
anvil_mcp_capture_begin() {
	: >&8
}

anvil_mcp_capture_finish() {
	:
}

# Run one text-only external program under a Bash-owned deadline.  GNU
# `timeout' cannot bound its own dynamic-loader startup; process substitution
# plus builtin `read -t' can.  The runner discovers its real PID through a
# pre-dispatch Python child because Bash 3.2 reports stale $$/$PPID values in
# subshells.  A delayed guard loader must validate that numeric owner instead
# of adopting a reparent/subreaper.  NUL is forbidden in helper output.
anvil_mcp_exec_program() {
	if [ -n "$ANVIL_MCP_PARENT_GUARD" ]; then
		ANVIL_HEADLESS_PARENT_PID="$ANVIL_MCP_RUNNER_PID" exec \
			"$ANVIL_MCP_PARENT_GUARD_PYTHON" -I -S \
			"$ANVIL_MCP_PARENT_GUARD" group "$@"
	else
		exec "$@"
	fi
}

anvil_mcp_exec_child() {
	local stderr_mode="$1" input_mode="$2" input="$3"
	shift 3
	exec 7<&- 9>&-
	case "$input_mode:$stderr_mode" in
	input:merge)
		anvil_mcp_exec_program "$@" < <(printf '%s' "$input") 2>&1 8>&-
		;;
	input:separate)
		anvil_mcp_exec_program "$@" < <(printf '%s' "$input") 2>&8 8>&-
		;;
	descriptor:merge)
		anvil_mcp_exec_program "$@" <&5 5<&- 2>&1 8>&-
		;;
	descriptor:separate)
		anvil_mcp_exec_program "$@" <&5 5<&- 2>&8 8>&-
		;;
	inherit:merge)
		anvil_mcp_exec_program "$@" 2>&1 8>&-
		;;
	inherit:separate)
		anvil_mcp_exec_program "$@" 2>&8 8>&-
		;;
	null:merge)
		anvil_mcp_exec_program "$@" </dev/null 2>&1 8>&-
		;;
	null:separate)
		anvil_mcp_exec_program "$@" </dev/null 2>&8 8>&-
		;;
	*)
		printf 'anvil-mcp: invalid bounded-run mode: %s/%s' \
			"$input_mode" "$stderr_mode"
		return 64
		;;
	esac
}

anvil_mcp_run_child() {
	local guard_deadline="$1" stderr_mode="$2" input_mode="$3" input="$4"
	local child="" rc=70 runner_pid="" timed_out=0
	shift 4

	# A guarded launch keeps its Python loader in the bridge group until the
	# loaded guard validates the exact runner and moves the target.  Timeout
	# signals cover the pre-group PID and post-group PGID.
	trap '
		timed_out=1
		[ -z "$child" ] || kill -TERM "$child" 2>/dev/null || :
		[ -z "$child" ] || kill -TERM -- "-$child" 2>/dev/null || :
	' TERM
	trap '
		timed_out=1
		[ -z "$child" ] || kill -KILL "$child" 2>/dev/null || :
		[ -z "$child" ] || kill -KILL -- "-$child" 2>/dev/null || :
	' USR1

	if [ -n "$ANVIL_MCP_PARENT_GUARD" ]; then
		[ "$guard_deadline" -le 5 ] || guard_deadline=5
		exec 4< <(exec "$ANVIL_MCP_PARENT_GUARD_PYTHON" -I -S -c \
			'import os; print(os.getppid())' 2>/dev/null)
		child=$!
		if ! IFS= read -r -t "$guard_deadline" runner_pid <&4; then
			exec 4<&-
			kill -KILL "$child" 2>/dev/null || :
			trap - TERM USR1
			printf '\0%s\n' 124
			return
		fi
		exec 4<&-
		case "$runner_pid" in
		""|*[!0-9]*|0|1)
			kill -KILL "$child" 2>/dev/null || :
			trap - TERM USR1
			printf '\0%s\n' 70
			return
			;;
		esac
		ANVIL_MCP_RUNNER_PID=$runner_pid
		child=""
	else
		set -m
	fi

	anvil_mcp_exec_child "$stderr_mode" "$input_mode" "$input" "$@" &
	child=$!
	while :; do
		if wait "$child"; then rc=0; else rc=$?; fi
		kill -0 "$child" 2>/dev/null || break
	done
	trap - TERM USR1
	# A leader that daemonized must not leave same-group descendants holding
	# the result pipe open after the direct child exits.
	kill -KILL -- "-$child" 2>/dev/null || :
	[ "$timed_out" -eq 0 ] || rc=124
	printf '\0%s\n' "$rc"
}

# Results are returned in ANVIL_MCP_RUN_OUTPUT / STATUS.  A missing sentinel is
# always a timeout/failure; partial output is deliberately ignored.
anvil_mcp_run_bounded() {
	local deadline="$1" stderr_mode="$2" input_mode="$3" input="$4"
	local runner status=""
	shift 4
	ANVIL_MCP_RUN_OUTPUT=""
	ANVIL_MCP_RUN_STATUS=70

	exec 7< <(anvil_mcp_run_child \
		"$deadline" "$stderr_mode" "$input_mode" "$input" "$@")
	runner=$!
	if IFS= read -r -d '' -t "$deadline" ANVIL_MCP_RUN_OUTPUT <&7; then
		if IFS= read -r -t "$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT" status <&7 \
			&& [[ "$status" =~ ^[0-9]+$ ]] \
			&& [ "$status" -le 255 ]; then
			ANVIL_MCP_RUN_STATUS=$status
		fi
	else
		ANVIL_MCP_RUN_OUTPUT=""
		ANVIL_MCP_RUN_STATUS=124
		if kill -0 "$runner" 2>/dev/null; then
			kill -TERM "$runner" 2>/dev/null || :
			if ! IFS= read -r -d '' \
				-t "$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT" _ <&7; then
				kill -USR1 "$runner" 2>/dev/null || :
				IFS= read -r -d '' \
					-t "$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT" _ <&7 || :
			fi
			kill -KILL "$runner" 2>/dev/null || :
		fi
	fi
	exec 7<&-
}

# --- Retry wrapper for emacsclient ------------------------------------
# Absorbs the ~few-second window where `emacs --daemon' is being
# restarted: emacsclient then fails with "can't find socket" /
# "Connection refused" until the new daemon's server file is ready.
# Retrying silently keeps the MCP pipe alive so upstream Claude Code
# (or whoever is driving this bridge) doesn't see a hard failure for
# a routine daemon bounce.
#
# Configure with env vars (defaults chosen to cover a typical restart):
#   ANVIL_EMACSCLIENT_RETRY_MAX        attempts (default 60)
#   ANVIL_EMACSCLIENT_RETRY_DELAY_MS   delay per attempt in ms (default 100)
# 60 * 100ms = 6 seconds of tolerance.
ANVIL_EMACSCLIENT_RETRY_MAX=${ANVIL_EMACSCLIENT_RETRY_MAX:-60}
ANVIL_EMACSCLIENT_RETRY_DELAY_MS=${ANVIL_EMACSCLIENT_RETRY_DELAY_MS:-100}
# Per-attempt readiness timeout.  Timed-out readiness probes are replayed
# only within the bounded readiness phase; the probe expression is pure.
# Runtime overrides may shorten these values for tests or local policy, but
# may never enlarge the package's cross-client safety envelope.
ANVIL_EMACSCLIENT_PROBE_TIMEOUT=${ANVIL_EMACSCLIENT_PROBE_TIMEOUT:-5}
ANVIL_EMACSCLIENT_READINESS_TIMEOUT=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT:-20}
ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT=${ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT:-20}
ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT=${ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT:-150}
ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT=${ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT:-1}
ANVIL_MCP_REQUEST_PARSE_TIMEOUT=${ANVIL_MCP_REQUEST_PARSE_TIMEOUT:-10}
ANVIL_MCP_FRAME_READ_TIMEOUT=${ANVIL_MCP_FRAME_READ_TIMEOUT:-10}
readonly ANVIL_MCP_MAX_REQUEST_BYTES=16777216
# Linux limits each exec argument to 128 KiB even when ARG_MAX is much larger.
# Keep inline request expressions comfortably below that boundary; larger
# requests travel through one private bridge-owned staging directory instead.
readonly ANVIL_MCP_INLINE_REQUEST_BYTES=16384
_anvil_request_tmp=${TMPDIR:-/tmp}
case "$_anvil_request_tmp" in
/) ANVIL_MCP_REQUEST_DIRECTORY="/anvil-mcp.$$.$RANDOM$RANDOM$RANDOM$RANDOM" ;;
*) ANVIL_MCP_REQUEST_DIRECTORY="${_anvil_request_tmp%/}/anvil-mcp.$$.$RANDOM$RANDOM$RANDOM$RANDOM" ;;
esac
readonly ANVIL_MCP_REQUEST_DIRECTORY
unset _anvil_request_tmp
ANVIL_MCP_REQUEST_SEQUENCE=0

anvil_mcp_validate_timeout() {
	local name="$1" value="$2" maximum="$3"
	case "$value" in
	""|*[!0-9]*)
		echo "anvil-mcp: $name must be an integer between 1 and $maximum" >&2
		return 64
		;;
	esac
	if [ "$value" -lt 1 ] || [ "$value" -gt "$maximum" ]; then
		echo "anvil-mcp: $name must be between 1 and $maximum seconds" >&2
		return 64
	fi
}

anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_PROBE_TIMEOUT \
	"$ANVIL_EMACSCLIENT_PROBE_TIMEOUT" 5
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_READINESS_TIMEOUT \
	"$ANVIL_EMACSCLIENT_READINESS_TIMEOUT" 20
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT \
	"$ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT" 20
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT \
	"$ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT" 150
anvil_mcp_validate_timeout ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT \
	"$ANVIL_EMACSCLIENT_KILL_AFTER_TIMEOUT" 1
anvil_mcp_validate_timeout ANVIL_MCP_REQUEST_PARSE_TIMEOUT \
	"$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" 10
anvil_mcp_validate_timeout ANVIL_MCP_FRAME_READ_TIMEOUT \
	"$ANVIL_MCP_FRAME_READ_TIMEOUT" 10

ANVIL_MCP_PYTHON=$(type -P python3 || :)
ANVIL_MCP_EMACSCLIENT=$(type -P emacsclient || :)
ANVIL_MCP_SLEEP=$(type -P sleep || :)
for _anvil_program in \
	ANVIL_MCP_PYTHON ANVIL_MCP_EMACSCLIENT ANVIL_MCP_SLEEP; do
	if [ -z "${!_anvil_program}" ]; then
		echo "anvil-mcp: ${_anvil_program#ANVIL_MCP_} is required" >&2
		exit 69
	fi
done
readonly ANVIL_MCP_PYTHON ANVIL_MCP_EMACSCLIENT ANVIL_MCP_SLEEP
unset _anvil_program
if [ -n "$ANVIL_MCP_PARENT_GUARD" ]; then
	[ -n "$ANVIL_MCP_PARENT_GUARD_PYTHON" ] \
		|| ANVIL_MCP_PARENT_GUARD_PYTHON=$ANVIL_MCP_PYTHON
	case "$ANVIL_MCP_PARENT_GUARD:$ANVIL_MCP_PARENT_GUARD_PYTHON" in
	/*:/*) ;;
	*)
		echo "anvil-mcp: parent guard paths must be absolute" >&2
		exit 64
		;;
	esac
	if [ ! -r "$ANVIL_MCP_PARENT_GUARD" ] \
		|| [ ! -x "$ANVIL_MCP_PARENT_GUARD_PYTHON" ]; then
		echo "anvil-mcp: parent guard or its Python is unavailable" >&2
		exit 69
	fi
elif [ -n "$ANVIL_MCP_PARENT_GUARD_PYTHON" ]; then
	echo "anvil-mcp: parent guard Python was set without a guard" >&2
	exit 64
fi
readonly ANVIL_MCP_PARENT_GUARD ANVIL_MCP_PARENT_GUARD_PYTHON

# Run one emacsclient invocation under a hard timeout.  This helper never
# retries: callers must decide whether the expression is safe to replay.
anvil_emacsclient_once() {
	local timeout_seconds="$1"
	shift
	if [ "${1-}" = "--" ]; then shift; fi

	local out="" rc=0
	anvil_mcp_run_bounded "$timeout_seconds" merge null "" \
		"$ANVIL_MCP_EMACSCLIENT" -a false "$@"
	out=$ANVIL_MCP_RUN_OUTPUT
	rc=$ANVIL_MCP_RUN_STATUS
	printf '%s' "$out"
	return "$rc"
}

# Retry only the side-effect-free readiness expression.  A sibling bridge may
# occupy the single Emacs server event loop longer than one probe timeout, so
# timeout exits are replayed until ANVIL_EMACSCLIENT_READINESS_TIMEOUT expires.
# Missing/refused socket races retain their independent attempt limit.
anvil_emacsclient_probe_retry() {
	if [ "${1-}" = "--" ]; then shift; fi

	local socket_attempt=0 timeout_attempt=0 out="" rc=0
	local started_seconds=$SECONDS this_timeout elapsed remaining
	while :; do
		this_timeout=$ANVIL_EMACSCLIENT_PROBE_TIMEOUT
		if [ "$ANVIL_EMACSCLIENT_READINESS_TIMEOUT" = "0" ]; then
			# A disabled overall timeout means one unbounded readiness wait.
			this_timeout=0
		else
			elapsed=$((SECONDS - started_seconds))
			remaining=$((ANVIL_EMACSCLIENT_READINESS_TIMEOUT - elapsed))
			if [ "$remaining" -le 0 ]; then
				mcp_debug_log "PROBE-WAIT-EXHAUSTED" \
					"timeouts=$timeout_attempt budget=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT}s rc=$rc"
				printf '%s' "$out"
				return "$rc"
			fi
			if [ "$this_timeout" -eq 0 ] \
				|| [ "$this_timeout" -gt "$remaining" ]; then
				this_timeout=$remaining
			fi
		fi
		if out=$(anvil_emacsclient_once "$this_timeout" -- "$@"); then
			rc=0
		else
			rc=$?
		fi
		if [ "$rc" -eq 0 ]; then
			printf '%s' "$out"
			return 0
		fi
		case "$rc" in
		124|137|142)
			timeout_attempt=$((timeout_attempt + 1))
			mcp_debug_log "PROBE-TIMEOUT" \
				"attempt=$timeout_attempt rc=$rc budget=${ANVIL_EMACSCLIENT_READINESS_TIMEOUT}s"
			continue
			;;
		esac
		if [[ "$out" == *"can't find socket"* \
			|| "$out" == *"Connection refused"* \
			|| "$out" == *server*not*running* \
			|| "$out" == *"No such file or directory"* ]]; then
			socket_attempt=$((socket_attempt + 1))
			if [ "$socket_attempt" -ge "$ANVIL_EMACSCLIENT_RETRY_MAX" ]; then
				mcp_debug_log "PROBE-RETRY-EXHAUSTED" "attempts=$socket_attempt max=$ANVIL_EMACSCLIENT_RETRY_MAX rc=$rc"
				printf '%s' "$out"
				return "$rc"
			fi
			if [ "$socket_attempt" -eq 1 ] || [ $((socket_attempt % 10)) -eq 0 ]; then
				local probe_summary="${out//$'\n'/ }"
				mcp_debug_log "PROBE-RETRY" \
					"attempt=$socket_attempt rc=$rc stderr=${probe_summary:0:120}"
			fi
			if [ "$ANVIL_EMACSCLIENT_RETRY_DELAY_MS" -gt 0 ]; then
				local delay_sec
				printf -v delay_sec '%d.%03d' \
					"$((ANVIL_EMACSCLIENT_RETRY_DELAY_MS / 1000))" \
					"$((ANVIL_EMACSCLIENT_RETRY_DELAY_MS % 1000))"
				local delay_cap=$((ANVIL_EMACSCLIENT_RETRY_DELAY_MS / 1000 + 1))
				anvil_mcp_run_bounded "$delay_cap" merge null "" \
					"$ANVIL_MCP_SLEEP" "$delay_sec"
			fi
			continue
		fi
		printf '%s' "$out"
		return "$rc"
	done
}

anvil_emacsclient_dispatch_once() {
	local timeout_seconds="$1"
	shift
	if [ "${1-}" = "--" ]; then shift; fi

	local out="" rc=0
	anvil_mcp_run_bounded "$timeout_seconds" separate null "" \
		"$ANVIL_MCP_EMACSCLIENT" -a false "$@"
	out=$ANVIL_MCP_RUN_OUTPUT
	rc=$ANVIL_MCP_RUN_STATUS
	printf '%s' "$out"
	return "$rc"
}

# Parse command line arguments
while [ $# -gt 0 ]; do
	case "$1" in
	--init-function=*)
		INIT_FUNCTION="${1#--init-function=}"
		shift
		;;
	--stop-function=*)
		STOP_FUNCTION="${1#--stop-function=}"
		shift
		;;
	--socket=*)
		SOCKET="${1#--socket=}"
		shift
		;;
	--server-id=*)
		SERVER_ID="${1#--server-id=}"
		shift
		;;
	*)
		echo "Unknown option: $1" >&2
		echo "Usage: $0 [--init-function=name] [--stop-function=name] [--socket=path] [--server-id=id]" >&2
		exit 1
		;;
	esac
done

# Set socket arguments if provided
if [ -n "$SOCKET" ]; then
	readonly SOCKET_OPTIONS=("-s" "$SOCKET")
	mcp_debug_log "INFO" "Using socket: $SOCKET"
else
	readonly SOCKET_OPTIONS=()
fi

# Log init function info if provided
if [ -n "$INIT_FUNCTION" ]; then
	mcp_debug_log "INFO" "Using init function: $INIT_FUNCTION"

	# Derive server-id from init function if not explicitly provided
	# This is a hack for backwards compatibility and will be removed later
	if [ -z "$SERVER_ID" ]; then
		# Extract server-id by removing -mcp-enable suffix
		SERVER_ID="${INIT_FUNCTION%-mcp-enable}"
		mcp_debug_log "INFO" "Derived server-id from init function: $SERVER_ID"
	fi
else
	mcp_debug_log "INFO" "No init function specified"
fi

# Log server-id
if [ -n "$SERVER_ID" ]; then
	mcp_debug_log "INFO" "Using server-id: $SERVER_ID"
else
	# Default to "default" if not specified
	SERVER_ID="default"
	mcp_debug_log "INFO" "Using default server-id: $SERVER_ID"
fi

# Initialize MCP if init function is provided.  Probe readiness with the
# only replayable expression, then invoke the potentially stateful init once.
if [ -n "$INIT_FUNCTION" ]; then
	mcp_debug_log "INIT-CALL" "readiness probe, then one emacsclient -e ($INIT_FUNCTION)"

	init_probe_output=""
	set +e
	init_probe_output=$(anvil_emacsclient_probe_retry -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} -e t)
	INIT_READY_RC=$?
	INIT_RC=$INIT_READY_RC
	set -e
	if [ "$INIT_READY_RC" -ne 0 ]; then
		anvil_mcp_log_probe_stderr "INIT-PROBE-STDERR" "$init_probe_output"
	fi
	mcp_debug_log "INIT-READY-RC" "$INIT_READY_RC"

	if [ "$INIT_READY_RC" -eq 0 ]; then
		if anvil_mcp_capture_begin; then
			ANVIL_MCP_RESPONSE_PENDING=1
			set +e
			anvil_emacsclient_dispatch_once \
				"$ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT" -- \
				${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
				-e "($INIT_FUNCTION)" >/dev/null
			INIT_RC=$?
			set -e
			anvil_mcp_capture_finish
			ANVIL_MCP_RESPONSE_PENDING=0
		else
			INIT_RC=74
			mcp_debug_log "INIT-CAPTURE" "failed before dispatch rc=$INIT_RC"
		fi
	fi
	mcp_debug_log "INIT-RC" "$INIT_RC"
else
	mcp_debug_log "INFO" "Skipping init function call (none provided)"
fi

# --- T71: MCP Content-Length framing read helpers --------------------
# The standard MCP stdio transport frames messages as:
#
#     Content-Length: <N>\r\n
#     \r\n
#     <N bytes JSON body>
#
# Older callers may still emit line-delimited JSON.  We sniff the first
# byte of each request: if it is `{', treat as legacy line mode; if it
# is `C' (start of "Content-Length:"), treat as framed.  Mode is
# decided per-request to keep the legacy fallback transparent for
# dev/test invocations.
#
# Output: when input was framed, emit a framed response; otherwise emit
# legacy single-line JSON.

# anvil_mcp_read_framed_message
#
# Reads one MCP framed message from STDIN.  Headers are read line by
# line until an empty line; then exactly N bytes of body are read.
# Prints the JSON body on STDOUT (no trailing newline).  Returns 1 on
# EOF or malformed framing, 2 if no Content-Length header found.
anvil_mcp_read_framed_message() {
	local first_line="$1"
	local frame_deadline="$2"
	local header_line content_length="" frame_remaining
	ANVIL_MCP_FRAME_BODY=""

	# Process the already-consumed first line.
	# Strip trailing CR (DOS line endings).
	first_line="${first_line%$'\r'}"
	if [[ "$first_line" =~ ^[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]:[[:space:]]*([0-9]+)[[:space:]]*$ ]]; then
		content_length="${BASH_REMATCH[1]}"
	fi

	# Read remaining header lines under the same cumulative frame deadline as
	# the body.  A client that stalls with stdin open after Content-Length must
	# not retain this bridge indefinitely before dispatch.
	while :; do
		frame_remaining=$((frame_deadline - SECONDS))
		[ "$frame_remaining" -gt 0 ] || return 1
		if ! LC_ALL=C IFS= read -r -t "$frame_remaining" header_line; then
			return 1
		fi
		header_line="${header_line%$'\r'}"
		if [ -z "$header_line" ]; then
			break
		fi
		if [[ "$header_line" =~ ^[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]:[[:space:]]*([0-9]+)[[:space:]]*$ ]]; then
			content_length="${BASH_REMATCH[1]}"
		fi
	done

	if [ -z "$content_length" ]; then
		return 2
	fi
	if [ "${#content_length}" -gt 8 ] \
		|| [ "$content_length" -gt "$ANVIL_MCP_MAX_REQUEST_BYTES" ]; then
		return 3
	fi

	# Read exactly content_length bytes within the unspent frame budget.  The
	# bounded runner preserves trailing newlines that command substitution would
	# strip.  The unbuffered reader never asks the pipe for more than the frame's
	# remaining bytes; `head -c' may over-read and discard a pipelined request.
	frame_remaining=$((frame_deadline - SECONDS))
	[ "$frame_remaining" -gt 0 ] || return 1
	mcp_debug_log "FRAMING" \
		"body reader start bytes=$content_length remaining=$frame_remaining"
	exec 5<&0
	anvil_mcp_run_bounded "$frame_remaining" merge descriptor "" \
		"$ANVIL_MCP_PYTHON" -I -S -c '
import os
import sys

remaining = int(sys.argv[1])
while remaining:
    chunk = os.read(0, min(65536, remaining))
    if not chunk:
        raise SystemExit(1)
    view = memoryview(chunk)
    while view:
        written = os.write(1, view)
        if written <= 0:
            raise SystemExit(1)
        view = view[written:]
    remaining -= len(chunk)
' "$content_length"
	exec 5<&-
	if [ "$ANVIL_MCP_RUN_STATUS" -ne 0 ]; then
		return 1
	fi
	local LC_ALL=C
	if [ "${#ANVIL_MCP_RUN_OUTPUT}" -ne "$content_length" ]; then
		return 1
	fi
	ANVIL_MCP_FRAME_BODY=$ANVIL_MCP_RUN_OUTPUT
	return 0
}

# anvil_mcp_emit_framed_response BODY
#
# Emits BODY framed with Content-Length.  N is computed in bytes
# (not characters) because the MCP spec mandates byte length.
anvil_mcp_emit_framed_response() {
	local body="$1"
	local LC_ALL=C
	local n=${#body}
	printf 'Content-Length: %s\r\n\r\n%s' "$n" "$body"
}

anvil_mcp_emit_response() {
	local framed="$1"
	local body="$2"
	if [ "$framed" = "1" ]; then
		anvil_mcp_emit_framed_response "$body"
	else
		printf '%s\n' "$body"
	fi
	ANVIL_MCP_RESPONSE_PENDING=0
}

# Decode and emit a validated percent-byte wire without materializing another
# full response string.  Global substitution is quadratic in macOS Bash 3.2;
# splitting once and printing each byte remains linear for large responses.
anvil_mcp_emit_wire_response() {
	local framed="$1" wire="$2" wire_length byte_count
	local offset=0 chunk_chars=49152 chunk
	local LC_ALL=C
	local -a bytes
	wire_length=${#wire}
	byte_count=$((wire_length / 3))

	if [ "$framed" = "1" ]; then
		printf 'Content-Length: %s\r\n\r\n' "$byte_count"
	fi
	while [ "$offset" -lt "$wire_length" ]; do
		chunk="${wire:offset:chunk_chars}"
		bytes=()
		IFS='%' read -r -a bytes <<<"$chunk"
		unset 'bytes[0]'
		printf '%b' "${bytes[@]/#/\\x}"
		offset=$((offset + chunk_chars))
	done
	if [ "$framed" != "1" ]; then
		printf '\n'
	fi
	ANVIL_MCP_RESPONSE_PENDING=0
}

# Parse only bounded request metadata outside Emacs.  The root may be dead on
# the error path, so correlation cannot depend on another emacsclient call.
# Output is KIND|STARTUP|MODE|PAYLOAD|BYTE-SIZE|JSON-ID.  Small requests use an
# inline base64 payload.  Large requests are marked for bounded staging only
# after the readiness probe succeeds, avoiding both exec argument limits and
# unused files when Emacs is unavailable.
anvil_mcp_request_metadata() {
	local request="$1"
	if [ "${#request}" -gt "$ANVIL_MCP_MAX_REQUEST_BYTES" ]; then
		printf 'parse-error|0|none||0|null'
		return 0
	fi
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" \
		merge input "$request" "$ANVIL_MCP_PYTHON" -I -S -c '
import base64
import json
import math
import sys

maximum = int(sys.argv[1])
inline_maximum = int(sys.argv[2])
raw = sys.stdin.buffer.read(maximum + 1)

def emit(kind, startup, request_id):
    if kind in {"request", "notification"}:
        if len(raw) <= inline_maximum:
            mode = "inline"
            payload = base64.b64encode(raw).decode("ascii")
        else:
            mode = "file"
            payload = ""
    else:
        mode = "none"
        payload = ""
    encoded_id = json.dumps(
        request_id,
        ensure_ascii=True,
        separators=(",", ":"),
    )
    print(
        f"{kind}|{1 if startup else 0}|{mode}|{payload}|{len(raw)}|{encoded_id}"
    )

if len(raw) > maximum:
    emit("parse-error", False, None)
else:
    try:
        document = json.loads(
            raw,
            parse_constant=lambda value: (_ for _ in ()).throw(
                ValueError(value)
            ),
        )
    except (UnicodeDecodeError, ValueError):
        emit("parse-error", False, None)
    else:
        if not isinstance(document, dict):
            emit("invalid-request", False, None)
        else:
            startup = document.get("method") == "initialize"
            if "id" not in document:
                emit("notification", startup, None)
            else:
                request_id = document["id"]
                valid_id = (
                    request_id is None
                    or isinstance(request_id, str)
                    or (
                        not isinstance(request_id, bool)
                        and isinstance(request_id, int)
                    )
                    or (
                        isinstance(request_id, float)
                        and math.isfinite(request_id)
                    )
                )
                if valid_id:
                    emit("request", startup, request_id)
                else:
                    emit("invalid-request", startup, None)
' "$ANVIL_MCP_MAX_REQUEST_BYTES" "$ANVIL_MCP_INLINE_REQUEST_BYTES"
	printf '%s' "$ANVIL_MCP_RUN_OUTPUT"
	return "$ANVIL_MCP_RUN_STATUS"
}

# Stage one already-validated large request in a bridge-private directory.
# The helper is bounded and runs before stateful dispatch.  It creates both the
# directory and file without following links, verifies ownership/modes, writes
# the exact request bytes, and returns only the base64-encoded absolute path.
anvil_mcp_stage_request() {
	local request="$1" basename="$2"
	anvil_mcp_run_bounded "$ANVIL_MCP_REQUEST_PARSE_TIMEOUT" \
		merge input "$request" "$ANVIL_MCP_PYTHON" -I -S -c '
import base64
import os
import signal
import stat
import sys

requested_directory = os.path.abspath(sys.argv[1])
root = os.path.realpath(os.path.dirname(requested_directory))
directory = os.path.join(root, os.path.basename(requested_directory))
basename = sys.argv[2]
maximum = int(sys.argv[3])
raw = sys.stdin.buffer.read(maximum + 1)
if len(raw) > maximum:
    raise SystemExit(65)
if not basename.isascii() or not basename.startswith("request."):
    raise SystemExit(64)
if "/" in basename or (os.altsep and os.altsep in basename):
    raise SystemExit(64)

root_stat = os.lstat(root)
root_mode = stat.S_IMODE(root_stat.st_mode)
private_root = root_stat.st_uid == os.geteuid() and not (root_mode & 0o022)
sticky_root = bool(root_mode & stat.S_ISVTX) and root_stat.st_uid in {
    0,
    os.geteuid(),
}
if not stat.S_ISDIR(root_stat.st_mode) or not (private_root or sticky_root):
    raise SystemExit(73)

try:
    os.mkdir(directory, 0o700)
except FileExistsError:
    pass
directory_stat = os.lstat(directory)
if (
    not stat.S_ISDIR(directory_stat.st_mode)
    or directory_stat.st_uid != os.geteuid()
    or stat.S_IMODE(directory_stat.st_mode) != 0o700
):
    raise SystemExit(73)

path = os.path.join(directory, basename)
flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
if hasattr(os, "O_NOFOLLOW"):
    flags |= os.O_NOFOLLOW
descriptor = -1
created = False

def interrupted(_signum, _frame):
    raise TimeoutError("request staging interrupted")

signal.signal(signal.SIGTERM, interrupted)
try:
    descriptor = os.open(path, flags, 0o600)
    created = True
    file_stat = os.fstat(descriptor)
    if (
        not stat.S_ISREG(file_stat.st_mode)
        or file_stat.st_uid != os.geteuid()
        or file_stat.st_nlink != 1
        or stat.S_IMODE(file_stat.st_mode) != 0o600
    ):
        raise PermissionError("unsafe request staging file")
    with os.fdopen(descriptor, "wb") as stream:
        descriptor = -1
        stream.write(raw)
    print(base64.b64encode(os.fsencode(path)).decode("ascii"))
except BaseException:
    if descriptor >= 0:
        os.close(descriptor)
    if created:
        try:
            os.unlink(path)
        except OSError:
            pass
    raise
' "$ANVIL_MCP_REQUEST_DIRECTORY" "$basename" \
		"$ANVIL_MCP_MAX_REQUEST_BYTES"
	printf '%s' "$ANVIL_MCP_RUN_OUTPUT"
	return "$ANVIL_MCP_RUN_STATUS"
}

# Emit a correlated at-most-once error.  Notifications remain silent;
# malformed input receives a protocol error with id null.
anvil_mcp_synthetic_error() {
	local request_kind="$1"
	local request_id="$2"
	local framed="$3"
	local phase="$4"
	local dispatched="$5"
	local client_rc="$6"
	local code message response

	case "$request_kind" in
	notification)
		ANVIL_MCP_RESPONSE_PENDING=0
		return 0
		;;
	parse-error)
		request_id=null
		phase=parse
		dispatched=false
		code=-32700
		message="invalid JSON-RPC input before dispatch"
		;;
	invalid-request)
		request_id=null
		phase=parse
		dispatched=false
		code=-32600
		message="invalid JSON-RPC request before dispatch"
		;;
	request)
		code=-32603
		case "$phase" in
		readiness) message="daemon readiness probe failed before dispatch" ;;
		capture) message="bridge capture setup failed before dispatch" ;;
		stage) message="large request staging failed before dispatch" ;;
		*) message="daemon response was ambiguous after one dispatch" ;;
		esac
		;;
	*)
		request_id=null
		phase=parse
		dispatched=false
		code=-32603
		message="bounded JSON-RPC metadata parsing failed"
		;;
	esac
	printf -v response '{"jsonrpc":"2.0","id":%s,"error":{"code":%s,"message":"Bridge synthetic error: %s","data":{"phase":"%s","dispatched":%s,"replayed":false,"emacsclientRc":%s}}}' \
		"$request_id" "$code" "$message" "$phase" "$dispatched" "$client_rc"
	if [ "$dispatched" = "false" ]; then
		mcp_debug_log "SYNTH-ERROR" "id=$request_id phase=$phase dispatched=false replayed=false rc=$client_rc"
	fi
	anvil_mcp_emit_response "$framed" "$response"
}

# Process input and print response.  Idle stdin is intentionally unbounded,
# but one absolute request deadline starts as soon as the first byte arrives.
mcp_debug_log "READY" "stdio request loop"
while :; do
	_anvil_first_byte=""
	if ! LC_ALL=C IFS= read -r -d '' -n 1 _anvil_first_byte; then
		break
	fi
	_anvil_frame_deadline=$((SECONDS + ANVIL_MCP_FRAME_READ_TIMEOUT))
	if [ -z "$_anvil_first_byte" ]; then
		mcp_debug_log "FRAMING-ERROR" "phase=first-byte byte=nul"
		exit 65
	elif [ "$_anvil_first_byte" = $'\n' ]; then
		line=""
	else
		_anvil_frame_remaining=$((_anvil_frame_deadline - SECONDS))
		[ "$_anvil_frame_remaining" -gt 0 ] || exit 65
		_anvil_line_tail=""
		if ! LC_ALL=C IFS= read -r -t "$_anvil_frame_remaining" _anvil_line_tail; then
			mcp_debug_log "FRAMING-ERROR" "phase=first-line"
			exit 65
		fi
		line="${_anvil_first_byte}${_anvil_line_tail}"
	fi

	# T71: detect framing.  An MCP framed request begins with
	# `Content-Length:' (case-insensitive); legacy line-delimited
	# requests begin with `{'.
	# Strip CR for cross-platform safety.
	_anvil_first_line="${line%$'\r'}"
	_anvil_framed=0
	if [[ "$_anvil_first_line" =~ ^[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]: ]]; then
		_anvil_framed=1
		mcp_debug_log "FRAMING" "Content-Length detected"
		# Re-read full framed message; reuse the already-consumed line.
		if anvil_mcp_read_framed_message \
			"$_anvil_first_line" "$_anvil_frame_deadline"; then
			line=$ANVIL_MCP_FRAME_BODY
		else
			_anvil_frame_rc=$?
			mcp_debug_log "FRAMING-ERROR" \
				"rc=$_anvil_frame_rc first=$_anvil_first_line"
			# The header and some body bytes may already be consumed.  Continuing
			# would reinterpret a delayed remainder as another request.
			exit 65
		fi
	fi

	# Log the incoming request
	mcp_debug_log "REQUEST" "bytes=${#line} head=${line:0:160}"

	# Parse top-level metadata before touching Emacs.  This gives error paths a
	# correct correlation id and lets initialize use its shorter startup cap.
	set +e
	_anvil_metadata=$(anvil_mcp_request_metadata "$line")
	_anvil_metadata_rc=$?
	set -e
	if [ "$_anvil_metadata_rc" -ne 0 ]; then
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false "$_anvil_metadata_rc"
		continue
	fi
	# Split the parser record in one linear builtin pass.  Repeated glob-pattern
	# removal (`${record#*|}') is quadratic for large base64 request fields and
	# can peg Bash forever before the dispatch watchdog has started.  `read'
	# assigns all remaining fields to its final variable, preserving `|' inside
	# a JSON string request id.
	_anvil_request_kind=""
	_anvil_startup=""
	_anvil_request_mode=""
	_anvil_request_payload=""
	_anvil_request_size=""
	_anvil_request_id=""
	IFS='|' read -r \
		_anvil_request_kind _anvil_startup \
		_anvil_request_mode _anvil_request_payload \
		_anvil_request_size \
		_anvil_request_id <<<"$_anvil_metadata"
	case "$_anvil_request_kind" in
	request)
		if [ -z "$_anvil_request_id" ]; then
			anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
				parse false 70
			continue
		fi
		;;
	notification)
		_anvil_request_id=null
		;;
	parse-error|invalid-request)
		anvil_mcp_synthetic_error "$_anvil_request_kind" null \
			"$_anvil_framed" parse false 0
		continue
		;;
	*)
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false 70
		continue
		;;
	esac
	case "$_anvil_request_size" in
	""|*[!0-9]*)
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false 70
		continue
		;;
	esac
	case "$_anvil_request_mode" in
	inline)
		case "$_anvil_request_payload" in
		""|*[!A-Za-z0-9+/=]*)
			anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
				parse false 70
			continue
			;;
		esac
		;;
	file) ;;
	*)
		anvil_mcp_synthetic_error unknown null "$_anvil_framed" \
			parse false 70
		continue
		;;
	esac

	# A harmless readiness probe is the only expression this bridge may replay.
	# Once it succeeds, the JSON-RPC expression below is dispatched exactly once.
	probe_output=""
	set +e
	probe_output=$(anvil_emacsclient_probe_retry -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} -e t)
	_anvil_probe_rc=$?
	set -e
	if [ "$_anvil_probe_rc" -ne 0 ]; then
		anvil_mcp_log_probe_stderr "PROBE-STDERR" "$probe_output"
	fi
	if [ "$_anvil_probe_rc" -ne 0 ]; then
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" readiness false "$_anvil_probe_rc"
		continue
	fi

	# Dispatch the potentially stateful request exactly once.  Initialize has
	# a shorter cap so daemon readiness plus initialization remains inside the
	# client startup envelope; ordinary tools retain the full dispatch budget.
	_anvil_dispatch_timeout="$ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT"
	if [ "$_anvil_startup" = "1" ]; then
		_anvil_dispatch_timeout="$ANVIL_EMACSCLIENT_STARTUP_DISPATCH_TIMEOUT"
	fi
	if ! anvil_mcp_capture_begin; then
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" capture false 74
		continue
	fi

	# Process JSON-RPC and return an ASCII percent-byte wire value.  Inline
	# requests remain below the per-argument exec ceiling.  Large requests are
	# staged only now, after readiness and capture setup, and Emacs removes both
	# the file and its directory even when request processing signals an error.
	case "$_anvil_request_mode" in
	inline)
		base64_input=$_anvil_request_payload
		mcp_debug_log "BASE64-INPUT" "bytes=${#base64_input}"
		elisp_expr="(mapconcat (lambda (byte) (format \"%%%02x\" byte)) (encode-coding-string (or (anvil-server-process-jsonrpc (base64-decode-string \"$base64_input\") \"$SERVER_ID\") \"\") 'utf-8 t) \"\")"
		;;
	file)
		ANVIL_MCP_REQUEST_SEQUENCE=$((ANVIL_MCP_REQUEST_SEQUENCE + 1))
		_anvil_request_basename="request.${ANVIL_MCP_REQUEST_SEQUENCE}.json"
		set +e
		_anvil_request_payload=$(anvil_mcp_stage_request \
			"$line" "$_anvil_request_basename")
		_anvil_stage_rc=$?
		set -e
		case "$_anvil_request_payload" in
		""|*[!A-Za-z0-9+/=]*) _anvil_stage_rc=70 ;;
		esac
		if [ "$_anvil_stage_rc" -ne 0 ]; then
			anvil_mcp_capture_finish
			anvil_mcp_synthetic_error \
				"$_anvil_request_kind" "$_anvil_request_id" \
				"$_anvil_framed" stage false "$_anvil_stage_rc"
			continue
		fi
		mcp_debug_log "STAGED-INPUT" "bytes=$_anvil_request_size"
		elisp_expr="(let* ((anvil-request-file (decode-coding-string (base64-decode-string \"$_anvil_request_payload\") 'utf-8 t)) (anvil-request-directory (file-name-directory anvil-request-file)) anvil-request) (unwind-protect (progn (setq anvil-request (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally anvil-request-file) (unless (= (buffer-size) $_anvil_request_size) (error \"Staged Anvil request size changed\")) (buffer-string))) (delete-file anvil-request-file) (setq anvil-request-file nil) (ignore-errors (delete-directory anvil-request-directory)) (mapconcat (lambda (byte) (format \"%%%02x\" byte)) (encode-coding-string (or (anvil-server-process-jsonrpc anvil-request \"$SERVER_ID\") \"\") 'utf-8 t) \"\")) (when anvil-request-file (ignore-errors (delete-file anvil-request-file))) (ignore-errors (delete-directory anvil-request-directory))))"
		;;
	esac

	# No helper process starts beyond this point: response normalization and
	# decoding use Bash builtins so a delivered stateful response cannot wedge.
	ANVIL_MCP_RESPONSE_PENDING=1
	set +e
	wire_response=$(anvil_emacsclient_dispatch_once \
		"$_anvil_dispatch_timeout" -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
		-e "$elisp_expr")
	_anvil_client_rc=$?
	set -e
	anvil_mcp_capture_finish
	if [ "$_anvil_client_rc" -ne 0 ]; then
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" dispatch true "$_anvil_client_rc"
		continue
	fi

	# Repair Windows MSYS frame-boundary corruption.
	# emacsclient.c uses a read buffer of BUFSIZ+1 bytes; on MSYS / mingw
	# stdio.h, BUFSIZ is 512.  The Emacs server's `server-reply-print'
	# splits its output into frames of up to `server-msg-size' (1024 by
	# default), so on Windows every frame larger than ~512 bytes overruns
	# the client's read buffer.  When that happens, the tail of the frame
	# loses its `-print-nonl ' prefix and emacsclient prints it as
	#   *ERROR*: Unknown message: <tail>
	# interleaved with the legitimate payload.  Bash parameter expansion
	# removes those markers and CR/LF frame boundaries without launching a
	# post-dispatch helper process.
	# (No-op on Linux/macOS where one frame fits in one read.)
	wire_response="${wire_response//\*ERROR\*: Unknown message: /}"
	wire_response="${wire_response//$'\r'/}"
	wire_response="${wire_response//$'\n'/}"

	# Strip the Lisp string quotes after normalizing frame corruption: MSYS
	# may leave a trailing CR after command substitution removes the newline.
	# The wire alphabet contains only `%` and hexadecimal digits, so it needs
	# no quote or backslash unescaping.
	if [[ "$wire_response" == \"* && "$wire_response" == *\" ]]; then
		wire_response="${wire_response:1:${#wire_response}-2}"
	fi

	# Validate the wire before streaming it.  NUL cannot exist in a Bash
	# variable and is never present in valid serialized JSON, so reject %00
	# explicitly instead of silently truncating the response.
	if [[ ! "$wire_response" =~ ^(%[0-9A-Fa-f]{2})*$ ]] \
		|| [[ "$wire_response" == *%00* ]]; then
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" dispatch true 70
		continue
	fi

	if [ -n "$wire_response" ]; then
		anvil_mcp_emit_wire_response "$_anvil_framed" "$wire_response"
	else
		anvil_mcp_synthetic_error \
			"$_anvil_request_kind" "$_anvil_request_id" \
			"$_anvil_framed" dispatch true "$_anvil_client_rc"
	fi
done

# Stop MCP if stop function is provided.  As with init, readiness may
# retry but the potentially stateful stop expression is invoked at most once.
if [ -n "$STOP_FUNCTION" ]; then
	mcp_debug_log "INFO" "Stopping MCP with function: $STOP_FUNCTION"
	mcp_debug_log "STOP-CALL" "readiness probe, then one emacsclient -e ($STOP_FUNCTION)"

	stop_probe_output=""
	set +e
	stop_probe_output=$(anvil_emacsclient_probe_retry -- \
		${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} -e t)
	STOP_READY_RC=$?
	STOP_RC=$STOP_READY_RC
	set -e
	if [ "$STOP_READY_RC" -ne 0 ]; then
		anvil_mcp_log_probe_stderr "STOP-PROBE-STDERR" "$stop_probe_output"
	fi
	mcp_debug_log "STOP-READY-RC" "$STOP_READY_RC"

	if [ "$STOP_READY_RC" -eq 0 ]; then
		if anvil_mcp_capture_begin; then
			ANVIL_MCP_RESPONSE_PENDING=1
			set +e
			anvil_emacsclient_dispatch_once \
				"$ANVIL_EMACSCLIENT_DISPATCH_TIMEOUT" -- \
				${SOCKET_OPTIONS[@]+"${SOCKET_OPTIONS[@]}"} \
				-e "($STOP_FUNCTION)" >/dev/null
			STOP_RC=$?
			set -e
			anvil_mcp_capture_finish
			ANVIL_MCP_RESPONSE_PENDING=0
		else
			STOP_RC=74
			mcp_debug_log "STOP-CAPTURE" "failed before dispatch rc=$STOP_RC"
		fi
	fi
	mcp_debug_log "STOP-RC" "$STOP_RC"
else
	mcp_debug_log "INFO" "Skipping stop function call (none provided)"
fi
