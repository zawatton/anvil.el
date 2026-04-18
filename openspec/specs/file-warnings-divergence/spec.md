# Specification: file-warnings-divergence

## Purpose

Define the observable `:warnings` contract that every mutating
`file-*` MCP tool SHALL honor so that an AI caller can detect when
its disk write may be colliding with a live Emacs buffer's unsaved
state, without the write itself being silently refused.

## Requirements

### Requirement: Every mutating `file-*` tool MUST surface a `:warnings` list in its result

Every tool that writes to disk and is registered under the `file-*`
name family (such as `file-read`, `file-replace-string`,
`file-replace-regexp`, `file-insert-at-line`, `file-delete-lines`,
`file-append`, `file-prepend`, `file-batch`, `file-ensure-import`)
SHALL return a plist that includes a `:warnings` field. The field
SHALL be a list, possibly empty. An empty list indicates that no
disk/buffer divergence was detected for the target file at the time
of the call. A non-empty list indicates detected divergence, with at
least one entry identifying the detected divergence kind (for example
buffer-newer, disk-newer, both-modified, or unknown).

#### Scenario: External edit on an open buffer surfaces a warning

- **GIVEN** a file visited by an Emacs buffer with no unsaved changes
- **AND** the file on disk is modified externally so that its mtime
  is newer than the buffer's recorded modtime
- **WHEN** a client calls any mutating `file-*` tool on that file
- **THEN** the tool's result plist includes a non-empty `:warnings`
  list whose contents name the detected divergence kind (here,
  disk-newer)

### Requirement: A non-empty `:warnings` list MUST NOT cause the tool to refuse the write

The `file-*` tool family SHALL honor a disk-first contract: reporting
a divergence warning SHALL NOT prevent the write from completing. The
disk content after the call SHALL reflect what the tool was asked to
produce, regardless of the warnings. Refusal on divergence is the
responsibility of the separate `buffer-save` tool and is out of scope
here.

#### Scenario: Write proceeds despite divergence warning

- **GIVEN** a file that has divergence between disk and a visited
  buffer (for example both have been modified since the last sync)
- **WHEN** a client calls `file-replace-string` on that file
- **THEN** the disk content reflects the replacement
- **AND** the returned plist still includes a non-empty `:warnings`
  entry describing the divergence

## Non-goals

- The `buffer-*` tool family (`buffer-read`, `buffer-save`,
  `buffer-list-modified`) has its own semantics and is out of scope.
  In particular `buffer-save` MAY refuse to write on `disk-newer` or
  `both-modified`; that behavior is covered by a separate spec.
- This spec does not fix the exact string labels, symbols, or plist
  shape of the individual warning entries — only that the `:warnings`
  field exists, is a list, and is empty iff no divergence was
  detected.
- Non-mutating observability tools (for example `file-outline`,
  `code-extract-pattern`) that do not write to disk are not required
  to surface `:warnings`.
- Detection of divergence on filesystems with very coarse mtime
  resolution (for example FAT 2-second precision) MAY be approximate;
  this spec only requires best-effort detection using the ambient
  `file-attributes` / `visited-file-modtime` signals.
