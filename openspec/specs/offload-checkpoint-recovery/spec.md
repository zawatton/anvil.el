# Specification: offload-checkpoint-recovery

## Purpose

Define what an MCP tool registered with `:offload t` and `:resumable
t` observably returns when it is killed for exceeding its offload
budget, so that long-running scans can surface their partial progress
to the client instead of losing it.

## Requirements

### Requirement: Budget-exceeded resumable tools MUST return a partial result, not an error

When a tool registered with `:offload t :resumable t` runs longer
than its configured offload timeout, the system SHALL kill the
subprocess slot AND return a serialized plist indicating partial
completion — not raise a tool error. The plist SHALL include at
minimum a status marker identifying the result as partial and a
reason identifying budget exhaustion as the cause.

#### Scenario: Long resumable scan is preempted cleanly

- **GIVEN** a tool registered with `:offload t`, `:resumable t`, and a
  short offload timeout (e.g. one second)
- **AND** a handler whose natural completion would take much longer
- **WHEN** the client invokes the tool
- **THEN** the tool response is a plist with a `:status 'partial`
  marker and a reason identifying budget exhaustion
- **AND** the response is **not** a tool error

### Requirement: The last checkpoint MUST be surfaced in the partial result

When a resumable tool's handler has reported progress via
`anvil-preempt-checkpoint` before being killed, the most recently
reported `:value` and `:cursor` SHALL appear in the partial result.
If the handler never reported a checkpoint, the partial result SHALL
still be returned, with `:value` and `:cursor` absent or nil.

#### Scenario: Handler checkpoints are preserved across preemption

- **GIVEN** a resumable offloaded tool whose handler calls
  `anvil-preempt-checkpoint` multiple times during its run with
  increasing `:cursor` values
- **WHEN** the handler is preempted by budget exhaustion after at
  least one checkpoint has been reported
- **THEN** the partial result's `:value` and `:cursor` match the
  **last** checkpoint the handler sent before the kill
- **AND** earlier checkpoints are not visible

## Non-goals

- Non-resumable offloaded tools keep the original error-on-budget
  behavior — unchanged by this spec.
- This spec does not define how the client should **resume** from the
  returned `:cursor`; that is a separate per-tool contract (e.g.
  `org-index-rebuild`'s `:start-from` handling).
- This spec does not constrain checkpoint frequency, cursor shape, or
  the specific data carried in `:value` — those are handler concerns.
- Crash modes other than budget exhaustion (segfault, OOM kill,
  stdin/stdout pipe failure) are out of scope.
