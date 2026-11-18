# Invocation history and remote commands

Use durable invocation records to observe and control a background worker.
Do not restore its checkpoint into the interactive Emacs or pass its plist to
in-process run control functions. For pipeline design or detailed core recovery
semantics, use the companion pipelines skill; this reference covers the job
transport, addresses, receipts, and background lifecycle.

## Inspection APIs

| Call | Contract |
| --- | --- |
| `(gptel-runner-invocation-get id)` | Read the latest manifest from the current job store. |
| `(gptel-runner-invocation-list)` | List newest first, across jobs. Skip unreadable/corrupt manifests with a message. Dead owners of `open`/`closing` invocations are projected as `interrupted` without rewriting history. |
| `(gptel-runner-invocation-live-p invocation)` | Test exact owner identity. Can mean a live launch reservation, not yet an initialized worker. |
| `(gptel-runner-outputs source)` / `(gptel-runner-get source key default)` | Read all named values as an alist, or one value with a missing default; nil values stay nil. Source is a run or invocation record. |
| `(gptel-runner-node-outcome source node)` | Current `:node`, `:state`, and known terminal `:value`; `node-state`, `node-result`, `node-error` also accept invocation records and node objects/IDs. |
| `(gptel-runner-artifacts source)` | Retained descriptors in publication order; source is a run or invocation record. |
| `(gptel-runner-artifact-file source descriptor)` / `(gptel-runner-read-artifact source descriptor)` | Resolve a readable managed path, or read text; same calls in either mode. |
| `(gptel-runner-invocation-call invocation call)` | Expand a compact call descriptor into its full inspection record. Both arguments are records. |
| `(gptel-runner-invocation-commands invocation)` | Read receipts newest first by submission sequence; accepts a record or ID. |
| `(gptel-runner-job-command-status invocation command-id)` | Read one command receipt; accepts a record or ID. |

Filter `invocation-list` by `:job-id` when inspecting one job. Bind
`gptel-runner-job-directory` before ID-based lookup in a non-default store.
Records returned earlier are snapshots; re-read before acting or reporting.

Useful manifest fields:

| Fields | Meaning |
| --- | --- |
| `:id`, `:job-id`, `:job`, `:workflow`, `:trigger` | Invocation identity, originating definition and embedded launch configuration. |
| `:root`, `:directory` | Store and this invocation's directory. |
| `:epoch`, `:run-id` | Worker ownership generation and session-local run ID. Epoch advances on recovery in a new worker. |
| `:phase`, `:worker-state`, `:worker` | Ownership lifecycle; phases are `starting`, `open`, `closing`, `closed`. |
| `:state`, `:reason`, `:error` | Pipeline or worker outcome. Reason/error are not guaranteed for every failure; inspect calls and events too. |
| `:created-at`, `:started-at`, `:finished-at`, `:heartbeat` | Available timing data. A stale timestamp alone is not proof the owner died. |
| `:calls`, `:interactions` | Compact call history and currently pending tool interactions. |
| `:outputs`, `:outputs-file`, `:nodes-file`, `:artifacts` | Named value keys, their data file, node outcomes file, and retained artifact descriptors. |

Run states include running/paused and terminal succeeded/failed/blocked/stalled/
cancelled. Job-layer outcomes also include `skipped` with reason `overlap`,
`startup-error`, `worker-error`, `storage-error`, and `launch-uncertain`.
An `interrupted` listing is a derived inspection state; `invocation-get` can
still return the last stored run state. Check phase and liveness together.

`succeeded` with `:phase closed` means the worker completed its durable close.
It does not alone prove the user's artifact requirements were met. A terminal
state while still open/closing is not permission to remove its worker service.

## Outputs, transcripts, and artifacts

```elisp
(let* ((gptel-runner-job-directory job-store)
       (invocation (gptel-runner-invocation-get invocation-id))
       (call (car (last (plist-get invocation :calls)))))
  (list :outputs (gptel-runner-outputs invocation)
        :call (when call (gptel-runner-invocation-call invocation call))
        :artifacts (gptel-runner-artifacts invocation)))
```

Call descriptors include `:id`, `:epoch`, `:node`, `:agent`, `:state`,
`:generation`, timing, `:attempts`, `:record`, and optional `:transcript`.
The full record adds `:prompt`, `:workspace`, `:result`, and `:error`.
Opaque runtime objects in inspection data can appear as `(:unavailable TYPE)`;
this is not executable state. Older worker epochs remain in call history.

`gptel-runner-invocation-outputs` remains supported for existing clients; prefer
`gptel-runner-outputs` in new code. The four node inspection helpers read exported
outcomes without loading the workflow. Histories created before node exports
report those outcomes unavailable; do not reconstruct them from old calls.

The invocation directory contains `manifest.el`, `checkpoint.el`, `outputs.el`, `nodes.el`,
`calls/`, `events.el`, `commands/`, `receipts/`, and published `artifacts/` as
applicable. Events are an append-only sequence of Lisp forms, not one plist.
launchd workers also have `stdout.log`, `stderr.log`, and registration files;
generic CLI output follows the caller's redirection. Read history as data,
never `load` it. Prefer public readers or dashboard views. For published
artifacts use `gptel-runner-artifact-file`; it validates registration, readability,
and containment. Do not rebuild its path checks in each pipeline.

Publication uses the same core functions locally and in workers:

```elisp
;; In a pipeline step: return this and use :save-as 'report.
(gptel-runner-write-artifact run "report.txt" report-text "text/plain")
;; Copy an existing file when that is the actual source.
(gptel-runner-publish-artifact run "report" report-file "text/plain")
;; In the editor, after refreshing the invocation record:
(gptel-runner-read-artifact invocation (gptel-runner-get invocation 'report))
```

Both publishers return `(:name ... :file ... :media-type ... :epoch ...
:published-at ...)`. Files are retained independently of their sources; names
append distinct versions. The worker supplies invocation storage automatically.
Snapshots preserve the registry across recovery, and new workers publish with
the new epoch. Files are separate from checkpoints and survive run completion.
Use the [shared output contract](../../gptel-runner-pipelines/references/runner-api.md#managed-outputs-in-every-launch-mode)
for storage lifetime and context handoffs. No worker-specific branch belongs in
pipeline output code.

## Submit a command and inspect its receipt

```elisp
(let* ((gptel-runner-job-directory job-store)
       (invocation (gptel-runner-invocation-get invocation-id))
       (receipt (gptel-runner-job-command
                 invocation 'resume '(:feedback "Prefer the smaller change."))))
  ;; Recheck later; the returned receipt only acknowledges submission.
  (gptel-runner-job-command-status invocation (plist-get receipt :id)))
```

`(gptel-runner-job-command invocation operation &optional arguments)` accepts
a record or ID and returns a queued receipt. Arguments are a data-only keyword
plist with no duplicate or unsupported keys. Submission validates the schema;
the worker checks ownership, current call identity, and operation state.
An accepted schema does not guarantee the operation can be applied.

| Receipt `:status` | Meaning |
| --- | --- |
| `queued` | Persisted for the worker, not yet applied. |
| `processing` | Worker began processing; effect is not yet acknowledged. |
| `applied` | Operation returned successfully and checkpoint/history were flushed. The workflow or feedback request may still be running. |
| `rejected` | Rejected before operation dispatch, e.g. stale identity. Inspect `:error`. |
| `indeterminate` | Dispatch began but failed, or an earlier worker died while processing. Some effects may have happened. Inspect history/state before any retry. |

Receipts also carry `:id`, `:operation`, `:sequence`, `:invocation`, `:epoch`,
and eventually `:finished-at` and possibly `:error`. Ordering uses submission
sequence. A storage failure after dispatch can leave `processing`; recovery
marks abandoned processing receipts `indeterminate`. Do not assume an error
means no action occurred or resubmit blindly after a timeout.

`gptel-runner-job-poll-interval` controls the worker's command and external-lock
checks (default 0.2 seconds). Controllers should use bounded, spaced receipt
checks that let Emacs process events; timeout is not a failed-command receipt.

## Operation arguments

In the table, **budgets** means optional `:additional-requests`,
`:additional-calls`, and `:additional-duration`. Counts must be positive integers;
duration is a positive number of seconds. **Call address** means required
`:call-id` (nonempty string) and `:call-epoch` (positive integer copied from the
observed call). Node IDs are symbols, strings, or integers, as used by the workflow.

| Operation | Accepted arguments | Effect / prerequisite |
| --- | --- | --- |
| `pause` | None | Pause the whole run; stop active work and preserve completed results. |
| `resume` | Optional `:feedback` string | Resume a paused run; feedback reaches the next unfinished prompt. |
| `cancel` | None | Abort the run. Disabling its schedule is separate. |
| `retry` | Optional `:from-node`, `:feedback` | Retry at the safe checkpoint, or from an executed agent or Elisp node. Select by node ID, not a historical call ID. Does not change the goal. |
| `continue` | Required nonempty `:observation`; optional `:reset-budget` t/nil and budgets | Continue a terminal run with the observation as a new goal. Success starts a new cycle; an unsuccessful run retains completed stages and restarts its checkpoint. |
| `extend` | Budgets; provide at least one | Increase finite budgets of running/paused work, or restart budget-failed work after extending every exhausted limit. |
| `extend-repeat` | Required `:node`, `:additional-iterations`; optional budgets | Restart a failed run whose named repeat exhausted its iteration limit. Does not change the saved workflow. |
| `decision` | Required nonempty `:text`; optional `:reason` string | Record an accepted decision and rationale for future prompts. |
| `pause-call` | Call address | Suspend one active call for human feedback. |
| `cancel-call` | Call address | Cancel the selected call; enclosing workflow policy determines propagation. |
| `feedback` | Call address and required `:feedback` string | Send a new request in the live gptel conversation of a paused/waiting-feedback call. Nonblank text and a retained buffer are required. |
| `accept` | Call address; optional `:result` string | Accept a paused/waiting-feedback call. Omission/nil uses its latest response. Wait for active feedback to finish; a nonblank result must be available. |
| `approve`, `reject` | Call address, required `:interaction-id`; optional `:generation` | Answer exactly the pending tool request. Include its observed generation. Rejection cancels that call. |

Optional feedback/result/reason fields accept nil where the operation permits
omission. `:generation` is a nonnegative integer. `:additional-iterations` is a
positive integer. There is no remote arbitrary-eval command or callback argument.

Budget changes, continuation, retry, resume, feedback, and approval can dispatch
work. Apply the user's existing execution authority and intended budget.
Continuation preserves consumed budgets unless explicitly reset; extending an
unlimited budget leaves it unlimited. Changing a goal does not automatically
invalidate every earlier result on an unsuccessful run.

## Address the observed call or interaction

Calls from different worker epochs can share a call ID. Use the epoch from the
call you inspected, never replace an old call's epoch with the invocation's newest
epoch to make an action pass. For example, after selecting and inspecting a current
waiting call:

```elisp
(gptel-runner-job-command
 invocation 'accept
 (list :call-id (plist-get selected-call :id)
       :call-epoch (plist-get selected-call :epoch)
       :result accepted-text))
```

For a pending tool interaction selected from fresh `:interactions`, inspect its
actual tool request before answering it. Entries include `:source-buffer` and
`:tools`; approvals from subagents/discussions share the outer runner `:call-id`
but have distinct interaction IDs. Several can be pending at once. Select the
specific source/tool request, not merely the first entry for a waiting node:

```elisp
(gptel-runner-job-command
 invocation 'approve
 (list :call-id (plist-get interaction :call-id)
       :call-epoch (plist-get interaction :epoch)
       :interaction-id (plist-get interaction :id)
       :generation (plist-get interaction :generation)))
```

Interaction ID, worker epoch, and call generation identify different things.
A stale approval cannot authorize a subsequent tool request. An accepted child
approval runs in that child's request context; its siblings may still await
approval. Rejecting any pending interaction cancels the owning runner call and
its descendants. Closed/cancelled children lose their approvals even if the
outer call continues. Re-read `:interactions` after commands or lifecycle changes.
`feedback` produces another response that still needs `accept`; `resume` restarts
unfinished workflow work and is not acceptance of a paused call.

## Worker recovery

Workers stay alive while paused or waiting for feedback/approval. Pausing the
whole run stops its active-duration clock; waiting on a call or tool approval
continues consuming the duration limit. Closing a dashboard does not stop work
or answer a gate.

If the owner is gone, only `resume`, `retry`, `continue`, `extend`, `extend-repeat`,
and `cancel` can launch recovery, and a readable checkpoint must exist. These
commands claim the job lock, advance the invocation epoch, rerun the embedded
job's bootstrap, and restore inside a new worker under the same invocation ID.
The core operation's state requirements still apply. Another active invocation
of the job prevents recovery. Recovery uses the configured launcher; a generic
CLI initial run alone does not configure a portable detached recovery launcher.

Live callbacks, buffers, and pending tool interactions do not survive a crash.
This includes nested agent/discussion requests and their supervision handles;
unfinished pipeline nodes restart rather than resuming those child requests.
Use `resume` for interrupted work restored as paused, `retry` for a retryable
failure, and the appropriate extension for budget exhaustion. Refresh history
after recovery; old call addresses and approvals are invalid. A startup failure
without a checkpoint needs repaired initialization and a new invocation.

`launch-uncertain` means startup cancellation could not be confirmed. Its
reservation remains held deliberately. Use launchd diagnostics/reaping to
confirm the exact worker service is absent; do not delete locks or create a
duplicate job to bypass ownership.

## Dashboard access

`M-x gptel-runner-show-dashboard` combines local and external history. Background
actions operate on the selected external row. The `gptel-runner-background-*`
inspection commands are `inspect`, `outputs`, `commands`, `transcript`, `events`,
and `artifact`. They read durable data without adopting the run.

`gptel-runner-background-command` offers all operations above. Convenience
commands with the same prefix include `pause`, `resume`, `cancel`, `retry`,
`continue`, `extend`, `feedback`, `accept`, `approve`, and `reject`. Call actions
require the selected call; approval displays and addresses a pending interaction.
For automation, prefer `gptel-runner-job-command` with explicit data and receipts.
