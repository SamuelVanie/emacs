# Running and managing pipelines

Contents: [load/preflight](#load-and-preflight), [start](#start-and-retain-the-run),
[inspect](#inspect-and-determine-success), [choose an operation](#choose-the-operation),
[gates](#human-gates-and-pausing), [recovery](#retry-and-changed-goals),
[budgets](#budget-and-repeat-extension), [persistence](#save-and-restore),
[cleanup](#cancellation-and-cleanup).

## Load and preflight

1. Use the intended Emacs session. `(locate-library "gptel-runner")` identifies
   the discoverable library; `(symbol-file 'gptel-runner-start 'defun)` identifies
   the loaded definition. Changing `load-path` does not replace loaded Lisp.
2. Inspect the pipeline file before loading it: registration-only definitions are
   safe to load within setup scope, but top-level start forms launch work. Load
   gptel and needed tool packages, selected preset definitions, then the runner,
   helper functions, agent registrations, and the named workflow. Use the user's
   established package setup; do not reload the whole init file. For example,
   evaluate `(require 'gptel-runner)` and `(load pipeline-file nil t)` after setting
   `pipeline-file` to the inspected definition file. `require` does not discover
   or load personal presets. See [presets.md](presets.md) for runtime discovery.
3. Confirm the selected workflow, node → agent → preset mapping, output parsers,
   tool registrations, model/backend, completion mode, and confirmation behavior.
   Handoff nodes need a tool-capable model. The dashboard lists registered
   workflows, including those with no runs. The bundled starter's
   `grp-*` preset values must be replaced with compatible loaded presets before
   provider execution; do not mistake successful registration for preset validation.
4. Verify the current goal, canonical brief, acceptance checks, decision authority,
   existing artifacts, and workspaces. All step overrides must exist. Relative
   context files resolve in each receiving step's workspace and are checked even
   on later/unselected branches. Use inline text or absolute descriptors for a
   shared cross-repository brief. Check writer overlap and external effects; the
   runner's writer lock is limited to equal canonical directories within one run.
5. Confirm finite call/request/active-duration limits, concurrency, repeat bounds,
   intended gates, and the existing authorization for writes and tool execution.
   `:allow-writes t` must be supplied explicitly to start writable workflows.
   By default, pass `:allow-unconfirmed-tools t` explicitly in start
   arguments; workflow defaults do not supply these flags. Retain approval
   when explicitly requested. Separate access controls and tool hooks can still
   require approval, so test worker behavior. Neither flag restricts the filesystem,
   and `isolated` does not provision a worktree.
6. A durable run requires a named workflow and readable Lisp data. Generated
   node IDs are supported when the restored workflow layout matches. Optional
   explicit IDs help you identify nodes. Keep the definition, named context
   functions, and dependencies available for restoration. Inspect retained runs or
   snapshots before starting another copy of work already in progress.

There is no public dry-run/validate-only command. `gptel-runner-start`
validates definitions and dispatches immediately. Use an isolated batch Emacs and
fake driver for routing checks, not a live start to test preflight. A fake driver
does not validate actual presets, credentials, tool capabilities, or artifacts.

## Start and retain the run

`gptel-runner-start` takes a workflow symbol, workflow object, or anonymous root
node plus keywords `:goal`, `:workspace`, `:inputs`, `:driver`, `:max-calls`, `:max-requests`,
`:max-duration` (seconds), `:max-concurrency`, `:context`, `:decision-memory`,
`:persist`, `:completion` (default `'handoff`), `:allow-writes`, `:allow-unconfirmed-tools`, and `:callback`.
Unknown keywords may be silently ignored; use the supported options above.
Anonymous roots cannot be durably paused or saved. Omitted budget limits inherit
workflow defaults; otherwise nil means unlimited. Omitted workspace inherits the
workflow's `:workspace` default, then the calling buffer's `default-directory`.
Explicit nil uses `default-directory`. Check the selected directory exists.
`:inputs` initializes blackboard values before dispatch; see the
[input/default precedence contract](runner-api.md#starting-inputs-and-workspace-defaults).

For an adapted [starter](../assets/intent-preserving-pipeline.el), first set
`my-project-root` to the existing project directory and `my-brief` to the agreed
brief string or supported context sources. Select `my-driver` explicitly:
`(gptel-runner-gptel-driver-create)` for authorized live work, or a scripted
`(gptel-runner-fake-driver-create)` for a network-free check. This avoids accidentally
using a fake default in a live run. The following numbers are examples to size
against the actual task, not permission to increase an existing allowance.

```elisp
(defun my/pipeline-finished (run)
  (message "Pipeline %s: %s; inspect results and artifacts"
           (gptel-runner-run-id run) (gptel-runner-run-state run)))

(setq my-run
      (gptel-runner-start
       'grp-intent-preserving
       :goal "Add resumable import; preserve the current CLI"
       :workspace my-project-root :context my-brief :driver my-driver
       :max-calls 14 :max-requests 60 :max-duration 1800 :max-concurrency 1
       :allow-writes t :allow-unconfirmed-tools t
       :decision-memory t :persist t :callback #'my/pipeline-finished))
(setq my-run-id (gptel-runner-run-id my-run))
```

Evaluate Lisp with the available Emacs evaluation tool, `M-:`, or a deliberately
invoked start file. `gptel-runner-start` is not an `M-x` command. Do not append a
live start to a file meant only to register definitions. Supply initial narrative
context with `:context` and structured values with `:inputs` at start: putting
them on the blackboard afterward can miss the first dispatched call.
Ordinary prompts still need to include the initial goal explicitly; the starter's
shared context does this.

Live execution is asynchronous; return of `start` does not mean completion. A fake
driver or immediate failure may finish before `start` returns, so callbacks must
use their `run` argument, not assume `my-run` has already been assigned. Keep the
event loop responsive; a busy wait blocks callbacks/timers. The completion callback
runs once for its terminal transition. Pause is not terminal. After a previously
reported terminal outcome, pass a fresh `:callback` to retry/rerun/continue/extension
if another terminal notification is required; the previously invoked callback
is not automatically invoked again. Resume takes its callback positionally.

Run IDs are session-local. `(gptel-runner-find-run my-run-id)` retrieves a retained
run, and `(gptel-runner-list-runs)` lists newest first. After restart, load its
snapshot before relying on its ID. Prefer run objects: resume, pause, abort, save,
and inspection accessors do not generally resolve ID strings. Retry, rerun, continue,
both extension APIs, record-decision, and forget-run do accept object or ID.

## Inspect and determine success

Open `M-x gptel-runner-show-dashboard`. Select an agent node and press `RET`
to open its editable chat. On a run row `RET` opens its journal. Use `? m` for
handoffs, questions, and replies, or `? j` for child discussions and delegated
worker transcripts. Selecting a node filters the view to that node. Use `? e`
for events and `? v` to visit the selected node's worker chat. Press `g` to
refresh and `q` to quit. The action menu enables controls for the selected row.

For a background invocation, `RET` opens a published local chat copy. Send
feedback to the worker from its waiting node row with `? f`. The
[jobs skill](../../gptel-runner-jobs/SKILL.md) covers remote controls. A local
run ID alone does not identify a background invocation.

For a compact status read (the `grp-*` keys/ID belong to the starter):

```elisp
(let ((budget (gptel-runner-run-budget my-run)))
  (list :id (gptel-runner-run-id my-run)
        :state (gptel-runner-run-state my-run)
        :goal (gptel-runner-run-goal my-run)
        :workspace (gptel-runner-run-workspace my-run)
        :calls (list (gptel-runner-budget-calls budget)
                     (gptel-runner-budget-max-calls budget))
        :requests (list (gptel-runner-budget-requests budget)
                        (gptel-runner-budget-max-requests budget))
        :rounds (gptel-runner-iteration my-run 'grp-cycle)
        :failure (gptel-runner-run-terminal-data my-run)
        :snapshot-status (gptel-runner-store-save-status my-run)))
```

Inspect only the evidence needed for the next decision:

| Evidence | Read API |
| --- | --- |
| Current plan, result, loop history | `gptel-runner-get` with `grp-plan`, `grp-implementation`, `grp-review`, `grp-history` (or the workflow's actual keys) |
| All named values | `(gptel-runner-outputs my-run)`; prefer selected keys for downstream prompts |
| Retained reports | `(gptel-runner-artifacts my-run)`; resolve/read with `gptel-runner-artifact-file` / `gptel-runner-read-artifact` |
| Decision log and previous goals/results | `(gptel-runner-decisions my-run)`, `(gptel-runner-continuations my-run)` |
| Current node state and accepted outcome, including after recovery | `(gptel-runner-node-outcome my-run 'grp-review)`; see [node APIs](runner-api.md#node-traversal-and-outcomes) |
| Historical/current calls | `(gptel-runner-run-calls my-run)`; select by `gptel-runner-call-id` and `gptel-runner-node-id` of `gptel-runner-call-node` |
| Selected call details | `gptel-runner-call-state`, `-workspace`, `-prompt`, `-result`, `-error`, `-request-attempt`, `-buffer` accessors |
| Failure chronology | `(gptel-runner-run-events my-run)`; select recent relevant events using `gptel-runner-event-type`, `-node-id`, `-call-id`, `-data` |

The named-value, node outcome/state/result/error, and artifact readers also
accept a refreshed background invocation record. They do not restore executable
state. Local lifecycle controls below still require the owning run; use job
commands for remote mutations. Do not scan calls or raw state tables to recreate
the current node's outcome.

Calls/events are in chronological order. Historical failed calls can remain after
successful recovery. Consult the current node/run state, latest relevant call,
round history, and input versions together. A displayed call may be `succeeded`
while the run is `blocked` because its valid review triggered `:stop-when`.
`waiting-confirmation` needs tool approval; `waiting-tool` means tool execution
(possibly waiting for user input). In `waiting-feedback`, check whether an
agent ended its turn in prose or a proposed result is awaiting approval. The
node chat retains the conversation for subsequent turns, including tools and
feedback. The accepted node outcome and recorded decisions change through
runner operations.

Prefer a callback or a filtered `gptel-runner-event-hook` (each function receives
one event) for ongoing monitoring. If polling is needed, use a bounded observation
window and spaced compact reads, for example every 5–15 seconds. Stop and surface
the reason at terminal state, human gate, tool approval, or an unresolved blocker;
do not turn unchanged status into retries or extend monitoring indefinitely.

Report success only when the run is `succeeded` **and** current output/evidence
satisfies the brief. Check required branch outcomes (a `collect` join tolerates
failures), current-round validated reviews, actual files in the effective writer
workspace, and recorded verification. An old passing value, nonempty report,
successful provider response, or stopped stream is insufficient. Say what remains
unverified; fake-driver success establishes control flow only.

## Choose the operation

Use the current state and the cause of interruption. In the dashboard shortcuts
below, the key follows `?`; it is not a permanent direct binding. Except for
recording decisions, interactive wrappers use the selected dashboard row.

| Need / eligible state | Lisp API | Dashboard / effect |
| --- | --- | --- |
| Planned acceptance before advancing | Step `:pause-after t` | The submitted handoff waits for human approval before parsing and saving |
| Correct one unfinished call now | `(gptel-runner-pause-call call &optional reason)` | `p`, `gptel-runner-dashboard-pause-call`; retain live continuation |
| Send feedback to a waiting gptel call | `(gptel-runner-gptel-feedback call text)` | `f` sends a turn. An open discussion can hand off. A human approval gate waits for acceptance. |
| Accept human-reviewed call output | `(gptel-runner-complete-call-from-buffer &optional result call)` | `a`, `gptel-runner-dashboard-complete-call`; parser/validator still apply |
| Stop all work for a longer wait | `(gptel-runner-pause-run run &optional reason)` | `P`, `gptel-runner-dashboard-pause-run`; named workflow, stop duration clock, queue snapshot |
| Restart a paused run | `(gptel-runner-resume-run run &optional feedback callback)` | `r`, `gptel-runner-dashboard-resume-run`; completed nodes kept, unfinished work restarts |
| Retry same goal after failed/blocked/stalled/cancelled outcome | `(gptel-runner-retry run &key from-node feedback callback)` | `t`, `gptel-runner-dashboard-retry-run`; safe checkpoint |
| Rerun an executed agent node in an unsuccessful run | Same retry API with `:from-node 'node-id` | `T`, `gptel-runner-dashboard-retry-node` on a call row; invalidate node and dependent sequence work |
| Rerun the entire finished workflow with its current goal | `(gptel-runner-rerun run &key reset-budget callback)` | `R` starts at the root, retains chats and inputs, and resets consumed budget by default |
| New observation/goal after terminal outcome | `(gptel-runner-continue run observation &key reset-budget additional-requests additional-calls additional-duration callback)` | `c`, `gptel-runner-dashboard-continue-run`; new cycle after success, checkpoint after other outcomes |
| Add run capacity while running/paused, or recover run-budget failure | `(gptel-runner-extend run &key additional-requests additional-calls additional-duration callback)` | `b`, `gptel-runner-dashboard-extend-run`; cumulative use retained |
| Recover an exhausted repeat in a failed run | `(gptel-runner-extend-repeat run node-id additional-iterations &key additional-requests additional-calls additional-duration callback)` | Evaluate Lisp; no dedicated repeat-extension menu command |
| Cancel one call / whole run | `(gptel-runner-abort-call call &optional reason)` / `(gptel-runner-abort-run run &optional reason)` | `k` / `K`, `gptel-runner-dashboard-abort-call` / `-abort-run` |
| Save without pausing / load without dispatch | `(gptel-runner-save-run run &optional file)` / `(gptel-runner-load-run file &optional callback driver)` | `s` / `l`, `gptel-runner-dashboard-save-run` / `-load-snapshot` |
| Remove retained terminal/paused run | `(gptel-runner-forget-run run &optional delete-snapshot)` | `x`, `gptel-runner-dashboard-forget-run`; check completed save first |

These are argument signatures, not forms to paste with literal `&key`/`&optional`.
Core control APIs other than pause-call/pause-run are generally Lisp functions;
use the named dashboard commands for `M-x` interaction.

## Human gates and pausing

A handoff-mode agent that ends its turn with prose parks in `waiting-feedback`.
Use ordinary gptel send in the node chat, `? f` on its dashboard row, or
`(gptel-runner-gptel-feedback call text)` to add a turn. This open discussion
reenters the regular dispatch queue, uses request budget, and can ask a previous
agent or hand off. Use `? a` to accept an answer directly.

For a planned human review put `:pause-after t` on the step. Even a tool-submitted
handoff waits before parsing, validation, saving, and downstream dispatch.
For a correction during active work use `M-x gptel-runner-pause-call` from the
chat or `? p` from its row. The runner stops the provider and leaves the call
available for review. Send a revision request with `? f` or from Lisp:

```elisp
(gptel-runner-gptel-feedback selected-call
                            "Check cancellation as well as successful completion.")
;; After inspecting the revised answer, release the review gate.
(gptel-runner-complete-call-from-buffer nil selected-call)
```

At a human review gate, feedback requires a free workspace/concurrency slot.
It runs asynchronously outside ordinary dispatch counters, then waits again
for acceptance. Inspect the revised answer or any `feedback-failed` event.
Acceptance is disabled while a feedback request is active. From the chat use
`M-x gptel-runner-complete-call-from-buffer` to submit the active region or
the latest response. With a proposed handoff and no newer response, acceptance
uses the submitted handoff. From the dashboard `? a` accepts the latest answer.
From Lisp, pass explicit text as the first argument. Parser and validator still
apply. Use `? d` to record an approved enduring decision.

A call pause or review gate leaves other branches free to run and the duration
clock ticking. Pause an entire named run with `? P` to stop its clock and queue
a checkpoint. After opening a saved run, a waiting node can receive feedback
or acceptance in its recovered chat. Accept saved parallel gates individually.
Choosing `? r` on the run restarts unfinished work from its safe checkpoint in
the existing node conversations. A prior provider request cannot be resumed.
See [save and restore](#save-and-restore) for older snapshot formats.

## Retry and changed goals

First fix the cause and check whether artifacts/inputs are still valid. For a
blocked review whose implementation remains valid, this example retries only the
starter's reviewer and its dependent work:

```elisp
(gptel-runner-retry
 my-run :from-node 'grp-review
 :feedback "The missing test dependency is now available; rerun the checks."
 :callback #'my/pipeline-finished)
```

Plain retry keeps the goal, completed prerequisites, saved results, and cumulative
budget usage. A completed body that blocked or stalled a repeat restarts as a whole,
including the implementer. `:from-node` must name an **executed agent step ID**, not
an agent name, repeat/container, skipped branch, or call ID. It clears that node's
saved result and later sequence results; earlier results and independent successful
parallel siblings survive. Enclosing repeat history/counters survive, and completing
the retried body consumes another iteration. Downstream subtrees that are invalidated
restart their own loop state/results. Selecting an old call does not rewind to its
historical iteration. Check remaining capacity before any retry.

Retry `:feedback` reaches the selected node's next prepared prompt, or the next
prepared prompt when no node was specified. Resume feedback likewise reaches only
the first unfinished prompt. Neither becomes a durable shared instruction by itself.
Record an authorized lasting correction **before** releasing work:

```elisp
(gptel-runner-record-decision
 my-run "D2 supersedes D1: preserve the existing import option names."
 "User correction: compatibility with existing scripts remains required.")
```

Use task-specific text, authority, and rationale; the above is a format example.
Update affected brief/plan artifacts too. File/function context is re-resolved for
new calls; putting text under an arbitrary blackboard key only helps if a context
or prompt function reads it. No new decision changes already prepared/running calls.
If conflicting work is still active, stop or finish it deliberately before dispatching
dependent work. Keep workspace/revision identity attached to all retained evidence.

Use `(gptel-runner-rerun my-run)` or `? R` on any terminal run to execute
from the first node with the same goal, inputs, recorded decisions, and chats.
It archives named outputs and individual node outcomes in
`(gptel-runner-continuations my-run)`. The default resets consumed budget under
the configured limits. Pass `:reset-budget nil` to share the remaining allowance.

For a terminal run, a new observation can become the goal:

```elisp
(gptel-runner-continue
 my-run "Preserve resumable import and the CLI; name invalid options in errors."
 :callback #'my/pipeline-finished)
```

After success, continue archives workflow-owned outputs, clears those outputs,
resets all nodes and repeat counters/progress, and starts the whole workflow again.
The old repeat history is in the continuation archive; a new cycle builds new history.
Run-specific repeat limits remain. After failure/block/stall/cancellation, continue
archives outputs but resumes at the safe checkpoint, retaining completed predecessors
and their results. It does **not** automatically redo a stale plan. A changed goal
that invalidates that plan needs a fresh run with the corrected brief or an explicit
replan path before implementation; do not call continue and hope to repair context
after it has already dispatched. Re-registering a workflow does not retrofit its
definition into an existing run.

The observation replaces `gptel-runner-run-goal`; previous goal and observation
are also injected into subsequent ordinary prompts. Keep unchanged requirements
in the canonical brief. Calls, events, decisions, manually stored context, and
`gptel-runner-continuations` remain. Budgets stay cumulative unless explicitly
reset or increased. `:reset-budget t` resets consumed calls/requests/duration while
retaining limits; it is a new allowance, not free recovery. Continue does not
increase an exhausted repeat limit. Do not use a new run or reset to hide exhaustion.

## Budget and repeat extension

Use positive additions; calls/requests/iterations require integers, duration is
seconds. Nil limits stay unlimited. For authorized additional capacity:

```elisp
(gptel-runner-extend my-run
 :additional-calls 4 :additional-requests 8 :additional-duration 300)
```

While running/paused, this updates limits in place without resetting work or
resuming a paused run. A duration extension replaces the active deadline timer
while retaining consumed time. After run-budget failure, extend restarts from
the checkpoint; increase **every** exhausted finite budget. It does not fix a
parser/provider failure or repeat exhaustion. Inspect terminal data and consumed
limits instead of repeatedly issuing retry.

For a failed repeat that reached its effective `:max`:

```elisp
(gptel-runner-extend-repeat my-run 'grp-cycle 1
 :additional-calls 2 :additional-requests 8
 :callback #'my/pipeline-finished)
```

This adds iterations to this run only, resets the repeat body, preserves existing
history and completed iteration numbering, and leaves prior usage charged. Add
enough run-level headroom if needed. A stalled/blocked repeat with capacity left
needs cause-aware retry, not iteration extension. Existing blackboard results may
remain until replaced, so conditional producers still need freshness checks.

Normally retry rejects exhausted limits. There is an important exception: after
work launched by an extension fails, retry can **reapply the stored extension**
to newly exhausted run/repeat limits. Inspect `gptel-runner-run-extension-context`
and the `run-retried` event's `:reapplied-extension` / `:reapplied-repeat-extension`.
Such a retry can grant more capacity; do not silently repeat it beyond the authorized
allowance. State the cause, remaining work, and proposed increment when more authority
is needed. Budget limits are not token/cost caps.

## Save and restore

Use `M-x gptel-runner-save-session` for an explicit runner session save and
`M-x gptel-runner-open-session` to select a saved session in the dashboard
without a provider request. Opening a file already attached to a local run
selects that run. Use `:persist t` for ongoing checkpoints. `(gptel-runner-save-run run)` and
whole-run pause also enable persistence on a named run. A manual save does not
pause work. Default snapshots live under `gptel-runner-snapshot-directory`, normally
`~/.emacs.d/gptel-runner/snapshots/`; `(gptel-runner-list-snapshots)` returns paths
newest first. Use the actual `gptel-runner-run-snapshot-file`, not a guessed filename.

```elisp
(setq my-snapshot (gptel-runner-save-run my-run))
(gptel-runner-store-save-status my-run)
```

Save returns a path **before** serialization and disk I/O complete. Automatic
checkpoints are coalesced after a quiet delay (five seconds by default); manual
save/pause/terminal checkpoints bypass the delay, but still write asynchronously.
Status is `pending`, `writing`, `clean`, or `error`. Observe `snapshot-saved` or
`snapshot-error` events and use bounded status checks while yielding to Emacs's
event loop. After this requested save, require `clean` plus the expected readable
file before claiming durability. `clean` alone on a run never saved means only
that nothing is pending. A file's existence alone may reflect an older snapshot.
Pause first for a settled checkpoint before shutdown or removal. Normal Emacs
shutdown flushes pending writes; an abnormal exit can lose queued progress.

After restart, load the same presets/tools, helpers, agents, workflow definition,
and driver before loading the snapshot as data. Reload all per-step workspace
overrides; they come from the workflow definition, not embedded workflow code.
Check the actual repositories/revisions and required context files still exist;
restore revalidates directories and declared context files, but does not freeze
external artifacts or verify that a changed definition has equivalent meaning.

```elisp
(setq my-run (gptel-runner-load-run
              my-snapshot #'my/pipeline-finished my-driver))
(setq my-run-id (gptel-runner-run-id my-run))
;; Inspect state, decisions, inputs, workspace and budgets before dispatch.
;; Only if the loaded state is paused and the retained plan remains valid:
(gptel-runner-resume-run my-run "Recheck the current artifacts before continuing.")
```

Loading never starts work. A nonterminal saved state loads as `paused`. A
terminal state stays terminal. Choose rerun, retry, continue, or extension
according to the outcome and which work should execute again.
Loading an ID already retained in the session is rejected; use the existing run
instead of creating a duplicate. `gptel-runner-resume-snapshot` has signature
`(file &optional feedback callback driver)` and combines load/resume immediately;
prefer separate calls when inspection is needed, and do not use it for terminal saves.

Version 4 saves workflow checkpoints, budgets, accepted outcomes, blackboard
data, and the managed output registry together with each node's chat and drafts,
child discussions, delegated agent transcripts, and inter-node messages. The
outstanding question's return path is saved so an upstream reply can reach its
requester after recovery. Artifacts are separate files in the retained output
directory. Supply a new callback/driver where needed. Old provider requests,
tool approvals, timers, and live child workers end with their Emacs process.

The runner exclusively owns these chats even with global
`gptel-agent-session-mode` enabled and even before a run is saved. Use runner
sessions for recovery. A compatible gptel-agent preserves native assistant/tool
boundaries and chat settings through its serializer. Plain gptel uses its own
chat format. Version 3 includes node chats, version 2 includes workflow
checkpoints, and captured child transcripts require version 4. Older sessions
that predate handoff completion reopen in response mode. Version 1 is unsupported.
Keep saved outcomes and context readable as Lisp data. A mode-`0600` snapshot
may contain prompts, tool outputs, and drafts. A background invocation's
published inspection history is separate from its recoverable runner session.

## Cancellation and cleanup

`gptel-runner-abort-call` terminally cancels one call and propagates through its
parent policy; an optional branch's cancellation need not cancel the whole run.
`gptel-runner-abort-run` normally ends the run as `cancelled`; explicit
[run finalizers](runner-api.md#bounded-run-finalization) control finalization
after cancellation and retain the body's outcome.
Neither reverses completed file writes, external requests, or already launched
tool effects. Inspect those effects before retrying; cancellation is not rollback.

Before forgetting a run, retain needed artifact evidence and ensure the desired
snapshot save has completed. **Forget cancels pending snapshot writes** as well
as killing retained worker/event buffers and removing the session registry entry.
Terminal or paused runs can be forgotten; active runs are protected. Snapshots
are kept by default; the optional non-nil delete argument removes them too.
Deleting snapshots or evidence needs the corresponding user intent.

```elisp
(gptel-runner-forget-run my-run) ; After evidence retention and a completed save.
```

`gptel-runner-forget-workflow (name &optional delete-snapshots)` removes its
registration and retained runs; it rejects groups with active runs.
`gptel-runner-unregister-workflow (name)` removes only the registration, leaving
retained runs. Dashboard `? X` invokes `gptel-runner-dashboard-forget-workflow`;
`? C` invokes `gptel-runner-dashboard-clear-finished` for all terminal runs (not
paused ones). A prefix argument to these cleanup commands or `? x` also deletes
snapshots. Cleanup does not remove managed output directories, workspace artifacts,
or the definition file. Retained files require separate intentional cleanup.
