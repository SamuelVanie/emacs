# Running and managing pipelines

Verified against gptel-runner `51570dc` on 2026-10-30. Recheck the loaded source
when versions differ. This reference operates the definitions described in
[runner-api.md](runner-api.md); it does not grant extra execution authority.

Contents: [load/preflight](#load-and-preflight), [start](#start-and-retain-the-run),
[inspect](#inspect-and-determine-success), [choose an operation](#choose-the-operation),
[gates](#human-gates-and-pausing), [recovery](#retry-and-changed-goals),
[budgets](#budget-and-repeat-extension), [persistence](#save-and-restore),
[cleanup](#cancellation-and-cleanup).

## Load and preflight

1. Identify the intended Emacs session and code version. `locate-library` reports
   the discoverable file; `(symbol-file 'gptel-runner-start 'defun)` reports the
   loaded definition. A changed `load-path` does not replace already loaded Lisp.
   Inspect the local README and implementation rather than hot-reloading a live
   scheduler merely to match a checkout.
2. Inspect the pipeline file before loading it: registration-only definitions are
   safe to load within setup scope, but top-level start forms launch work. Load
   gptel and needed tool packages, selected preset definitions, then the runner,
   helper functions, agent registrations, and the named workflow. Use the user's
   established package setup; do not reload the whole init file. For example,
   evaluate `(require 'gptel-runner)` and `(load pipeline-file nil t)` after setting
   `pipeline-file` to the inspected definition file. `require` does not discover
   or load personal presets. See [presets.md](presets.md) for runtime discovery.
3. Confirm the selected workflow, node → agent → preset mapping, output parsers,
   tool registrations, model/backend, and confirmation behavior. The dashboard
   lists registered workflows, including those with no runs. The bundled starter's
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
   `:allow-writes t` must be supplied explicitly to start writable workflows;
   it is not inherited from workflow defaults. Keep `:allow-unconfirmed-tools nil`
   unless the user authorized disabling confirmation. Neither flag restricts the
   filesystem by itself, and `isolated` does not provision a worktree.
6. For durable work use a named workflow, stable explicit IDs, readable Lisp
   blackboard/context data, and named context functions. Keep its definition file
   and dependencies available for restoration. Inspect existing retained runs or
   snapshots before starting another copy of work already in progress.

There is no public dry-run/validate-only command in this version. `gptel-runner-start`
validates definitions and dispatches immediately. Use an isolated batch Emacs and
fake driver for routing checks, not a live start to test preflight. A fake driver
does not validate actual presets, credentials, tool capabilities, or artifacts.

## Start and retain the run

`gptel-runner-start` takes a workflow symbol, workflow object, or anonymous root
node plus keywords `:goal`, `:workspace`, `:driver`, `:max-calls`, `:max-requests`,
`:max-duration` (seconds), `:max-concurrency`, `:context`, `:decision-memory`,
`:persist`, `:allow-writes`, `:allow-unconfirmed-tools`, and `:callback`.
Anonymous roots cannot be durably paused or saved. Omitted budget limits inherit
workflow defaults; otherwise nil means unlimited. Omitted workspace uses the
calling buffer's `default-directory`, so supply the intended directory explicitly.

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
       :allow-writes t :allow-unconfirmed-tools nil
       :decision-memory t :persist t :callback #'my/pipeline-finished))
(setq my-run-id (gptel-runner-run-id my-run))
```

Evaluate Lisp with the available Emacs evaluation tool, `M-:`, or a deliberately
invoked start file. `gptel-runner-start` is not an `M-x` command. Do not append a
live start to a file meant only to register definitions. Supply all initial context
at start: putting it on the blackboard afterward can miss the first dispatched call.
Ordinary prompts still need to include the initial goal explicitly; the starter's
shared context does this.

Live execution is asynchronous; return of `start` does not mean completion. A fake
driver or immediate failure may finish before `start` returns, so callbacks must
use their `run` argument, not assume `my-run` has already been assigned. Keep the
event loop responsive; a busy wait blocks callbacks/timers. The completion callback
runs once for its terminal transition. Pause is not terminal. After a previously
reported terminal outcome, pass a fresh `:callback` to retry/continue/extension
if another terminal notification is required; the previously invoked callback
is not automatically invoked again. Resume takes its callback positionally.

Run IDs are session-local. `(gptel-runner-find-run my-run-id)` retrieves a retained
run, and `(gptel-runner-list-runs)` lists newest first. After restart, load its
snapshot before relying on its ID. Prefer run objects: resume, pause, abort, save,
and inspection accessors do not generally resolve ID strings. Retry, continue,
both extension APIs, record-decision, and forget-run do accept object or ID.

## Inspect and determine success

Open `M-x gptel-runner-show-dashboard`. Permanent keys are `RET` (call transcript
or run journal), `?` (action menu), `g` (refresh), and `q` (quit). The menu disables
inapplicable actions. The default refresh interval is two seconds; it is not a
reason for an agent to repeatedly fetch complete transcripts. Choose a run/call
row, then `? e` for events or `? v` for its worker buffer. Equivalent commands are
`gptel-runner-dashboard-inspect-events` and `gptel-runner-dashboard-visit-worker`.

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
| Decision log and previous goals/results | `(gptel-runner-decisions my-run)`, `(gptel-runner-continuations my-run)` |
| Node state, including a successful reviewer in a blocked loop | `(gethash 'grp-review (gptel-runner-run-node-states my-run))` and the enclosing repeat's state |
| Historical/current calls | `(gptel-runner-run-calls my-run)`; select by `gptel-runner-call-id` and `gptel-runner-node-id` of `gptel-runner-call-node` |
| Selected call details | `gptel-runner-call-state`, `-workspace`, `-prompt`, `-result`, `-error`, `-request-attempt`, `-buffer` accessors |
| Failure chronology | `(gptel-runner-run-events my-run)`; select recent relevant events using `gptel-runner-event-type`, `-node-id`, `-call-id`, `-data` |

Calls/events are in chronological order. Historical failed calls can remain after
successful recovery. Consult the current node/run state, latest relevant call,
round history, and input versions together. A displayed call may be `succeeded`
while the run is `blocked` because its valid review triggered `:stop-when`.
`waiting-confirmation` needs tool approval; `waiting-tool` means tool execution
(possibly waiting for user input); `waiting-feedback` needs a human result handoff.
None is a completed workflow. Transcripts contain the dispatched prompt, tool
activity, and final answer; editing ordinary transcript text does not update the
blackboard or decisions. Transcripts are retained by default but are not agent memory.

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
| Planned acceptance before advancing | Step `:pause-after t` | Response enters `waiting-feedback`; no parsed/saved result yet |
| Correct one unfinished call now | `(gptel-runner-pause-call call &optional reason)` | `p`, `gptel-runner-dashboard-pause-call`; retain live continuation |
| Accept human-reviewed call output | `(gptel-runner-complete-call-from-buffer &optional result call)` | `a`, `gptel-runner-dashboard-complete-call`; parser/validator still apply |
| Stop all work for a longer wait | `(gptel-runner-pause-run run &optional reason)` | `P`, `gptel-runner-dashboard-pause-run`; named workflow, stop duration clock, queue snapshot |
| Restart a paused run | `(gptel-runner-resume-run run &optional feedback callback)` | `r`, `gptel-runner-dashboard-resume-run`; completed nodes kept, unfinished work restarts |
| Retry same goal after failed/blocked/stalled/cancelled outcome | `(gptel-runner-retry run &key from-node feedback callback)` | `t`, `gptel-runner-dashboard-retry-run`; safe checkpoint |
| Rerun an executed agent node in an unsuccessful run | Same retry API with `:from-node 'node-id` | `T`, `gptel-runner-dashboard-retry-node` on a call row; invalidate node and dependent sequence work |
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

Use `:pause-after t` on the intended agent step for a predictable gate. After a
nonempty provider response, the call waits before parsing, validation, `:save-as`,
and downstream dispatch. Record approved enduring choices with
`(gptel-runner-record-decision run text rationale)` or `M-x gptel-runner-add-decision`
(`? d`), then accept the response. A printed decision or transcript correction
does not record a decision.

For an unplanned correction, `M-x gptel-runner-pause-call` in its worker stops
the provider and leaves a normal gptel conversation with the original continuation.
Give feedback and use ordinary gptel commands only within existing authorization.
Then `M-x gptel-runner-complete-call-from-buffer` submits the active region, or
otherwise the last text marked by gptel as a response. From Lisp you can explicitly
provide the reviewed text and call object:

```elisp
(gptel-runner-complete-call-from-buffer accepted-text selected-call)
```

In the dashboard `? a` uses the selected call's last response, not a region in
another buffer. Submitted text still must pass the node's parser/validator;
acceptance can trigger format repair or fail. Invalid output is never approval.

A call pause or gate leaves the run running: other branches may proceed and the
active-duration clock continues. Manually continued gptel conversations are outside
normal runner scheduling/accounting; do not treat runner remaining budgets as a
cap on those manual requests. Use whole-run pause for a longer wait or when all
branches must stop. It requires a named workflow, enables persistence, stops the
clock, and queues a snapshot. Completed results and charged usage remain.

In the same session a call already waiting at a gate retains its continuation
across whole-run pause. Completing that retained call also resumes the run; in
parallel work this may restart other unfinished calls. Other paused calls may be
completed from their retained buffers via reconstructed execution. An ordinary
`resume-run` instead supersedes unfinished call records and makes fresh calls;
it does not continue the old provider conversation. After loading a v2 snapshot
there are no historical call objects, transcripts, or live continuations to accept:
resume restarts the unfinished gated node statelessly.

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

Use `:persist t` for ongoing checkpoints. `(gptel-runner-save-run run)` and
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

Loading never starts work. A nonterminal saved state loads as `paused`; a terminal
state stays terminal and needs the appropriate retry/continue/extension operation.
Loading an ID already retained in the session is rejected; use the existing run
instead of creating a duplicate. `gptel-runner-resume-snapshot` has signature
`(file &optional feedback callback driver)` and combines load/resume immediately;
prefer separate calls when inspection is needed, and do not use it for terminal saves.

Version 2 snapshots retain goal, default workspace, blackboard (including decisions,
loop history, continuation archive), node states, repeat counts/limits, budgets,
timing/options, terminal failure, and extension context. They do not restore old
calls, journals, transcripts, callbacks, provider processes, or continuations.
Supply a fresh callback/driver; unfinished calls restart statelessly and their
previous usage remains charged. V1 snapshots are unsupported. Keep runtime objects
such as buffers/processes and captured live closures out of saved values. Snapshot
files use mode `0600`; their contents may include sensitive brief/result data.

## Cancellation and cleanup

`gptel-runner-abort-call` terminally cancels one call and propagates through its
parent policy; an optional branch's cancellation need not cancel the whole run.
`gptel-runner-abort-run` cancels queued/active work and ends the run as `cancelled`.
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
snapshots. Cleanup does not remove workspace artifacts or edit the definition file.
