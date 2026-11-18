# API details that affect pipeline correctness

Requires Emacs 29.1+; live requests also require compatible gptel (minimum
0.9.9.4) and configured presets.

## Definitions and constructors

```elisp
(require 'gptel-runner)

(gptel-runner-register-agent
 'my-worker :preset 'configured-preset :workspace-mode 'read)

(gptel-runner-defworkflow my-workflow
    (:max-calls 8 :max-requests 30 :max-concurrency 2
     :max-duration 1200 :decision-memory t)
  ;; Exactly one root node; use a sequence for multiple stages.
  (gptel-runner-agent-step
   :id 'work :agent 'my-worker :prompt #'my/prompt :save-as 'result))
```

`gptel-runner-defworkflow` quotes its options. In its options use data such as
`:context ((:file "brief.md") my/shared-context)`, not an expression meant to
execute `(list ...)` or `(gptel-runner-context-file ...)`. Node constructors,
registration, and start arguments are ordinary evaluated Lisp.

There is no public workflow/agent enumeration API. For discovery only, guard
reads of `gptel-runner--workflows` / `gptel-runner--agents` with `boundp`; inspect
objects through `gptel-runner-workflow-root`, `-options`, `gptel-runner-node-kind`,
`-properties`, `-children`, and `gptel-runner-agent-*` accessors. Do not mutate the
registries. Registering an existing name replaces its definition, so use scoped
names and inspect before replacement. `gptel-runner-unregister-agent` and
`gptel-runner-unregister-workflow` remove registrations; they are not run cleanup.
Personal definition files normally live in `~/.emacs.d/pipelines/`, loaded by
`pipeline.el`; include hidden `.el` files when searching and inspect top-level
forms before loading.

| Constructor | Relevant properties and behavior |
| --- | --- |
| `gptel-runner-agent-step` | `:id`, `:agent`, `:prompt`, `:save-as`, `:context`; optional `:completion`, `:workspace`, `:parser`, `:validator`, `:repair-invalid`, `:retries`, `:pause-after` |
| `gptel-runner-elisp-step` | Required `:function`; optional `:id`, `:save-as`, `:workspace`, `:workspace-mode`, `:retries`, `:async`, `:timeout`; see below |
| `gptel-runner-sequence` | Leading keyword/value properties, then child nodes; fail-fast; inherited `:context` |
| `gptel-runner-parallel` | Leading properties, then children; `:policy 'fail-fast`, `'collect`, or `'minimum-successes`; latter needs `:minimum-successes N`; optional aggregate `:save-as` |
| `gptel-runner-branch` | `:predicate` (alias `:if`) function of run, `:then` node, optional `:else` node, inherited `:context` |
| `gptel-runner-repeat-until` | Positive `:max`, `:body` node, optional `:until`, `:stop-when`, `:progress-key` functions of run; inherited `:context`; paired `:collect-keys` and `:save-history-as` |
| `gptel-runner-try` | `:body`, at least one of `:catch`/`:finally`; optional `:error-as`, `:catch-states`, `:save-as` |
| `gptel-runner-finalize` | Root-only `:body`/`:finally`, positive finite `:max-duration`, separate call/request allowance, optional `:on-cancel` |

Use Elisp steps for local operations and transformations with outputs. A planned
human gate is an agent step with `:pause-after t`; there is no standalone gate node.

Agent registration also supports `:context`, `:parser`, `:validator`, `:schema`,
`:retry-policy`, and `:metadata`. Schema is registered on the agent; do not assume
an unrecognized step keyword takes effect. Presets and tools must actually exist;
use [preset discovery](presets.md) before selecting them. Registration stores the
preset reference but does not prove it exists or that its dependencies resolve.
The gptel adapter accepts a preset symbol or an inline preset plist. A non-nil
step parser/validator overrides the registered one; nil falls back to it, rather
than disabling it. `:workspace-mode` defaults to `read`; its supported values
are `read`, `write`, and `isolated`. Metadata is application data, not behavior.

## Agent conversations and completion

The agent step owns its gptel buffer for the lifetime of the run. Its repeats,
semantic retries, output repairs, and follow-ups reuse that chat. Elisp and
control-flow steps retain outcomes and perform the sequencing. The standard
driver's default `handoff` mode exposes `RunnerHandoff(message)` and, where
applicable, `RunnerAskPrevious(question)`. Each takes one nonblank string and
must be the only tool call in its response. Use `(gptel-runner-handoff-p run node)`
and `(gptel-runner-previous-agent run node)` to inspect the mode and eligible
completed predecessor. See [routing and a worked exchange](handoffs.md#messages-between-node-chats).

A step can set `:completion 'response` for a preset that returns ordinary final
text, or `:completion 'handoff` to override a response-mode run. Run defaults
come from `gptel-runner-start :completion 'response` or the named workflow's
quoted options `(:completion response)`. A response-mode step ends automatic
routing in both directions. Use `:save-as` plus a consumer prompt or context
function to pass values across it.

## Node traversal and outcomes

Use these uniformly for agent, Elisp, and container nodes:

```elisp
(gptel-runner-find-node source 'review)      ; node or nil
(gptel-runner-list-nodes source predicate)   ; predicate optional
(gptel-runner-map-nodes function source)     ; function receives each node
(gptel-runner-node-path source 'review)      ; root-to-node path or nil
(gptel-runner-node-state run 'review)
(gptel-runner-node-outcome run 'review)
(gptel-runner-node-result run 'review 'unavailable)
(gptel-runner-node-error run 'review 'no-error)
```

Traversal accepts a run, workflow object/name, or subtree node; order is root
first, then children in definition order, including unused branches. Outcome
helpers take a run object or background invocation record and node object/ID;
unknown IDs signal an error. Invocation reads use exported node outcomes without
loading a checkpoint; older histories without this export report it unavailable.
Traversal needs the workflow definition and does not accept invocation records.

Outcomes contain `:node` and `:state`, plus `:value` for known terminal outcomes.
`pending`/`running` have no value; `succeeded :value nil` is successful nil;
`failed :value DATA` contains failure details. `node-result` returns accepted
successful output (parsed/validated for agents), otherwise its default;
successful nil stays nil. `node-error` returns failed/blocked/stalled/cancelled
data, otherwise its default.

Outcomes survive checkpoints without historical calls or `:save-as`. They track
the current execution: resetting a node clears its previous outcome. Older v2
snapshots may omit `:value`; check `plist-member` to distinguish unknown from nil.
Persistent outcome values must be readable Lisp data. Treat returned nodes and
values as read-only.

## Elisp steps

```elisp
(gptel-runner-elisp-step
 :id 'process :save-as 'processed
 :function (lambda (run _node)
             (upcase (gptel-runner-get run 'agent-output))))
```

Mix these with agents in sequences, parallel groups, branches, and repeats.
By default, the return value is the output, including nil. Execution is synchronous
in a temporary buffer with the node workspace as `default-directory`; keep functions
short. Elisp steps consume concurrency slots, but no call/request budget, and
Elisp-only workflows need no driver or agent registration.

`:workspace-mode` defaults to `read`; use `write` with `:allow-writes t` to share
writer locks with agents. Errors fail the step; `:retries` defaults to zero.
Agent-only options (`:prompt`, `:context`, `:parser`, `:validator`,
`:repair-invalid`, `:pause-after`) are rejected; validate inside the function.
Saved outputs participate in checkpoints and recovery like agent outputs.

### Asynchronous Elisp

- With `:async t`, `:function` receives `(run node resolve reject)` and returns
  promptly. Call `(funcall resolve value)` to succeed or `(funcall reject data)`
  to fail with `(:type elisp-error :error DATA)`; `:save-as` stores resolved values.
- Return nil or a cleanup function `(lambda (reason) ...)` to stop processes,
  cancel timers, and release buffers. It runs once with `succeeded`, `failed`,
  `cancelled`, `paused`, or `timeout`; cleanup errors are journaled.
- Optional `:timeout` is positive finite seconds per attempt, starting at
  admission. Expiry fails with `:type 'elisp-timeout`. `:retries` also retries
  rejections/timeouts, with a fresh timeout. Timers cannot preempt non-yielding Lisp.
- Duplicate and late callbacks are ignored. Inline completion waits for startup
  to return its cleanup. Pending work holds concurrency slots and writer locks.
- `gptel-runner-pause-run` cleans up; resume restarts unfinished work with fresh
  callbacks. Use lexical binding and capture your own buffers: the temporary
  startup buffer does not survive the function's return.

## Failure recovery and cleanup

```elisp
(gptel-runner-try
 :id 'attempt :body operation-node
 :error-as 'failure :catch recovery-node
 :finally cleanup-node :save-as 'result)
```

Each phase is a workflow node. Require `:body` and at least one of `:catch` or
`:finally`. Catch handles `failed` after retries/repairs; optionally set
`:catch-states '(failed blocked stalled)` or a subset. A successful handler
recovers the try and lets the sequence continue.

`:error-as` stores `(:node BODY-ID :state STATE :value VALUE)` before recovery
or cleanup, or nil on success. Read it with `gptel-runner-get`; use a key distinct
from `:save-as`, which stores the successful body/handler value, including nil.
Cleanup runs even if the handler fails; successful cleanup preserves the outcome,
while cleanup failure replaces it. An enclosing try can catch propagated failures.

Cancellation and duration expiry stop immediately without cleanup. Other budgets
still apply. Checkpoints resume the current phase; retry resumes the failed phase.
Branch/repeat function errors are catchable failures with `:type 'flow-error`.

## Bounded run finalization

Use `gptel-runner-finalize` only at the workflow root:

```elisp
(gptel-runner-finalize
 :id 'run :body workflow-node :finally publish-node
 :max-duration 30 :max-calls 1 :max-requests 2
 :on-cancel 'skip)               ; 'finalize explicitly opts in
```

Runs after success, failure, blocked/stalled outcomes, or budget exhaustion,
sharing the run's partial results. The finalizer needs its own positive finite
`:max-duration`; `:max-calls` and `:max-requests` default to 1 and accept 0.
Cancellation skips it by default; cancellation during finalization always stops it.
`try :finally` behavior is unchanged.

The run retains the body's outcome even if publication fails. Inspect
`(gptel-runner-finalization-outcome run)`: `:body` and `:result` contain
`(:state STATE :value VALUE)`; `:phase` is `finalizing` or `complete`.
The terminal callback waits for finalization. Pause/resume preserves its phase
and remaining allowance; `extend` cannot increase that allowance. Timers cannot
preempt non-yielding Lisp. Make publication idempotent for checkpoint restarts.

## Managed outputs in every launch mode

Use one output contract whether `gptel-runner-start` or a job launches the workflow:

| Need | Public call |
| --- | --- |
| Named values | `(gptel-runner-get source key &optional default)`; `(gptel-runner-outputs source)` returns an alist including inputs/history |
| Retain generated text | `(gptel-runner-write-artifact run name text &optional media-type)`; UTF-8, default `text/plain` |
| Retain an existing file | `(gptel-runner-publish-artifact run name file &optional media-type)`; source stays caller-owned |
| List retained versions | `(gptel-runner-artifacts source)`; descriptors in publication order |
| Resolve or read a descriptor | `(gptel-runner-artifact-file source descriptor)`, `(gptel-runner-read-artifact source descriptor)` |
| Feed an artifact to an agent | `:context (gptel-runner-context-artifact 'saved-key)` |

`run` is the executing run object. Reader `source` accepts a run or a fresh
invocation record, not an invocation ID. `gptel-runner-put` writes live run values;
`:save-as` stores the step's return value. Known nil stays nil with a non-nil
`get` default. Do not add separate local/background reader wrappers.

```elisp
(gptel-runner-elisp-step
 :id 'publish :save-as 'report
 :function (lambda (run _node)
             (gptel-runner-write-artifact
              run "report.md" (gptel-runner-get run 'analysis) "text/markdown")))
;; A later agent can use :context (gptel-runner-context-artifact 'report).
;; A consumer/inspector can read the same descriptor directly:
(gptel-runner-read-artifact run (gptel-runner-get run 'report))
```

Both publishers return `(:name ... :file ... :media-type ... :epoch ...
:published-at ...)`. `:file` is relative to managed storage, not a workspace;
resolve it with `gptel-runner-artifact-file`, never hand-assemble an absolute path.
`:epoch` is nil locally and the worker generation in jobs. Publication appends
to the artifact registry; `:save-as`/`put` separately names its descriptor on the
blackboard. Repeated names retain distinct versions rather than overwriting files.

Local storage is private and unique beneath `gptel-runner-output-directory`;
workers choose invocation storage automatically. No launch-mode tests, temporary
text files, manual copies, or directory/permission setup belong in the pipeline.
Files are published atomically with mode 0600 in private directories. Publishing
managed outputs alone does not require `:workspace-mode 'write` or `:allow-writes`;
actual workspace mutations still do. Relative file-copy sources use the run
workspace; for a node override, expand against `gptel-runner-node-workspace`.

Snapshots retain the directory and registry, not file contents. Artifacts survive
completion, snapshot deletion, forgetting runs, and continuation; delete retained
directories only as intentional cleanup. A copied snapshot alone does not transport
its files. Use node outcomes or selected loop history to build partial reports in
`gptel-runner-finalize`; do not reconstruct results from historical calls. Side
effects belong in steps, not parsers/context functions that can run again.

## Starting inputs and workspace defaults

`gptel-runner-start :inputs` accepts a proper alist or hash table, not a plist.
It creates a fresh `equal` blackboard before any node, context function, or prompt
executes. Keys and values are copied with `copy-tree` (including vectors), not a
general deep clone of arbitrary objects. Keep values as readable Lisp data for
snapshots; do not rely on runtime objects or mutable nested hash tables being copied.
Symbol, keyword, and string keys are different: match the pipeline's actual keys.
The symbol keys `gptel-runner-decisions` and `gptel-runner-continuations` are
reserved and rejected. Avoid other runner-internal keys as application inputs.

For example, after registering `my-worker` with a compatible preset:

```elisp
(defun example/input-prompt (run node)
  (let ((subject (gptel-runner-get run 'subject)))
    (unless (and (stringp subject) (not (string-blank-p subject)))
      (error "Pipeline requires a non-empty subject input"))
    (format "Goal: %s\nWorkspace: %s\nInspect: %s"
            (gptel-runner-run-goal run)
            (gptel-runner-node-workspace run node) subject)))

(gptel-runner-defworkflow input-example
    (:max-calls 3 :max-requests 6 :max-duration 300 :allow-unconfirmed-tools t)
  (gptel-runner-agent-step
   :id 'inspect :agent 'my-worker :prompt #'example/input-prompt
   :save-as 'report))

;; Dispatches immediately: choose the driver and existing directory beforehand.
(gptel-runner-start 'input-example
 :goal "Explain the cancellation path" :workspace my-project-root
 :inputs '((subject . "src/importer.el")) :driver my-driver)
```

`:inputs` is a start argument, not an inherited workflow option. It makes data
available through `gptel-runner-get`; it does not stringify it into every prompt.
Use `:context` or a prompt helper to select what the receiving agent needs.
Inputs persist as blackboard entries. Retry/resume/continue have no replacement
`:inputs` argument; deliberately update application keys with `gptel-runner-put`
before restarting dependent work, or start a new run when the definition/plan
must change. A workflow-owned `:save-as` key can overwrite an input and is cleared
on a successful-run continuation; keep input and output keys separate.

Run workspace precedence is explicit start `:workspace` → workflow option
`:workspace` → calling buffer's `default-directory`. Explicit nil at start
bypasses the workflow default and uses `default-directory`. Relative run defaults
resolve from the calling buffer directory; prefer absolute paths for predictable
direct starts. The selected directory must exist and is canonicalized. Workflow
options are quoted: use a literal string, not `(expand-file-name ...)`, for a
default `:workspace`; pass computed directories at start instead.

Budgets, concurrency, `:persist`, and `:decision-memory` also allow explicit start
overrides of workflow defaults. Package fallbacks are unlimited calls/requests/
duration, concurrency 1, decision memory t, and persistence nil. Workflow and run
`:context` are additive. `:allow-unconfirmed-tools` uses the explicit start
value first, then the workflow default, then nil. For example, starting the
workflow above with `:allow-unconfirmed-tools nil` restores its usual tool
confirmation policy for that run. Supply goal, inputs, driver, callback, and
`:allow-writes` as start arguments. Select a driver explicitly or configure
`gptel-runner-default-driver`.

## Function signatures

| Hook/API | Arguments/result |
| --- | --- |
| Step `:prompt` | String, or function `(run node)` returning a string |
| Context function | `(run node)` returning a string or nil; node is the target agent step |
| Parser | One raw result argument; return parsed data or signal an error; nil is invalid |
| Validator | One parsed result argument; return non-nil if acceptable |
| Branch/loop/progress function | `(run)`; boolean predicate or stable comparable progress value |
| `gptel-runner-get` | `(source key &optional default)`; source is a run or invocation record |
| `gptel-runner-put` | `(run key value)` |
| `gptel-runner-iteration` | `(run repeat-node-id)`; **completed** iteration count |
| `gptel-runner-record-decision` | `(run text &optional rationale call)`; run object or displayed run ID |
| `gptel-runner-node-workspace` | `(run node-or-id)`; effective canonical directory for the target agent or Elisp step |

During a loop body, display `(1+ (gptel-runner-iteration run 'cycle))`.
After that body completes, the count is incremented before the stop/until
predicates run. Avoid comparing a body-stamped round to `1+` in an after-body
predicate. Initial `:until` is also checked before the first body execution.

## Per-node workspaces and concurrent writers

Agent and Elisp step `:workspace` accepts a non-empty directory-name string.
Omitted or nil inherits the run workspace; relative paths resolve from the run workspace and
absolute paths are used directly. Paths are canonicalized, including symlinks,
with a trailing separator. All overrides must name existing directories and are
validated before any calls start. Functions and nonexistent future directories
are not supported workspace overrides. Provision directories before starting.

The override belongs to the **step**, not the registered agent or a sequence,
parallel, branch, or repeat container. Set it on each relevant leaf, including
the reviewer that must inspect a writer's files. Worker-buffer `default-directory`,
call workspace, retries, and repair prompts use that effective directory. Prompt
and context functions should call `gptel-runner-node-workspace`; do not assume a
helper's ambient `default-directory` has already been changed. The run accessor
still returns the default directory, useful for explicitly shared artifacts.

For example, after configuring the preset `repo-writer` and creating `api/` and
`web/` beneath the chosen run directory:

```elisp
(gptel-runner-register-agent
 'repo-writer :preset 'repo-writer :workspace-mode 'write)

(defun example/repo-task (run node)
  (format
   (concat "Shared goal: %s\nYour workspace: %s\n"
           "Implement this repository's part of the agreed plan. Preserve "
           "shared decisions. Return evidence, checks, and artifact paths "
           "qualified by repository; identify unresolved integration needs.")
   (gptel-runner-run-goal run) (gptel-runner-node-workspace run node)))

(gptel-runner-defworkflow multi-repo-work
    (:max-concurrency 2 :max-calls 6 :max-requests 30 :max-duration 1800)
  (gptel-runner-parallel
   :id 'repos :policy 'fail-fast
   (gptel-runner-agent-step
    :id 'api :agent 'repo-writer :workspace "api"
    :prompt #'example/repo-task :save-as 'api-report)
   (gptel-runner-agent-step
    :id 'web :agent 'repo-writer :workspace "web"
    :prompt #'example/repo-task :save-as 'web-report)))
```

Supply the shared brief/plan using `gptel-runner-start :context` (inline text or
an absolute file descriptor) and the common goal, run workspace, and
`:allow-writes t`. This example assumes independent work against an already agreed
interface. Add a joined review/integration stage when completion requires combined
verification; parallel completion alone does not prove compatibility.

Within one run, agents registered with `:workspace-mode 'write` serialize only
when their effective canonical directories are equal. Symlink aliases share that
lock; different directories can run concurrently within the global concurrency
limit. This is not a lock across separate runs, a reader/writer lock, or a check
for overlapping parent/child trees, shared Git state, or external resources.
Order conflicting work explicitly. `isolated` creates no isolation and does not
count as a writer for this lock; use `write` with separately provisioned worktrees.

The bundled `gptel-runner-workspace-diff-key` still reads the **run** workspace.
It hashes `git diff --no-ext-diff` only when `.git` is a directory: staged-only
changes, untracked files, and Git worktrees with a `.git` file are not covered.
For progress across overridden workspaces, compute a stable digest of relevant
artifacts/tests from each directory; do not mistake an unchanged default-root diff
for stalled work elsewhere. For external admission hooks, see
[runtime integrations](runtime-integrations.md#call-admission-and-release).

## Context and decision propagation

Context sources are strings, functions, `(:file "path")` descriptors, or
`(:artifact key)` descriptors from `gptel-runner-context-artifact`, singly or in
a list. Order is workflow → start/run → registered agent → nodes from
root to leaf → ordinary task prompt. Recorded decisions are appended afterward
when decision memory is enabled, as it is by default.

At every scope, relative file descriptors resolve from the **receiving agent's
effective workspace**, including workflow, run, and ancestor-node context.
Absolute paths are used directly. Thus a workflow-level `(:file "AGENTS.md")`
loads each receiving repository's own file. Use an absolute descriptor for one
shared brief; in evaluated code, for example,
`(gptel-runner-context-file (expand-file-name "brief.md" shared-root))`.
Workflow options are quoted, so use literal absolute descriptors there or a
named context function that explicitly resolves from the intended shared root.

File contents and functions are resolved anew when a call is prepared. **All
declared file descriptors must be readable at workflow start in each receiving
workspace**, including those on later/unselected nodes. For a managed artifact
created by an earlier step, use `(gptel-runner-context-artifact 'saved-key)`.
It resolves the descriptor from the blackboard at call preparation, so the file
need not exist at workflow start. Missing keys/unreadable artifacts fail as context
errors. Use a context function only for task-specific selection or formatting,
not to reproduce this reader. Artifact context follows managed storage regardless
of the receiving agent's workspace. It does not automatically include the goal.

An initial `:goal` is stored on the run but not automatically placed in ordinary
agent prompts. Inject it explicitly. The adjacent downstream agent in handoff
mode receives the accepted raw message. To use its parsed value in Elisp, across containers,
or in another agent's prompt, store it with `:save-as` and read it with
`gptel-runner-get`. A blackboard write takes effect on subsequent reads.

Decisions are ordered, append-only, run-local entries with text, optional
rationale, and source metadata. They persist on the blackboard. Source call/node
is inferred in runner worker buffers or can be supplied explicitly. Automatic
propagation does not authorize arbitrary agents to accept or supersede decisions.
Avoid copying the full decision log into prompts as well as enabling injection.

## Loops, joins, and result freshness

Repeat options `:collect-keys '(implementation review)` and
`:save-history-as 'history` must be supplied together. Every collected key must
be written by the body; history must have a separate destination. Entries are:

```elisp
((:iteration 1 :values ((implementation . "...") (review . (:verdict revise))))
 (:iteration 2 :values ((implementation . "...") (review . (:verdict pass)))))
```

Collection occurs after a successful body, before stop/until checks. Only keys
whose producer succeeded in that iteration are captured. Ordinary blackboard
values are **not all cleared** when the body resets: a skipped branch can leave
an old result. For conditional producers, use the current history entry or
explicit freshness metadata/state checks; never use the mere existence of an
old passing review to approve new work. Unconditional sequential producers avoid
this particular hazard.

`fail-fast` requires every parallel child to succeed. `collect` finishes the
join successfully even when children fail; inspect each child's state before
using its value. `minimum-successes` tolerates some failures and waits
for all remaining children once the threshold is possible/met; it is not a race
that automatically stops at the first N successes. Aggregate records contain
`:node`, `:state`, and `:value`; identify them by node, not completion position.
Use independent per-child keys for required inputs.

## Validation, repairs, and gates

Request retry policy and agent-step retry are separate:

```elisp
(gptel-runner-register-agent
 'my-worker :preset 'configured-preset :workspace-mode 'read
 :retry-policy (gptel-runner-retry-policy-create
                :max-retries 2 :base-delay 1.0 :max-delay 30.0 :jitter 0.2))
```

These are the policy defaults. Retryable HTTP statuses default to
408/425/429/500/502/503/504/529; transient failures with no HTTP status or driver
metadata `:retryable t` can also retry, unless `:permanent` is non-nil.
Retries use capped exponential delay / retry-after with jitter, reuse the logical
call, and consume another request attempt. Step `:retries N` defaults to zero and
starts fresh calls after execution failures; it does not turn a parsed `revise`
into an iteration or keep retrying an invalid parsed result. Use a repeat for
revision and the format repair path for invalid output. Retries can repeat tool
side effects, so recheck artifacts and make consequential operations idempotent.

`:max-requests` counts runner driver dispatch attempts;
additional open-discussion turns reenter dispatch and consume request budget.
Internal tool-loop HTTP requests and explicit human-review feedback have
separate accounting. Nested gptel-agent workers/discussions are internal to the
outer call and are not separate runner calls or concurrency slots; see the
[nested-work contract](runtime-integrations.md#gptel-agent-presets-and-nested-work).
These counters are not complete provider-request or spending caps.
`:max-calls` counts logical agent calls, including routed question/reply turns, fresh agent retries, and output repairs.
Duration measures active run time, including call gates but excluding whole-run
pauses. Keep provider/tool limits separate when an actual spend cap is required.

The optional review helpers provide `gptel-runner-parse-review`,
`gptel-runner-valid-review-p`, and `gptel-runner-review-schema`. A review is a
plist with symbolic `:verdict` (`pass`, `revise`, `blocked`), `:summary`, and
`:issues`. Add a semantic validator if pass must imply no unresolved issues;
the generic validator alone does not enforce that relationship.
Issue fields are required string `:severity` / `:message`, optional `:file`,
positive integer or nil `:line`, and `:suggested_fix`. In handoff mode the driver describes the schema in the prompt and asks for a
JSON string as the handoff message. In response mode the backend can use its
usual structured-output mechanism. The runner enforces parsing and validation.
`gptel-runner-normalize-review (review)` normalizes verdict/issues;
`gptel-runner-review-progress-key (run &optional review-key)` hashes that material,
and `gptel-runner-review-diff-progress-key` combines it with the run workspace
diff. Both default to the key `review`; pass your actual review key.

`:repair-invalid t` or a registered schema allows one automatic format repair.
It is a fresh call in the same node chat. Its prompt contains the goal,
effective workspace, invalid output, and validation error. The inherited
context and decisions from earlier turns remain in the conversation.
Keep it a format-only operation. Make parser errors explain the required
shape; do not let repair revise substantive choices. Where a repair needs task
judgment or full context, build an explicit contextual repair stage instead.
An empty final output can also trigger a repair carrying the original resolved
prompt. The empty-output and invalid-format paths share one repair allowance
per node execution. In handoff mode nonblank prose without a handoff is a
discussion turn and parks the call. Streaming follows gptel/preset settings.
A tool stop missing its required tool payload is a transient protocol failure.

`:pause-after t` holds a proposed handoff or final response before parsing,
saving, and advancing. Accept or replace it with
`gptel-runner-complete-call-from-buffer`. Current runner session snapshots
preserve the waiting call's chat and proposed answer. Open the run and accept
the answer to release the gate, or resume the run to restart unfinished turns.

## Run control and durable context

Use [running and managing pipelines](operations.md) for exact lifecycle signatures,
interactive commands, operation selection, examples, and snapshot limitations.
Supply first-call context to `gptel-runner-start`; it dispatches immediately, so
seeding the blackboard afterward is too late. Preserve downstream inputs through
node outcomes, `:save-as`, or durable artifacts, and use readable Lisp values/named
context functions for persistence. Recovery does not freeze files, copy repositories, or
automatically refresh a completed plan after the goal changes.
