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
| `gptel-runner-agent-step` | `:id`, `:agent`, `:prompt`, `:save-as`, `:context`; optional `:workspace`, `:parser`, `:validator`, `:repair-invalid`, `:retries`, `:pause-after` |
| `gptel-runner-elisp-step` | Required `:function`; optional `:id`, `:save-as`, `:workspace`, `:workspace-mode`, `:retries`; see below |
| `gptel-runner-sequence` | Leading keyword/value properties, then child nodes; fail-fast; inherited `:context` |
| `gptel-runner-parallel` | Leading properties, then children; `:policy 'fail-fast`, `'collect`, or `'minimum-successes`; latter needs `:minimum-successes N`; optional aggregate `:save-as` |
| `gptel-runner-branch` | `:predicate` (alias `:if`) function of run, `:then` node, optional `:else` node, inherited `:context` |
| `gptel-runner-repeat-until` | Positive `:max`, `:body` node, optional `:until`, `:stop-when`, `:progress-key` functions of run; inherited `:context`; paired `:collect-keys` and `:save-history-as` |

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

## Elisp steps

```elisp
(gptel-runner-elisp-step
 :id 'process :save-as 'processed
 :function (lambda (run _node)
             (upcase (gptel-runner-get run 'agent-output))))
```

Mix these with agents in sequences, parallel groups, branches, and repeats.
The return value is the output, including nil. Execution is synchronous in a
temporary buffer with the node workspace as `default-directory`; keep functions
short. Elisp steps consume concurrency slots, but no call/request budget, and
Elisp-only workflows need no driver or agent registration.

`:workspace-mode` defaults to `read`; use `write` with `:allow-writes t` to share
writer locks with agents. Errors fail the step; `:retries` defaults to zero.
Agent-only options (`:prompt`, `:context`, `:parser`, `:validator`,
`:repair-invalid`, `:pause-after`) are rejected; validate inside the function.
Saved outputs participate in checkpoints and recovery like agent outputs.

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
    (:max-calls 3 :max-requests 6 :max-duration 300)
  (gptel-runner-agent-step
   :id 'inspect :agent 'my-worker :prompt #'example/input-prompt
   :save-as 'report))

;; Dispatches immediately: choose the driver and existing directory beforehand.
(gptel-runner-start 'input-example
 :goal "Explain the cancellation path" :workspace my-project-root
 :inputs '((subject . "src/importer.el")) :driver my-driver
 :allow-unconfirmed-tools t)
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
`:context` are additive. Goal, inputs, driver, callback, and the two permission
flags are supplied at start; putting them in workflow options does not supply
their start arguments. Select a driver explicitly or configure
`gptel-runner-default-driver`.

## Function signatures

| Hook/API | Arguments/result |
| --- | --- |
| Step `:prompt` | String, or function `(run node)` returning a string |
| Context function | `(run node)` returning a string or nil; node is the target agent step |
| Parser | One raw result argument; return parsed data or signal an error; nil is invalid |
| Validator | One parsed result argument; return non-nil if acceptable |
| Branch/loop/progress function | `(run)`; boolean predicate or stable comparable progress value |
| `gptel-runner-get` | `(run key &optional default)` |
| `gptel-runner-put` | `(run key value)` |
| `gptel-runner-iteration` | `(run repeat-node-id)`; **completed** iteration count |
| `gptel-runner-record-decision` | `(run text &optional rationale call)`; run object or displayed run ID |
| `gptel-runner-node-workspace` | `(run node)`; effective canonical directory for the target agent or Elisp step |

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

Context sources are strings, functions, or `(:file "path")` descriptors, singly
or in a list. Order is workflow → start/run → registered agent → nodes from
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
workspace**, including those on later/unselected nodes. For an artifact
created by an earlier step, use its blackboard result or a context function that
reads it when required; do not declare a nonexistent future file descriptor.
Required missing input should signal an error rather than produce an empty prompt.

An initial `:goal` is stored on the run but not automatically placed in ordinary
agent prompts. Inject it explicitly. `:save-as` stores a parsed result; it does
not route it automatically. `gptel-runner-put` does not alter prompts already sent.

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
gptel's internal tool-loop HTTP requests and manual/programmatic feedback do not
each increment it. It is not a complete provider-request or spending cap.
`:max-calls` counts logical agent calls, including fresh agent retries and output repairs.
Duration measures active run time, including call gates but excluding whole-run
pauses. Keep provider/tool limits separate when an actual spend cap is required.

The optional review helpers provide `gptel-runner-parse-review`,
`gptel-runner-valid-review-p`, and `gptel-runner-review-schema`. A review is a
plist with symbolic `:verdict` (`pass`, `revise`, `blocked`), `:summary`, and
`:issues`. Add a semantic validator if pass must imply no unresolved issues;
the generic validator alone does not enforce that relationship.
Issue fields are required string `:severity` / `:message`, optional `:file`,
positive integer or nil `:line`, and `:suggested_fix`. Treat the schema as a
provider hint; parser and validator enforce the actual contract.
`gptel-runner-normalize-review (review)` normalizes verdict/issues;
`gptel-runner-review-progress-key (run &optional review-key)` hashes that material,
and `gptel-runner-review-diff-progress-key` combines it with the run workspace
diff. Both default to the key `review`; pass your actual review key.

`:repair-invalid t` or a registered schema allows one automatic format repair.
It is a fresh call. Its prompt contains goal, effective node workspace, invalid
output, and validation error, **not the complete inherited context or decision log**.
Keep it a format-only operation. Make parser errors explain the required
shape; do not let repair revise substantive choices. Where a repair needs task
judgment or full context, build an explicit contextual repair stage instead.
An empty final answer also gets one repair, carrying the original resolved prompt.
The empty-answer and invalid-format paths share one repair allowance per node
execution. Streaming follows gptel/preset settings; intermediate commentary and
tool activity remain in the transcript/events, while only the final model answer
is parsed/saved. A tool stop without its required tool payload is retried as a
transient protocol failure, not accepted as a finished answer.

`:pause-after t` holds the response before parsing, saving, and advancing. Accept
or replace it with `gptel-runner-complete-call-from-buffer`; parser/validator still
apply. A loaded snapshot cannot restore that live continuation and restarts the
unfinished gated node.

## Run control and durable context

Use [running and managing pipelines](operations.md) for exact lifecycle signatures,
interactive commands, operation selection, examples, and snapshot limitations.
Supply first-call context to `gptel-runner-start`; it dispatches immediately, so
seeding the blackboard afterward is too late. Preserve downstream inputs through
`:save-as` or durable artifacts, and use readable Lisp values/named context
functions for persistence. Recovery does not freeze files, copy repositories, or
automatically refresh a completed plan after the goal changes.
