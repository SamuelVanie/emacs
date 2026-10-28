# API details that affect pipeline correctness

Grounded in the local gptel-runner implementation inspected on 2026-10-28.
Recheck the installed source when an option differs. The engine requires Emacs
29.1+; live requests additionally need the installed compatible gptel and presets.

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

| Constructor | Relevant properties and behavior |
| --- | --- |
| `gptel-runner-agent-step` | `:id`, `:agent`, `:prompt`, `:save-as`, `:context`; optional `:parser`, `:validator`, `:repair-invalid`, `:retries`, `:pause-after` |
| `gptel-runner-sequence` | Leading keyword/value properties, then child nodes; fail-fast; inherited `:context` |
| `gptel-runner-parallel` | Leading properties, then children; `:policy 'fail-fast`, `'collect`, or `'minimum-successes`; latter needs `:minimum-successes N`; optional aggregate `:save-as` |
| `gptel-runner-branch` | `:predicate` (alias `:if`) function of run, `:then` node, optional `:else` node, inherited `:context` |
| `gptel-runner-repeat-until` | Positive `:max`, `:body` node, optional `:until`, `:stop-when`, `:progress-key` functions of run; inherited `:context` |

There is no public generic transform node or standalone gate node in this
version. Put deterministic projection in prompt/context helpers or predicates,
without using their repeated evaluation for unrelated side effects. A planned
human gate is an agent step with `:pause-after t`.

Agent registration also supports `:context`, `:parser`, `:validator`, `:schema`,
`:retry-policy`, and `:metadata`. Schema is registered on the agent; do not assume
an unrecognized step keyword takes effect. Presets and tools must actually exist.

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

During a loop body, display `(1+ (gptel-runner-iteration run 'cycle))`.
After that body completes, the count is incremented before the stop/until
predicates run. Avoid comparing a body-stamped round to `1+` in an after-body
predicate. Initial `:until` is also checked before the first body execution.

## Context and decision propagation

Context sources are strings, functions, or `(:file "path")` descriptors, singly
or in a list. Order is workflow → start/run → registered agent → nodes from
root to leaf → ordinary task prompt. Recorded decisions are appended afterward
when decision memory is enabled, as it is by default.

Files are workspace-relative unless absolute. File contents and functions are
resolved anew when a call is prepared. **All declared file descriptors must be
readable at workflow start**, including those on later nodes. For an artifact
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
using its value. `minimum-successes` tolerates some failures and currently waits
for all remaining children once the threshold is possible/met; it is not a race
that automatically stops at the first N successes. Aggregate records contain
`:node`, `:state`, and `:value`; identify them by node, not completion position.
Use independent per-child keys for required inputs.

## Validation, repairs, and gates

The optional review helpers provide `gptel-runner-parse-review`,
`gptel-runner-valid-review-p`, and `gptel-runner-review-schema`. A review is a
plist with symbolic `:verdict` (`pass`, `revise`, `blocked`), `:summary`, and
`:issues`. Add a semantic validator if pass must imply no unresolved issues;
the generic validator alone does not enforce that relationship.

`:repair-invalid t` or a registered schema allows one automatic format repair.
It is a fresh call. Its prompt contains goal, workspace, invalid output, and
validation error, **not the complete inherited context or decision log** in this
version. Keep it a format-only operation. Make parser errors explain the required
shape; do not let repair revise substantive choices. Where a repair needs task
judgment or full context, build an explicit contextual repair stage instead.
An empty final answer also gets one repair, carrying the original resolved prompt.

`:pause-after t` holds the response before parsing, saving, and advancing. Accept
or replace it with `gptel-runner-complete-call-from-buffer`; parser/validator still
apply. A loaded snapshot cannot restore that live continuation and restarts the
unfinished gated node.

## Run control and durable context

`gptel-runner-start` accepts goal, workspace, driver, budgets, `:context`,
`:persist`, callback, `:decision-memory`, `:allow-writes`, and
`:allow-unconfirmed-tools`. It starts immediately; do not seed first-call context
by calling `gptel-runner-put` after start. Supply that context at start instead.

`gptel-runner-retry` preserves the goal and completed prerequisites; `:from-node`
reruns that node and downstream dependent work. Retry feedback is consumed by the
selected/next call. `gptel-runner-resume-run` also delivers feedback only to the
first unfinished prompt. Record enduring corrections before using these APIs.

`gptel-runner-continue` uses a human observation as the new goal. After success,
it archives/clears workflow-owned results and runs the whole workflow again.
After failure it resumes at a checkpoint, retaining completed predecessors:
if the changed goal invalidates an earlier plan, prefer a new run with the
corrected brief, or arrange an explicit replan before releasing dependent work.
Do not call continue expecting it to redo already completed planning automatically.
`gptel-runner-extend` adds run budget; `gptel-runner-extend-repeat` adds loop
iterations. Do not silently call them to evade a configured limit.

Persistence saves goal, workspace, blackboard, node/loop states, and budgets;
it does not restore historical transcripts or events. Use readable Lisp data and
named context functions, not buffers, processes, or captured live objects. Re-read
artifact files after restoring: snapshots do not freeze external workspace content.
