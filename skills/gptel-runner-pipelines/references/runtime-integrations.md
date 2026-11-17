# Pipeline runtime integrations

Verified against `8cdc6ed` on 2026-11-17. Read this for custom drivers, external
resource admission, headless feedback, or publishing a pipeline's file outputs.
Ordinary pipeline authoring needs the [API](runner-api.md) and
[operations](operations.md) references. Job definitions, worker bootstrap,
schedules, remote commands, and command receipts stay in the
[jobs skill](../../gptel-runner-jobs/SKILL.md).

## Drivers and deterministic execution checks

Select `gptel-runner-gptel-driver-create` for live gptel calls or
`gptel-runner-fake-driver-create` for network-free control-flow checks. A fake
driver does not resolve presets or run tools. Queue outcomes before starting:

```elisp
(setq my-driver (gptel-runner-fake-driver-create))
(gptel-runner-fake-queue my-driver 'my-worker
                         '(:value "Initial report" :manual t)
                         '(:value "Revised report"))
;; Start your workflow with :driver my-driver, then inspect its pending call.
;; (gptel-runner-fake-release my-driver selected-call)
```

`gptel-runner-fake-queue (driver agent &rest outcomes)` queues by registered
runner agent name, not node ID or preset. Each outcome is a plist: `:status`
(default `success`), `:value`, `:metadata`, optional `:manual t` or `:delay`
seconds, `:observations` alist, and `:duplicate` for callback checks. An outcome
can also be a function of the call returning a plist. `gptel-runner-fake-release
(driver call-or-id &optional outcome)` completes a manual outcome, optionally
replacing it. Missing queued outcomes are errors, not implicit success.
Inspect `gptel-runner-fake-driver-starts` for actual calls/prompts and
`gptel-runner-fake-driver-max-active` for observed concurrency. Yield to Emacs's
event loop when using delayed responses or snapshot writes.

Only implement a custom driver when the supplied drivers do not fit:

| Generic | Contract |
| --- | --- |
| `gptel-runner-driver-start (driver call complete observe)` | Start asynchronous work; call `complete` at most once with status, value, optional metadata; call `observe` with event type/data. |
| `gptel-runner-driver-cancel (driver call)` | Stop provider work and arrange cleanup. |
| `gptel-runner-driver-pause (driver call)` | Optional; defaults to driver cancellation of external work while the core keeps the call nonterminal. |
| `gptel-runner-driver-await-feedback (driver call)` | Optional; prepare a completed provider transcript for human feedback; default is no-op. |

Completion status is `success`, `cancelled`, `blocked`, or `transient`; other
statuses fail. Transient metadata can contain `:http-status`, `:retry-after`,
`:retryable`, and `:permanent`. Core owns budgets, state transitions, parsing,
validation, and retry decisions. Keep private gptel API adaptation in
`gptel-runner-gptel.el`, not in generated pipelines. Consult that adapter and its
mocked-transport tests before integrating provider-specific behavior.

`gptel-runner-gptel-restore-worker-buffer (call transcript)` reconstructs a
human-readable buffer and applies the agent preset; it does not recreate a
provider request or scheduler continuation. Ordinary v2 snapshot loading does
not use it to restore historical calls. Feedback and explicit acceptance are
documented in [operations](operations.md#human-gates-and-pausing).

## Call admission and release

`gptel-runner-call-admission-functions` is a hook of functions taking one call.
All must return non-nil after normal concurrency/workspace checks for dispatch to
proceed. They may acquire resources. A nil return leaves the call queued (`ready`);
inspect an external lock/admission constraint before assuming the provider hung.
Do not busy-wait inside these functions.

`gptel-runner-call-release-hook` receives the call when it is deactivated and
when an admission attempt is refused or signals. Release functions must be
idempotent and tolerate partial acquisition or no acquisition. Use these hooks
only when integrating an actual resource controller; ordinary pipeline code
should not invoke private queue/drain functions. A custom controller must arrange
another scheduling opportunity when a resource becomes available; changing its
flag alone is not a public wake-up API.

Core writer serialization still covers equal canonical directories **within one
run**. The background jobs worker installs additional hooks for cross-process
writer coordination; direct interactive runs do not automatically participate.
Neither mechanism detects overlapping parent/child directories or shared external
resources, and neither provisions isolation. Preserve `write` registrations for
actual writers even when their worktrees are separate.

## Headless tool interactions

With `gptel-runner-headless` non-nil, the gptel driver publishes pending tool
approval through `gptel-runner-interaction-hook`, rather than local approval UI.
Each hook function receives `(call interaction)`. The serializable interaction
contains `:id`, `:kind` (`tool-approval`), `:call-id`, `:generation`, and `:tools`
(name/args). Live callbacks remain private to the owning process.

`gptel-runner-gptel-answer-interaction (call interaction-id approve)` answers that
exact pending interaction once. Non-nil approve executes its tools; nil cancels
the call with `tool-rejected`, whose effect on the run follows the parent policy.
It checks the current generation, `waiting-confirmation` state, live buffer,
and matching pending ID. Pausing/cancelling invalidates pending approval. Never
reuse an ID for a later interaction or serialize live call objects for another
process. Use the jobs layer to route answers to a background worker.

Headless mode does not grant tool approval or change the explicit start-time
confirmation policy. The policy also applies during programmatic feedback.

## Pipeline file outputs

Use `:save-as` for parsed node results and `gptel-runner-put` for explicit
application values. Ordinary local runs can retain absolute artifact paths on
the blackboard; a path does not freeze its contents or copy a file into snapshots.
Include repository/revision identity when downstream evidence needs it.

Inside a background invocation, `gptel-runner-publish-artifact
(run name file &optional media-type)` copies a readable regular file into durable
invocation history and returns a descriptor (`:name`, invocation-relative `:file`,
`:media-type`, `:epoch`, `:published-at`). The copy survives later changes to the
source; ordinary direct runs reject publication because no invocation owns it.
Use this in an explicit output integration after the file exists. Parsers and
context functions may be re-evaluated, so do not hide publication side effects
there or assume a model's response invokes this Lisp API.

## Dashboard integrations

`gptel-runner-dashboard-entry-functions` adds externally owned rows without
registering their runs in the local scheduler. Row identifiers must be distinct.
`gptel-runner-dashboard-row-action-functions` receives `(action row-id)` for
`open` or `menu`; non-nil means handled. These are adapter hooks, not workflow
nodes. Use the installed jobs dashboard support for background invocations.
