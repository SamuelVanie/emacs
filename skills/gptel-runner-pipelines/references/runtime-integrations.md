# Pipeline runtime integrations

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

For a conversational node, return `success` with the submitted handoff string,
`follow-up` with a question for the preceding agent, or `discussion` with
assistant prose that leaves the call waiting for feedback. In response mode
`success` carries the final response. The fake driver accepts these same
statuses in its queued outcomes, so exercise the return path by queueing a
`follow-up` for the requester, a reply for its predecessor, then a final result
for the requester. To exercise errors use `cancelled`, `blocked`, `transient`, or
a failure status. Transient metadata can contain `:http-status`, `:retry-after`,
`:retryable`, and `:permanent`. Core owns budgets, state transitions, parsing,
validation, and retry decisions. Keep private gptel API adaptation in
`gptel-runner-gptel.el`, not in generated pipelines. Consult that adapter and its
mocked-transport tests before integrating provider-specific behavior.

`gptel-runner-gptel-restore-worker-buffer (call transcript)` reconstructs a
legacy plain-text transcript as a node chat. For normal workflows,
`(gptel-runner-node-buffer run node)` opens the retained conversation,
including one restored from a current session. See [feedback and explicit
acceptance](operations.md#human-gates-and-pausing).

## gptel-agent presets and nested work

Use the standard gptel driver for gptel-agent presets. Plain gptel presets remain
usable without gptel-agent. When gptel-agent is loaded, the adapter requires these
public functions; compatible versions must be loaded in the process doing the work:

| Public gptel-agent API | Responsibility |
| --- | --- |
| `gptel-agent-supervise` | Create an opaque buffer-owned handle, with state and tool-approval callbacks. |
| `gptel-agent-supervision-request-live-p` | Check that a retained request still belongs to a live supervisor. |
| `gptel-agent-supervision-retry` | Rearm the failed root request's gate before the host resumes that FSM. |
| `gptel-agent-supervision-stop` | Release supervision and cancel descendants; the host owns root transport cancellation. |

For a loaded integration, this read-only query checks actual API availability
and the source of the loaded implementation, without sending model requests:

```elisp
(list :agent-loaded (featurep 'gptel-agent-tools)
      :agent-runtime (symbol-file 'gptel-agent-supervise 'defun)
      :runner-runtime (symbol-file 'gptel-runner-gptel-driver-create 'defun)
      :agent-api
      (mapcar (lambda (name) (cons name (fboundp name)))
              '(gptel-agent-supervise gptel-agent-supervision-request-live-p
                gptel-agent-supervision-retry gptel-agent-supervision-stop)))
```

An unloaded package is not proof of an incompatible version. Load the selected
setup normally, then check it. Resolve missing APIs or mismatched library paths;
do not recreate the former private-agent advice or periodic supervision timer.
A fresh process clears advice left by the old workaround; reloading new function
definitions alone does not remove that old advice.

Use companion versions that support externally managed sessions when
gptel-agent session support is loaded. The runner marks the node chat with
`gptel-agent-session-managed-externally` before gptel initializes it. Child
discussions and delegated worker transcripts also belong to the run. Leave the
user's global `gptel-agent-session-mode` setting intact. Use the runner save/open
commands for pipeline work. See [save and restore](operations.md#save-and-restore).

The runner manages handles automatically for calls and feedback requests. It
owns root scheduling and retries; gptel-agent owns descendant lifecycle and
event-driven settlement. Ordinary workflows should not call these four APIs
themselves. Only a custom host/driver needs their docstring-level contracts.

Completion and information flow:

- `Agent` workers, including `background: true` workers, finish and deliver their
  outcomes before the owning request completes.
- Active discussion turns, nested discussions, and queued discussion follow-ups
  hold a runner-owned request open. Idle discussions do not. A silent completion
  releases the held parent answer.
- `RunnerHandoff` and `RunnerAskPrevious` connect pipeline nodes in the
  workflow. `Discussion` and `DiscussionReport` operate inside a node's
  supervised request. Ordinary child discussion replies remain in the child. Request `DiscussionReport`
  explicitly when the findings must reach the parent for synthesis; it resumes
  the same request. Raw reports belong in the transcript, not the final node value.
- Pausing/cancelling the node stops its descendant work and invalidates pending
  approvals. Recovery restarts unfinished nodes rather than restoring live child
  callbacks. Inspect effects already performed before restarting.

Nested work is internal to a node, not additional runner nodes. Its calls and
concurrency are not separately charged against the runner's budgets, and nested
HTTP requests do not make `:max-requests` a provider spend cap. Use agent task
limits where appropriate. Register the outer node as a writer when its children
can write; runner locks cover its declared workspace, not every directory a
child might choose. Prefer explicit pipeline nodes when separate scheduling,
workspace locking, results, or budgets are required for each participant.

The checkout's `make integration-test INTEGRATION_LOAD_PATH="..."` exercises the
plain gptel adapter and agent delegation separately, using real state machines
with mocked transport, including a background process. Supply load paths for
gptel, gptel-agent and their dependencies. These checks complement fake-driver
workflow tests; neither proves live-provider or arbitrary custom-tool behavior.

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
contains `:id`, `:kind` (`tool-approval`), `:call-id`, `:generation`,
`:source-buffer`, and `:tools` (name/args). Live callbacks remain private to the
owning process. Subagent and discussion approvals are routed to the outer call;
several children can have different pending interaction IDs simultaneously.
Treat `:source-buffer` as a display label, not the request identity.

`gptel-runner-gptel-answer-interaction (call interaction-id approve)` answers that
exact pending interaction once. Non-nil approve executes its tools; nil cancels
the call with `tool-rejected`, whose effect on the run follows the parent policy.
It validates the generation, source buffer, request info identity and source
FSM's pending tool state, plus agent-supervision liveness when applicable.
Use `gptel-runner-gptel-interaction-pending-p (call id)` to check a selected ID;
the outer call's state alone cannot identify which child awaits approval.
Acceptance executes in the source request's context. Closing a child or
pausing/cancelling the node invalidates its pending approval; rejection cancels
the outer call and its descendants. Never reuse an ID for a later interaction or
serialize live call objects for another process. Use the jobs layer to route
answers to a background worker.

Headless mode does not grant tool approval or change the explicit start-time
confirmation policy. The policy also applies during programmatic feedback.
`AskUserQuestion` forms and separate interactive file-review interfaces still
need their own headless adapters; routing gptel tool approvals does not make
those interfaces remotely usable. Inspect the selected preset's actual tools
and hooks before treating it as suitable for unattended work.

## Pipeline file outputs

Publishing is a core pipeline API, not a background-only integration. Use
`gptel-runner-write-artifact` for generated text or `gptel-runner-publish-artifact`
for an existing file. See [managed outputs](runner-api.md#managed-outputs-in-every-launch-mode)
for signatures, descriptors, context handoffs, and retention. The runner owns
storage selection; custom drivers and workflow functions should not inspect
`gptel-runner-jobs--invocation` or duplicate output storage.

## Dashboard integrations

`gptel-runner-dashboard-entry-functions` adds externally owned rows without
registering their runs in the local scheduler. Row identifiers must be distinct.
`gptel-runner-dashboard-row-action-functions` receives `(action row-id)` for
`open` or `menu`; non-nil means handled. These are adapter hooks, not workflow
nodes. Use the installed jobs dashboard support for background invocations.
