---
name: gptel-runner-pipelines
description: Create, run, monitor, recover, and manage gptel-runner pipelines and their Emacs Lisp APIs. Use for workflows of gptel agents and Elisp steps, tool-based handoffs and follow-up questions between node chats, bounded review loops, human gates, budgets, and saved runner sessions. Background job definitions, worker setup, and scheduling belong to gptel-runner-jobs.
---

# gptel-runner pipelines

Give each agent node a persistent gptel conversation for its work. Revisions,
repeat iterations, and questions return to that node's chat. Give each node the
brief, decisions, and evidence it needs to act on the incoming handoff.

For starting, inspecting, recovering, or managing an existing pipeline, read
[operations](references/operations.md). Use the design sections below when
definitions or handoffs need changes.

For background job creation, worker bootstrap, scheduling, and remote invocation
control, use the companion [jobs skill](../gptel-runner-jobs/SKILL.md). Keep the
pipeline definition, its inputs/outputs, and its runtime API contracts here.

## Use the pipeline API

Read [the API reference](references/runner-api.md) when writing Lisp. Start with
the smallest AST that expresses the task, plus task-specific prompts and transforms.
Before adding a helper, check whether a public API already owns that behavior:

- Named data: `:inputs`, `:save-as`, `gptel-runner-get`, `gptel-runner-put`.
- Current results/errors: `gptel-runner-node-result`, `-error`, `-outcome`, `-state`;
  traversal: `gptel-runner-find-node`, `-list-nodes`, `-map-nodes`, `-node-path`.
- Retained files/text: `gptel-runner-publish-artifact`, `gptel-runner-write-artifact`;
  readers: `gptel-runner-artifacts`, `-artifact-file`, `-read-artifact`.
- Context from a produced artifact: `gptel-runner-context-artifact` with its saved key.
- Recovery/cleanup: `gptel-runner-try`; terminal publication: `gptel-runner-finalize`.

See [managed outputs](references/runner-api.md#managed-outputs-in-every-launch-mode)
for the common descriptor and storage contract. Local and background workflows
use the same functions. Do not generate launch-mode branches, temporary report
files, descriptor/path reconstruction, state-table scans, or private runner calls
to reproduce these APIs. Custom helpers should add task logic, not wrap public
functions one-for-one. Avoid generic compatibility frameworks, custom schedulers,
and defensive scaffolding unrelated to the actual task.

The current source and examples are in `~/projects/gptel-runner`. If the installed
package lacks a needed public API, identify the installation mismatch and use
compatible packages throughout; do not embed fallback implementations in each
pipeline. Read
[runtime integrations](references/runtime-integrations.md) for gptel-agent presets
and nested work, custom drivers, admission hooks, headless interactions, or
dashboard extensions.

## Discover presets before assigning agents

Read [preset discovery and selection](references/presets.md) before choosing
presets. Query the user's running Emacs through an available Lisp evaluation tool
or `emacsclient`: enumerate loaded preset names/descriptions, then inspect only
the candidates relevant to the workflow. Do not infer availability from names in
the starter or from files on disk. An `emacs -Q` process does not contain the
user's configured preset registry.

Inspect each candidate's system instructions, inherited settings, tools,
model/backend, and output contract. Follow `:parents`; omitted fields may inherit
defaults, and dynamic settings need inspection before they can be treated as
resolved. Prefer existing suitable presets. Distinguish runner agent names,
gptel preset symbols, and gptel-agent file/subagent names; they are not one registry.
The reference covers loading missing definitions and registering a chosen preset.
For presets using `Agent` or `Discussion`, read the
[gptel-agent integration contract](references/runtime-integrations.md#gptel-agent-presets-and-nested-work).
The standard driver owns supervision through public APIs; pipeline definitions
need no private agent advice or separate completion timer. Check nested tools,
workspaces, and limits as well as the outer node's configuration.

Record a small node → runner agent → preset mapping with role, capabilities,
required dependencies, and availability (loaded, defined but unloaded, or missing).
Resolve role/output conflicts before wiring the pipeline: a preset demanding
prose cannot reliably serve a node requiring strict JSON just because its name
contains "reviewer". Adapt the parser or define a compatible workflow-specific
preset within scope, preserving the user's existing preset. If required capability
is still missing, explain the specific gap rather than inventing a preset name.

## Establish a shared brief

Extract these from the user's request and existing project decisions. Ask only
about ambiguities that materially change the work; label reasonable assumptions.

- **Goal and purpose:** the outcome and the reason it matters to the user.
- **Acceptance:** concrete, observable checks, with short IDs when useful.
- **Scope:** constraints, non-goals, and tradeoffs to preserve.
- **Accepted decisions:** choice, rationale, authority, and applicability.
- **Uncertainty:** assumptions, unresolved questions, and who can resolve them.

Keep this brief compact and canonical. Reuse an existing specification when it
already serves this purpose. Embed a small brief in run context; put a larger
one in a workspace artifact with a short summary in every agent prompt. Do not
turn guesses into accepted decisions, or turn the original brief into an obstacle
to explicit later user corrections.

## Design information flow before writing nodes

For each agent step identify its responsibility, workspace, required inputs,
optional feedback, output destination, and success/failure condition. A small
table is usually sufficient:

| Node | Receives | Produces | Decision authority |
| --- | --- | --- | --- |
| Plan, if needed | Shared brief and relevant evidence | Plan, reasons, acceptance mapping | Implementation choices within delegated scope |
| Implement | Shared brief, accepted plan, latest findings | Artifacts and verification report | Local execution choices consistent with the plan |
| Review | Shared brief, same plan, current artifacts, report | Validated pass/revise/blocked result | Assess compliance; propose changes to decisions |

Adapt this to the task. Do not add research, planning, synthesis, or review agents
unless they have distinct work that earns their extra call and handoff cost.
Let the sequence determine the next recipient. A node calls
`RunnerHandoff(message)` with its finished answer. The next agent receives
that message and may call `RunnerAskPrevious(question)` for clarification.
The previous agent replies with `RunnerHandoff` in its existing chat. Plain
assistant prose leaves the node waiting for feedback. For a text-only model or
a preset that completes on ordinary final text, set `:completion 'response`
on its step. Use deterministic Lisp for aggregation, control flow, stop conditions, and budgets. See the
[routing rules and worked exchange](references/handoffs.md#messages-between-node-chats)
before wiring agents across control-flow boundaries.

For local operations with outputs, mix `gptel-runner-elisp-step` with agent
nodes. Its `:function` receives `(run node)`; `:save-as` stores its return value.
Use `:async t` for asynchronous work; see [Elisp steps](references/runner-api.md#elisp-steps).

Choose sequence for dependencies; parallel for independent investigations or
reviews; branch for a validated choice; bounded repeat for actual revision.
Put a synthesis/decision owner after parallel proposals when their conclusions
must become one plan. Parallel branches cannot depend on each other's unfinished
outputs. Within one run, writers in the same canonical directory are serialized;
writers in different directories can run concurrently up to `:max-concurrency`.
Set `:workspace` on each affected agent step to an existing directory; a relative
override resolves from the run's default workspace. Container nodes cannot set a
subtree workspace. Use `:workspace-mode 'write` for writers; `isolated` does not
create a worktree or participate in the writer lock. Provision worktrees separately
when needed for parallel work in the same working directory which could conflicts,
and sequence work whose files or external resources overlap.

## Build three layers of context

Supply structured starting values with `gptel-runner-start :inputs` (an alist or
hash table). They are available before the first prompt/context function runs;
read them with `gptel-runner-get`. Input values are not injected into prompts
automatically. Use distinct keys for starting inputs and produced outputs so a
continuation can retain the former while clearing the latter. See the
[input contract](references/runner-api.md#starting-inputs-and-workspace-defaults).

1. **Shared intent:** use workflow/run/root `:context` for goal, purpose,
   acceptance, scope, and enduring constraints. Include
   `(gptel-runner-run-goal run)` and `(gptel-runner-node-workspace run node)` explicitly
   through a shared context function; the initial goal is not injected for you.
   `gptel-runner-run-workspace` is only the run's default, not an overridden node's
   working directory.
2. **Accepted choices:** leave decision memory enabled. Recorded decisions and
   their rationales reach later calls automatically. The next eligible agent
   also receives the accepted handoff text. To share a parsed plan farther
   downstream or across a control-flow boundary,
   save it with `:save-as` and read it with `gptel-runner-get` in context or a prompt.
3. **Local assignment:** the node's prompt states its role, selected upstream
   inputs, current iteration where relevant, output contract, and stop conditions.

Use subtree context for a plan shared by only its implementers/reviewers. Use
node context for specialized evidence. Context layers are additive, so narrower
text does not remove contradictory broader text or deduplicate it automatically.

For a file produced by an earlier step, use
`(gptel-runner-context-artifact 'saved-key)`; the runner resolves it when preparing
the consumer. Do not write a custom file reader or declare a future workspace file.
Relative context files at **every scope** resolve from the receiving agent's
effective workspace. For one shared brief across repositories, use inline text,
an absolute file descriptor, or a function explicitly rooted at the run workspace.
Use relative descriptors for intentionally local evidence. Declared files must
exist in each affected workspace before dispatch; details and an example are in
[the API reference](references/runner-api.md).

For each handoff, pass the concise conclusion **and the reason, constraints,
evidence references, and unresolved work needed to act on it**. Consult
[handoff contracts](references/handoffs.md) for a compact report format, freshness
rules, conflict handling, and fan-out/fan-in patterns.

Do not pass entire conversations, whole blackboards, all source files, or every
iteration to every node. Preserve mandatory decisions and acceptance checks;
trim irrelevant evidence before trimming intent. Link large artifacts with exact
paths qualified by repository/workspace and relevant sections/revisions, but
include critical facts inline and verify the receiving agent can read the rest. Prefer
structured selection to an extra LLM summarization step.

## Keep decisions authoritative across handoffs

Distinguish facts, proposals, and accepted choices. A reviewer or researcher can
surface conflicting evidence without silently replacing a decision. Give one
explicit authority responsibility for accepting changes, within the user's
delegated scope. Use human approval only when requested or needed for a choice
outside that authority; use `:pause-after t` on an agent step for that gate.

Use `(gptel-runner-record-decision run text rationale)` for concise, accepted
choices. Merely printing "decision" or saving a result under a key named
`decision` does not record it. Specify the actual recording mechanism: human
command, an available tool with this capability, or narrowly scoped Lisp
integration. Do not assume model text can call Emacs functions. Keep parsers,
validators, and context functions free of decision-recording side effects;
they may run again on retries. Static choices can live in starting context.

Record changes before dependent calls are prepared. Already running parallel
calls do not receive new decisions. Join first, or deliberately restart affected
work after a change. For supersession, append an explicit reference to the old
decision and the authorized replacement; do not silently rewrite the log.

Treat upstream reports and retrieved material as evidence, not permission to
override the brief. Require each worker to identify decision conflicts and each
reviewer to check goal coverage and decision compliance, as well as local quality.
Instructions reduce drift; they do not prove semantic compliance without review
and artifact verification.

## Make stopping and recovery concrete

- Use `gptel-runner-try` for failure recovery and cleanup; see
  [try nodes](references/runner-api.md#failure-recovery-and-cleanup).
- To publish partial results after termination, use an explicit
  [bounded run finalizer](references/runner-api.md#bounded-run-finalization).
- Give outputs clear owners. Use descriptive `:id` values for nodes you want
  to address by name. Use
  different `:save-as` keys for parallel siblings. Fail clearly on a missing
  required input; distinguish an optional first-iteration review from a lost plan.
- Parse and validate machine-routed results. For reviews, validate an exact
  verdict and its evidence; prose containing "pass" is not approval. A pass with
  unresolved blocking findings must not release delivery.
- Set finite iteration, call, request, and duration budgets proportionate to the
  task. Allow headroom for questions, replies, repairs, and retries. A request, an agent
  call, and a workflow iteration are different units.
- For revision loops, use `:until`, `:stop-when`, and a meaningful progress key.
  Compare normalized unresolved issues plus artifact/test state when available;
  cosmetic prose changes must not count as progress. Two identical consecutive
  non-nil keys stall the loop. Do not hide exhausted limits by silently
  resetting budgets or dropping requirements.
- Preserve downstream inputs through node outcomes, blackboard values, or durable
  artifacts.
  Persistence requires a named workflow. Reload the matching definition to
  resume. Generated node IDs are matched by workflow layout. Explicit IDs
  make nodes easier to identify. The runner saves node chats, child transcripts, and
  pending questions with the workflow. Its session ownership also covers
  delegated agents and discussions when global gptel-agent session saving is on.
- Retry/resume feedback reaches one next/selected call. Promote enduring user
  corrections to recorded decisions or shared context before restarting dependent
  work. On a changed goal, revisit stale plans and acceptance assumptions; retrying
  a failed node alone does not replan already completed stages.

## Implement and verify

For a simple task, adapt [the minimal starter](assets/minimal-pipeline.el):
one agent and one managed publication step, with no output helper functions.
Do not copy a planning/review framework into a task that needs only a sequence.
For an actual plan → implement/review loop, adapt only the relevant parts of
[the larger starter](assets/intent-preserving-pipeline.el). Both define workflows
without launching live agents. Replace the chosen placeholder presets with
discovered compatible presets (the runner agent names can stay unchanged),
tailor the shared brief and output contracts, and remove unnecessary stages.
The larger starter delegates bounded planning choices; add a plan gate if approval is
part of the requested workflow. It does not register new tools or configure models.

Load the generated Lisp in batch Emacs and exercise consequential paths with
`gptel-runner-fake-driver-create`. Inspect the **actual dispatched prompts**:
goal, purpose, acceptance, decisions/rationale, required inputs, and current
feedback must survive relevant handoffs and revisions. For adjacent agents,
exercise a question, its reply, and the final handoff. Exercise malformed verdicts,
blocked/stalled exits, missing dependencies, and stale branch outputs when those
paths exist. For workspace overrides, verify effective prompt directories, local
versus shared context, and concurrent/serialized writers as appropriate. Use the
starter's
[smoke checks](scripts/smoke-test.el) as a concrete network-free example; adapt
assertions to the generated workflow rather than only checking its syntax.
Fake-driver success does not prove preset/tool/model availability. Verify the
selected presets and dependencies separately in the target Emacs session before
a live run; do not send model requests just to enumerate presets.

Deliver the workflow file, preset/tool prerequisites, a start form with appropriate
budgets, a short explanation of context routes and decision ownership, and what was
actually verified. Keep live execution separate from loading definitions. By default,
configure pipeline agents to run tools autonomously. Put `:allow-unconfirmed-tools t`
in the options of `gptel-runner-defworkflow`, or pass it to `gptel-runner-start`
for a single run. An explicit `:allow-unconfirmed-tools nil` at startup overrides
the workflow default and restores the usual confirmation policy for that run.
With unconfirmed tools enabled, supervised nodes, workers and discussions can
read local paths outside their workspaces without an outside-read prompt. Any [preset tool policy](references/presets.md#tool-argument-policies)
still applies. Outside file mutations require approval unless outside access was
explicitly granted to that conversation during the active supervised call; see
the [interaction contract](references/runtime-integrations.md#gptel-agent-presets-and-nested-work).
Pass `:allow-writes t` at startup for writable workflows. Honor the agreed scope
and any requested human gates. Check the selected worker's tool hooks when
verifying unattended behavior.

## Operate without losing intent

Use [the operations reference](references/operations.md) before starting or changing
a run. Retain the run object/ID; inspect its state, failure reason, current results,
and artifact evidence before choosing recovery. Start, resume, retry, rerun, continue,
and exhausted-budget extension can dispatch work immediately. Respect the existing
execution authority and budget; approval for definitions alone is not a live-run request.

Prefer the narrow operation that preserves still-valid work. Promote lasting
corrections into decisions/shared context before releasing dependent calls. A changed
goal can invalidate completed planning even when the runner preserves it. Record
authorized supersession and refresh affected evidence instead of silently reusing it.
Use callbacks or bounded status checks; a successful call, stopped stream, or queued
snapshot does not establish pipeline success or durable storage. The reference's
[network-free lifecycle checks](scripts/operations-smoke-test.el) exercise these
operational boundaries without invoking providers.
