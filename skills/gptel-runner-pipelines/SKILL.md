---
name: gptel-runner-pipelines
description: Create, run, monitor, recover, and manage gptel-runner pipelines in Emacs Lisp. Use for workflows of gptel agents, explicit context and decision handoffs, bounded review loops, human gates, budgets, and durable resume while preserving the user's goal, constraints, and accepted decisions.
---

# gptel-runner pipelines

Build the smallest useful workflow whose agents can act from fresh, independent
contexts. Every agent should understand **what success means, why it matters,
what has already been decided, and what its own contribution must accomplish**.
Preserve intent explicitly; do not rely on the agents remembering a conversation.

For an existing pipeline, go to [running and managing pipelines](references/operations.md).
It covers preflight/start, inspection, an operation-selection table, human feedback,
recovery, budgets, snapshots, and cleanup. Keep creation and operation in this
skill; read the design sections below when definitions or handoffs need changes.

## Ground the implementation

Locate the installed gptel-runner source, its README, and relevant examples.
On this machine the development checkout is normally
`~/projects/gptel-runner`; in Emacs, `(locate-library "gptel-runner")` identifies
the discoverable library, while `(symbol-file 'gptel-runner-start 'defun)` locates
the loaded start definition. Check for a different loaded version before relying
on checkout-only features. Follow applicable project instructions.

Read [the API reference](references/runner-api.md) when writing the Lisp. Verify
the constructors/options you use against the local source. Architecture proposals
such as `overall_description_must_read.md` describe intent, including features
that may not exist. Do not invent gate, transform, dialogue, dependency, or merge
nodes. Use public runner APIs, keeping private gptel calls out of workflows.

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
Use deterministic Lisp for routing, aggregation, stop conditions, and budgets.

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
when needed, and sequence work whose files or external resources overlap.

## Build three layers of context

1. **Shared intent:** use workflow/run/root `:context` for goal, purpose,
   acceptance, scope, and enduring constraints. Include
   `(gptel-runner-run-goal run)` and `(gptel-runner-node-workspace run node)` explicitly
   through a shared context function; the initial goal is not injected for you.
   `gptel-runner-run-workspace` is only the run's default, not an overridden node's
   working directory.
2. **Accepted choices:** leave decision memory enabled. Recorded decisions and
   their rationales reach later calls automatically. A plan saved with `:save-as`
   still needs a context/prompt function that reads it with `gptel-runner-get`.
3. **Local assignment:** the node's prompt states its role, selected upstream
   inputs, current iteration where relevant, output contract, and stop conditions.

Use subtree context for a plan shared by only its implementers/reviewers. Use
node context for specialized evidence. Context layers are additive, so narrower
text does not remove contradictory broader text or deduplicate it automatically.

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

- Give every node a stable, unique `:id` and every output a clear owner. Use
  different `:save-as` keys for parallel siblings. Fail clearly on a missing
  required input; distinguish an optional first-iteration review from a lost plan.
- Parse and validate machine-routed results. For reviews, validate an exact
  verdict and its evidence; prose containing "pass" is not approval. A pass with
  unresolved blocking findings must not release delivery.
- Set finite iteration, call, request, and duration budgets proportionate to the
  task. Count repairs and retries in call/request headroom. A request, an agent
  call, and a workflow iteration are different units.
- For revision loops, use `:until`, `:stop-when`, and a meaningful progress key.
  Compare normalized unresolved issues plus artifact/test state when available;
  cosmetic prose changes must not count as progress. Two identical consecutive
  non-nil keys currently stall the loop. Do not hide exhausted limits by silently
  resetting budgets or dropping requirements.
- Persist downstream inputs on the blackboard or in durable workspace artifacts.
  Use a named workflow and explicit IDs for `:persist t`; re-load the same
  definitions to resume. Worker transcripts are not durable shared memory.
- Retry/resume feedback reaches one next/selected call. Promote enduring user
  corrections to recorded decisions or shared context before restarting dependent
  work. On a changed goal, revisit stale plans and acceptance assumptions; retrying
  a failed node alone does not replan already completed stages.

## Implement and verify

For a plan → implement/review loop, adapt
[the executable starter](assets/intent-preserving-pipeline.el). It defines a
workflow without launching live agents. Replace its placeholder preset values
with discovered compatible presets (the runner agent names can stay unchanged),
tailor the shared brief and output contracts, and remove unnecessary stages.
The starter delegates bounded planning choices; add a plan gate if approval is
part of the requested workflow. It does not register new tools or configure models.

Load the generated Lisp in batch Emacs and exercise consequential paths with
`gptel-runner-fake-driver-create`. Inspect the **actual dispatched prompts**:
goal, purpose, acceptance, decisions/rationale, required inputs, and current
feedback must survive relevant handoffs and revisions. Exercise malformed verdicts,
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
actually verified. Keep live execution separate from loading definitions. Enable
`:allow-writes t` for intended writers; do not disable tool confirmation unless the
user requested it. Workspace modes alone do not restrict tool capabilities.

## Operate without losing intent

Use [the operations reference](references/operations.md) before starting or changing
a run. Retain the run object/ID; inspect its state, failure reason, current results,
and artifact evidence before choosing recovery. Start, resume, retry, continue,
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
