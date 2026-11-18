# Handoffs that preserve intent efficiently

## Messages between node chats

In the standard gptel driver, handoff completion is the default. Use a
tool-capable model. The driver supplies the applicable workflow tools on each
request. Send one workflow tool call by itself with a nonblank string.

1. The researcher calls `RunnerHandoff(message)` with its findings. After the
   node's parser, validator, and any approval gate accept the result, the planner
   receives that text alongside its own assignment.
2. If evidence is missing, the planner calls `RunnerAskPrevious(question)`.
   The researcher receives the question in its existing chat and can use its
   regular tools. Its `RunnerHandoff(message)` returns a reply to the planner.
   The original research result and its saved value remain available.
3. The planner resumes in its chat. It can ask again or call `RunnerHandoff`
   to submit its plan. The last agent uses the same tool to submit its result.
   Subsequent Elisp nodes can consume or publish the parsed output.

The sequence determines the recipient. Nested sequences and intervening Elisp
steps preserve adjacency. A question or reply leaves completed Elisp steps in
place. A branch, parallel group, repeat, other non-sequence container, or agent
with response completion bounds the route. Two agents in a sequence *inside*
one repeat body can exchange messages there. Supply a plan from outside the
repeat, or a finding from an earlier iteration, through an explicit saved value
or artifact. A follow-up can travel farther upstream when the responding agent
itself asks its previous partner. Each turn uses normal call and request budgets.

`:save-as` stores the parsed value for code. The text that another chat receives
is the accepted handoff string. For a machine-routed JSON review, ask for JSON
inside `RunnerHandoff`'s `message` and validate it. Avoid duplicating an incoming
handoff in the receiving prompt unless the task needs a different representation.
Plain prose leaves the node open for another turn or manual acceptance.

## Shared brief

Use a short versioned brief when the task is large enough to benefit from IDs:

```text
Goal G1: Add resumable batch import.
Purpose: Operators must recover interrupted imports without redoing accepted work.
Acceptance A1: Restart skips completed items. A2: Failed items remain retryable.
Constraint C1: Preserve the current CLI. Non-goal N1: No new dashboard.
Decision D1 (accepted by user): Use the existing SQLite store.
Reason: The store already provides atomic item updates; no extra service is needed.
Assumption U1: Input item IDs are stable; verify this before implementation.
```

This is a format example, not a default product requirement. Keep the brief in
one maintained source. Every call receives its essential content, not merely
"follow the plan" or a link it cannot open. Preserve rationale so agents can
make sensible local choices when the brief cannot prescribe every detail.

## Node assignment

Give each agent a bounded responsibility:

```text
Your contribution: implement checkpoint updates for A1/A2.
Authority: choose local structure consistent with C1 and D1.
Required inputs: accepted plan v2 and current source files.
Feedback: unresolved review issues I2/I4 from the previous iteration.
Return: changed artifacts, verification evidence, unresolved risks, next action.
Conflict: report the affected decision and evidence; do not silently change it.
```

Do not require private reasoning transcripts. Ask for concise conclusions,
decision rationale, and evidence that another agent can verify.

## Output contract

Use prose sections when downstream consumption is descriptive. Use parsed JSON
or a validated plist when fields control branching, approval, or completion.
Adapt this envelope to the workflow; these are **application-defined fields**,
not built-in runner keys:

```text
Producer / workspace or repository / round / brief version:
Status: completed | needs-revision | blocked
Result: concise useful outcome
Decision compliance: IDs followed; conflicts or proposed changes, if any
Evidence: artifact path + section/revision; command + outcome; fact + source
Remaining: unresolved findings, uncertainty, blocker
Next consumer: exact next action and required inputs
```

Set provenance (node, iteration, input version/digest) deterministically where
possible instead of trusting a model's claimed identity. A parser receives only
its value, not the run; perform run-aware comparisons in predicates or consumer
helpers. Adding custom keys to a node does not make the runner enforce them.

For pipeline-generated reports, save the descriptor returned by
`gptel-runner-write-artifact`/`gptel-runner-publish-artifact`. Feed it to a later
agent with `gptel-runner-context-artifact`, or read it with `gptel-runner-read-artifact`;
no workspace-relative path or custom file-reading helper is needed.

When nodes use different workspaces, qualify workspace artifact paths by their source
repository/root and revision, or use absolute paths when accessible. A bare
`src/importer.el` can name different files for different agents. The blackboard
and decision log remain shared within the run; workspace overrides do not copy
files, merge changes, or grant tools access to another repository. Keep global
goals/decisions in shared context and repository-specific evidence local. Send
reviewers to the actual writer workspace or explicitly identify all directories
they must inspect.

Keep stable IDs on unresolved issues so later agents can distinguish fixed,
deferred, and newly found problems. Carry critical uncertainty and dissent forward;
do not convert a proposal into consensus by omitting the caveat from a summary.

## Context selection

For each consumer, select:

1. The shared brief, accepted decisions/rationales, and accepted plan where relevant.
2. Direct prerequisites and unresolved feedback needed for that consumer's job.
3. Exact references to larger supporting artifacts, with critical excerpts inline.

A report can often fit in a few hundred words; choose the limit from the task,
not an arbitrary universal ceiling. Reserve room for evidence the agent must read
itself. Large histories belong in artifacts or blackboard archives. Give a final
reporter the selected history; give a revising worker current work and unresolved
findings. Do not progressively summarize a summary of a summary as the sole record
of accepted decisions.

If context is too large, remove unrelated evidence, replace full logs with failure
excerpts and exact paths, then summarize old resolved findings. Preserve current
acceptance criteria, hard constraints, accepted choices, reasons, and blockers.
If those cannot fit, split responsibilities instead of silently dropping them.

File references require real access: a reviewer cannot validate an artifact with
only a conversational summary and no read tools. For coding tasks, actual files
and executed checks are stronger evidence than an upstream success claim.

## Parallel proposals and synthesis

Give independent branches the same brief and decision baseline. Each writes its
own output key, identifies its evidence, and labels conclusions as proposals.
After joining, a single synthesis owner compares tradeoffs against acceptance
criteria and resolves contradictions. Save one canonical outcome and its reasons.
Route it to all dependent stages and record accepted enduring decisions through
the chosen recording mechanism.

For optional failed branches, synthesis receives explicit missing/failed status.
Do not silently turn missing evidence into agreement. For required independent
reviews, require all relevant current-round verdicts. Passing technical checks
while violating the user's scope or decisions is still a revision.

## Changes and recovery

An agent finding new evidence should identify the affected decision, explain the
conflict, and propose the smallest change. A designated authority accepts it or
returns a correction. If an unapproved decision change is necessary to proceed,
surface a blocked result rather than making the change invisible.

Record an authorized replacement with an explicit supersession reference and
rationale, then refresh affected plan/context artifacts. Avoid contradictory
copies of the old choice in immutable prompt strings. A user correction takes
precedence over earlier task decisions; retained decisions do not veto it.

After retry, resume, changed goal, or workspace edits, validate required inputs
against the current brief/artifacts. A saved result can be present but stale.
If conditional branches skip a producer, use current-round history or explicit
freshness checks. A final result should report goal coverage, decision compliance,
verified artifacts, and remaining limits; it should not simply repeat the last
agent's optimism.
