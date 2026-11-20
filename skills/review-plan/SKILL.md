---
name: review-plan
description: Review an implementation plan for regressions, security, and codebase fit before coding.
disable-model-invocation: true
---

# Review a plan

Review the proposed implementation before coding. Work read-only: return findings and proposed plan edits without changing files, rewriting the plan, or starting implementation.

## 1. Identify the plan

Use the plan supplied in the invocation, the referenced file, or the latest unambiguous plan in the conversation, in that order. If the plan or target repository is missing or ambiguous, ask which to review. Preserve the plan's goals, non-goals, constraints, and settled decisions. Flag contradictions rather than silently resolving them.

## 2. Launch three independent reviewers

Start three read-only research subagents in parallel. Give each the same standalone review packet: plan text or exact file reference, repository and known revision, scope, constraints, settled decisions, and its assigned axis. Do not rely on the workers having the conversation or each other's reports.

- **Regression — what could break outside the intended scope?** Trace changed contracts, shared state, or persisted data to consumers outside the planned change. Check compatibility across callers, configurations, and rollout states where relevant. For each material risk, name the affected consumer, the behavior it relies on today, how the plan could break it, and the focused regression check to add.
- **Security — what must be enforced, and where?** Trace affected trust boundaries and sensitive data. Check authorization, tenant isolation, validation, secret handling, and exposure through outputs or logs where relevant. For each material risk, name the missing invariant, the component that must enforce it, the required deny or failure behavior, and a negative test that verifies the control. Keep the review defensive; do not develop exploits.
- **Standards — what would be expensive to undo after coding?** Read applicable repository instructions and compare the plan with the closest maintained implementations and tests. Check module boundaries, responsibility, reuse, naming, error handling, and test placement where relevant. Distinguish explicit rules from inferred conventions. Cite the rule or precedent and propose the exact layout, interface, or dependency correction. Surface conflicting precedents instead of treating a preference as a rule.

Require each reviewer to return the surfaces inspected, material findings, and coverage gaps. Each finding must identify the plan step, cite repository evidence (`path:line` where available), explain the consequence, propose a specific plan edit, and state how to verify it. Distinguish facts about current code from predictions about the proposed design. If there are no findings, state what was checked; if evidence is missing, report the gap instead of guessing.

## 3. Synthesize the review

Wait for all three reports. If a reviewer cannot run or finish, mark that axis unreviewed and explain the gap. Verify evidence behind blockers, merge duplicate findings while retaining their axis labels, and surface conflicting recommendations as a decision with tradeoffs. Keep unsupported concerns under open questions rather than presenting them as confirmed defects.

Return:
1. **Verdict:** `Ready based on reviewed scope`, `Revise before implementation`, or `Review incomplete`. Give the reason in one sentence.
2. **Findings:** Blockers first, then other material findings. For each, include the axis, affected plan step, evidence, consequence, exact proposed edit, and verification.
3. **Coverage and open questions:** Give a short result for each axis, including axes with no findings. For each material unknown, name the smallest check or decision that would resolve it.

Stop after the review. Leave plan revisions and implementation to the user.
