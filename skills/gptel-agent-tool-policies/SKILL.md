---
name: gptel-agent-tool-policies
description: Write, test, and debug gptel-agent tool policies (`gptel-agent-define-tool-policy`, `:agent-tool-policy`) for agents, pipelines, and jobs. Use for unexpected tool denials, scoped delegation and oversized results, read-only Bash/glab/git or MCP, and file-access restrictions.
---

# Tool policies for autonomous agents

A policy is a trusted **allowlist** of tool calls plus optional denials. Selected by a preset
(`:agent-tool-policy 'name`), it applies even with `:allow-unconfirmed-tools t`, to retries,
and to every child agent/discussion (children inherit it and cannot widen it).
Reference implementation: `~/projects/back/review-referal-bonus-mrs.el` (policy `batch-mr-review`).
Source: `~/.emacs.d/straight/repos/gptel-agent/gptel-agent-policy.el` (docstring of
`gptel-agent-define-tool-policy` has the full pattern syntax).

## Semantics (memorize, do not re-derive)

- A tool with **no allow rule is denied** (verified with `gptel-agent-policy--allows-p`). Mentioning a tool in `:allow` with no `:args` allows all its arguments.
- Any matching `:deny` beats any `:allow`.
- Only the keys you mention are constrained; unmentioned arguments are free.
- Keys are the tool's **argument names** with a leading colon (`:command`, `:file_path`, `:path`, `:root_directory`, `:subagent_type`). Check real names with
  `(mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args (gptel-get-tool "Bash")))`.
  A key that is not in the tool's schema makes the rule never match (fail closed).
- Patterns: JSON literal, `(rx ...)` (whole-string match, case-sensitive), `(one-of ...)`, `(optional P)`, `(under ROOT...)` (paths, symlink/`..` safe), `(url-origin ...)`, `(object ...)`, `(every P)`.
  Booleans are `true`/`false`; an optional boolean arg is `(optional false)`.
- Paths: `(under workspace)` or absolute directory strings. Use `expand-file-name` for `~`.
- Policy names are symbols; redefining a name replaces it for new requests. Running chats keep their frozen version.

## Workflow (do it in this order, once)

1. Inventory required calls by caller, workspace, and effective schema. Include request-local tools supplied to children and follow-up calls: the preset `:tools` list alone misses them. Every required call must be both exposed and admitted by the inherited policy.
2. Write `:allow` per tool, as narrow as the task permits; put dangerous sub-cases in `:deny`.
3. Select it in **every** preset involved: coordinator preset AND `gptel-agent-register`ed sub-agents (`:agent-tool-policy 'name`). A missing one is an unprotected agent.
4. Define the policy **before** the presets/registrations (it is read at request time but keep it first for clarity).
5. Test with `gptel-agent-policy--allows-p` (below) before any real run. Never test by launching a model run.
6. Run the project's existing tests; reload the file in `Eval` before re-testing (a stale definition gives all-`nil` results, the classic false alarm).

## Diagnose a denied run

- Inspect saved events, node chats, and child transcripts. Extract the **exact tool name, full arguments, and workspace**. Reports may omit shell wrappers or flags; a `succeeded` runner state can still publish blocked reviews.
- Replay the raw call and a known-good variant against the worker's policy and effective schema. Distinguish a missing allow entry, a matching deny, and an argument/schema mismatch. If a standalone read passes but a compound call fails, document the supported form rather than loosening the shell rules.
- Validate a job's saved bootstrap in fresh batch Emacs with provider connections and any load-time inventory probes stubbed, and with disposable test storage. For an authorized live smoke test, start a fresh invocation and check that children actually start, return evidence, and deliver reports to the parent.

## Test harness

```elisp
(load "/path/to/definitions.el" nil t)   ; reload first!
(let* ((r "/workspace/") (c (list (cons 'my-policy r))))
  (mapcar (lambda (args) (and (gptel-agent-policy--allows-p c (gptel-get-tool "Bash") args r) t))
          '((:command "glab mr list" :query "q") (:command "glab mr note 3 -m hi" :query "q"))))
```
Test every allowed and every must-deny case in one batch (positive and negative), plus path edge cases (`..`, `.env`, symlink, outside root).

## Recipes

**Bash read-only.** Allow with one `rx` anchored on the command prefix, then an argument rx that bans shell metacharacters; permit pipes only into a fixed filter list.
Ban `; & | < > $ \` ( ) newline` outside quotes. Deny, separately, options that turn a read command into a write:
- `glab api`: `-X/--method`, `-f/-F/--field/--raw-field`, `--input`, `graphql`, `--hostname`. Clustered short flags (`-XPOST`) need `" -" (* letter) (any "XfF")`.
- `git`: `--output`, `--ext-diff`, `--textconv`, `--no-index`, `-O`. Allow only read subcommands (log show diff rev-parse ls-tree cat-file grep blame ...).
- Do not allow `glab mr note|approve|merge|update|create`, `glab issue note`, `git fetch|checkout|commit|push|config`.
Remember `Bash` also takes `query` and `background`; leave `query` free, pin `:background (optional false)`.

Put known-good forms in the prompts that issue Bash calls: one foreground read from the node workspace per call, with API paths containing query parameters quoted. With the corresponding allow rules, use `glab api 'projects/:id/merge_requests?state=opened&per_page=100' --paginate` or `glab mr list --output json --per-page 100 --page 1`. Use separate calls for later pages. Prefer `--output` if the policy denies `-F`. Keep `cd … &&`, loops, `2>&1`, and standalone shell processing of result files out of the workflow. A denied read or truncated preview is not an empty inventory.

**File reads.** `(under ROOTS...)` on `Read :file_path`, `Grep :path`, `PathInfo :path`, `Glob :root_directory (optional ...)`.
Allow the project tree and relevant skill dirs (e.g. `~/.emacs.d/skills/` and `~/.agents/skills/`) when agents read skill references. Add a spool root to ordinary `Read`/`Grep` only if direct spool access is required; the built-in result explorer does not need it. Deny secrets by basename component: `.env .git .m2 .ssh .gnupg .aws .config/gcloud`. Do not allow `Write/Edit/Insert/Mkdir/Rmdir/Eval`: omitting them is enough.

**Delegation tools.** Explicitly allow every stage the workflow uses. Constrain `Agent :subagent_type (one-of ...)`, `:directory (optional (under ROOT))`, and `:result_file (optional (under SPOOL-ROOT))`. For read-only discussions, use matching rules such as:

```elisp
(:tool "Discussion" :args (:preset "my-read-only-preset" :directory (under workspace)
                           :worktree (optional false) :model (optional "")))
(:tool "DiscussionControl" :args (:action (one-of "list" "read" "send" "wait")))
(:tool "DiscussionReport")
```

Allow `AgentControl` if the parent collects agent outcomes. The runner authorizes `RunnerHandoff` itself; add `RunnerAskPrevious` only if needed. Test a valid create and report path as well as denied worktrees, sibling directories, other presets/models, and cleanup. Use `:json-false` as the submitted argument in Lisp tests; `false` is the policy pattern.

**Oversized results.** Allowing `Agent` with subtype `result-explorer` only admits the launch. Add `(:tool "ResultRead")` and `(:tool "ResultGrep")` to the inherited policy, not to ordinary preset tool lists. The built-in explorer supplies these tools; neither takes a file path. Its harness validates the exact registered `result_file`/`query` pair and reads only that file. Constrain `Agent :result_file` to the worker's actual spool root (normally `gptel-agent-temp/` under the worker's temp directory). Test launch admission and both child read calls, plus a mismatched query or an unregistered file.

**Skill tool.** `(:tool "Skill" :args (:skill "glab"))` or `(one-of ...)`: otherwise agents may load unrelated skills.

**MCP tools (read-only).** Do not hand-prune `gptel--known-tools`.
1. Define the servers in `mcp-hub-servers` (in a `-Q` worker, in the job init file; token from `ATLASSIAN_TOKEN` or parsed from `~/.emacs.d/config/custom.el`, never pasted into repo files).
2. Keep one allowlist of tool names per server and connect through mcp.el: `(gptel-mcp-connect '(("srv" "tool1" "tool2") ...) t)` (non-function callback = synchronous).
3. Put the names in the presets' `:tools` (filter with `(ignore-errors (gptel-get-tool n))`). The agent only sees what the preset lists.
4. Policy: the engine denies any tool without an allow rule (verified: a policy with no `AgentControl` rule returns nil for it), so each listed tool still needs an allow entry. Generate them from the same list (`(mapcar (lambda (n) (list :tool n)) names)`) rather than writing them by hand; use `:args`/`:deny` only where a real restriction exists.
5. Note that `gptel-mcp-connect` registers every tool of the server in gptel (`gptel-get-tool` finds mutating ones); only the filter decides what is activated. The preset list and policy are what keep agents away from them.

## Jobs and runner

- Policy and presets are loaded by the job's `:init-file` (via the pipeline file); the worker re-reads files on every launch, so **no job re-registration is needed** after editing them. A saved node chat created before the policy existed keeps old settings: start a new invocation.
- Changing launchd-relevant settings (`:environment`, `:emacs`) needs `gptel-runner-job-schedule` again; policy edits do not.
- Preserve task instructions. Update tool-use guidance when the task and policy otherwise disagree; keep enforcement in the policy.
- Do not rely on `:workspace-mode 'read`: the runner does not enforce it.

## Pitfalls that cost rounds

- Testing before reloading the file (everything returns nil).
- Testing with nonexistent paths: `under` canonicalizes real paths, so use files that exist.
- Over-broad `rx`: `(* anything)` after a prefix lets `; rm -rf` through. Always anchor and exclude metacharacters.
- `timeout` is absent on macOS: do not use it in verification commands.
- Policy cannot see inside a command: for Bash it only matches the string. Keep the allowlist small rather than trying to parse shell.

## Checklist before declaring done

- [ ] Every job preset/sub-agent has `:agent-tool-policy`; required calls are exposed and admitted at every stage, including child result reads and reporting.
- [ ] Allowed + denied sample matrix run through `gptel-agent-policy--allows-p` (include `..`, secrets, write/post commands, mutating MCP tools).
- [ ] Batch init check: mutating MCP tools absent, read ones present.
- [ ] Project test suite passes after reload.
- [ ] Say whether a fresh invocation was tested end to end; distinguish actual workflow coverage from runner completion.
