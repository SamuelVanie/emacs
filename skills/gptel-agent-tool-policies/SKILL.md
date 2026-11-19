---
name: gptel-agent-tool-policies
description: Write, test and wire gptel-agent tool policies (`gptel-agent-define-tool-policy`, `:agent-tool-policy`) that restrict what autonomous agents, runner pipelines and background jobs may do. Use when asked to sandbox agents, forbid posting/writing, allow only read-only Bash/glab/git, limit file reads, or expose MCP tools read-only.
---

# Tool policies for autonomous agents

A policy is a trusted **allowlist** of tool calls plus optional denials. Selected by a preset
(`:agent-tool-policy 'name`), it applies even with `:allow-unconfirmed-tools t`, to retries,
and to every child agent/discussion (children inherit it and cannot widen it).
Reference implementation: `~/projects/back/review-referal-bonus-mrs.el` (policy `batch-mr-review`).
Source: `~/.emacs.d/straight/repos/gptel-agent/gptel-agent-policy.el` (docstring of
`gptel-agent-define-tool-policy` has the full pattern syntax).

## Semantics (memorize, do not re-derive)

- A tool with **no allow rule is denied**. Mentioning a tool in `:allow` with no `:args` allows all its arguments.
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

1. List the tools the agents really need (read the preset `:tools`) and their argument names (snippet above).
2. Write `:allow` per tool, as narrow as the task permits; put dangerous sub-cases in `:deny`.
3. Select it in **every** preset involved: coordinator preset AND `gptel-agent-register`ed sub-agents (`:agent-tool-policy 'name`). A missing one is an unprotected agent.
4. Define the policy **before** the presets/registrations (it is read at request time but keep it first for clarity).
5. Test with `gptel-agent-policy--allows-p` (below) before any real run. Never test by launching a model run.
6. Run the project's existing tests; reload the file in `Eval` before re-testing (a stale definition gives all-`nil` results, the classic false alarm).

## Test harness (use as is)

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

**File reads.** `(under ROOTS...)` on `Read :file_path`, `Grep :path`, `PathInfo :path`, `Glob :root_directory (optional ...)`.
Always include: the project tree, `temporary-file-directory` (oversized tool results are spooled there for `result-explorer`), and the skill dirs `~/.emacs.d/skills/` and `~/.agents/skills/` (agents read skill `references/`). Deny secrets by basename component: `.env .git .m2 .ssh .gnupg .aws .config/gcloud`. Do not allow `Write/Edit/Insert/Mkdir/Rmdir/Eval`: omitting them is enough.

**Delegation tools.** Constrain `Agent :subagent_type (one-of ...)`, `:directory (optional (under ROOT))`, `:result_file (optional (under TMP))`; `Discussion :preset "..." :directory (under ROOT) :worktree (optional false)`; `DiscussionControl :action (one-of ...)`. `DiscussionReport`/`AgentControl` can be allowed bare. Runner nodes: the runner authorizes `RunnerHandoff` itself; add `RunnerAskPrevious` only if follow-up questions are wanted.

**Skill tool.** `(:tool "Skill" :args (:skill "glab"))` or `(one-of ...)`: otherwise agents may load unrelated skills.

**MCP tools (read-only).** Policy denies by omission, but prune too:
1. Keep two explicit **allowlists of tool names** per server (never a denylist of mutating ones: new server tools would leak in).
2. Allow each name in the policy: `(mapcar (lambda (n) (list :tool n)) names)`.
3. Put those names in the preset `:tools` (filter with `(ignore-errors (gptel-get-tool n))` so missing servers do not break loading).
4. In a `-Q` worker the servers are not defined (they live in `~/.emacs.d/config/custom.el`): define `mcp-hub-servers` in the job init file, read the token from `ATLASSIAN_TOKEN` env or parse it from custom.el, never paste secrets into repo files. Connect with `(gptel-mcp-connect '("srv1" "srv2") t t)` (CONNECT-ONLY, synchronous), then remove non-allowlisted tools from `gptel--known-tools` (category `"mcp-<server>"`).
5. Tool names are the server's own (`confluence-*`, `jira-*`, `internal-*`); the category prefix is `mcp-`. Server lists: `~/.emacs.d/transient/history.el`.
6. Verify in batch: `emacs -Q --batch -L <straight/build dirs> -l job-init.el --eval '(princ (and (ignore-errors (gptel-get-tool "jira-create-issue")) t))'` must print `nil`. Set `ATLASSIAN_TOKEN` in that shell.

## Jobs and runner

- Policy and presets are loaded by the job's `:init-file` (via the pipeline file); the worker re-reads files on every launch, so **no job re-registration is needed** after editing them. A saved node chat created before the policy existed keeps old settings: start a new invocation.
- Changing launchd-relevant settings (`:environment`, `:emacs`) needs `gptel-runner-job-schedule` again; policy edits do not.
- Keep the existing agent instructions untouched when the request is only about permissions: restrictions belong to the policy, not prose.
- Do not rely on `:workspace-mode 'read`: the runner does not enforce it.

## Pitfalls that cost rounds

- Testing before reloading the file (everything returns nil).
- Testing with nonexistent paths: `under` canonicalizes real paths, so use files that exist.
- Forgetting one preset/sub-agent, or the temp dir (breaks oversized-output handling) or skill dirs (breaks skills).
- Over-broad `rx`: `(* anything)` after a prefix lets `; rm -rf` through. Always anchor and exclude metacharacters.
- `timeout` is absent on macOS: do not use it in verification commands.
- Policy cannot see inside a command: for Bash it only matches the string. Keep the allowlist small rather than trying to parse shell.
- A denial reads `Error: Tool policy denied X with the supplied arguments.`: when an agent reports it, widen the specific rule, do not remove the policy.

## Checklist before declaring done

- [ ] Every preset/sub-agent has `:agent-tool-policy`.
- [ ] Allowed + denied sample matrix run through `gptel-agent-policy--allows-p` (include `..`, secrets, write/post commands, mutating MCP tools).
- [ ] Batch init check: mutating MCP tools absent, read ones present.
- [ ] Project test suite passes after reload.
- [ ] Report what was not tested (no end-to-end model run).
