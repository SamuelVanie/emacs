# Job definitions and APIs

Require `gptel-runner-jobs` for definition, registration, scheduling, and
worker APIs. The top-level `gptel-runner` package also loads the jobs module.

## Definition API

| Call | Contract |
| --- | --- |
| `(gptel-runner-register-job id &rest properties)` | Create or update the definition under a stable string ID and return the saved record. Properties are evaluated keyword arguments. Registration alone saves the configuration. |
| `(gptel-runner-job-schedule job &optional schedule)` | Install the supplied rule on macOS or activate the saved rule when omitted. Accepts an ID or saved record and returns the updated definition. |
| `(gptel-runner-job-unschedule job)` | Remove the installed trigger and retain the saved rule, definition, and history. Accepts an ID or saved record and returns the updated definition. |
| `(gptel-runner-job-save job)` | Validate and save a complete job plist. Generates an ID when omitted. Existing callers can continue to use this API. |
| `(gptel-runner-job-get id)` | Read a saved job by its string ID from the current store. |
| `(gptel-runner-job-list)` | Return saved jobs sorted by name. Names need not be unique. |
| `(gptel-runner-job-run job &optional trigger)` | Launch a saved record or ID; return an invocation record. Trigger defaults to `manual`. See the launch reference. |
| `(gptel-runner-job-delete id)` | Delete the definition and, on macOS, remove its schedule. Retain invocation history. Does not cancel a running worker. |

`gptel-runner-job-directory` selects the store, normally
`~/.emacs.d/gptel-runner/jobs/`. Bind it around registration and operations by
ID for another store. A saved record's `:root` identifies its store.
`gptel-runner-job-save` always writes to the currently bound store.

Use IDs returned by the APIs. Job ID, invocation ID, command ID, and the
worker's session-local run/call IDs are different identities.

## Saved fields

| Field | Meaning and default |
| --- | --- |
| `:name` | Nonempty display name. Registration defaults it to the string ID when creating a job. |
| `:workflow` | Required non-nil symbol naming a workflow. It can be registered only by the worker init; saving does not require editor registration. |
| `:init-file` | Required readable file, stored as an absolute path. Trusted Lisp executed on every worker start, including recovery. |
| `:id` | Stable string ID supplied as the first argument to registration. Use letters, digits, underscores, or hyphens. |
| `:arguments` | Runner start keyword plist from the allowlist below; nil means use workflow/runner defaults. |
| `:base-directory` | Existing directory, stored canonically. Defaults to the old job's base on update, otherwise the saving Emacs's `default-directory`. |
| `:emacs` | Absolute executable path; defaults to `gptel-runner-job-emacs-program` (the current Emacs executable). |
| `:load-path` | List of local library directories. New jobs capture `gptel-runner-job-load-path` plus external editor paths; updates preserve the saved value. Explicit values, including nil, replace capture. |
| `:preload-features` | List of non-keyword feature symbols. Defaults to `gptel-runner-job-preload-features`, normally `(gptel gptel-agent)`. Updates preserve it; explicit nil disables preloading. |
| `:environment` | Alist of string names and string values, applied over the worker environment. Names must be valid environment variable names. Supplied `PATH` also rebuilds worker `exec-path`. |
| `:schedule` | nil, a positive-seconds interval, or calendar rules; see the launch reference. Saving it does not apply it to launchd. |
| `:enabled` | Defaults to t when absent. The CLI `trigger` checks it; manual `job-run` and CLI `run` do not. Setting it alone does not install/unload a service or cancel work. |

The API supplies `:root`, an incrementing `:revision`, `:format`, and `:version`.
Scheduler operations save `:scheduler` with `:state`, `:operation`, `:updated-at`,
and `:error`. Treat these as service metadata, not user-authored settings.

Use absolute init/executable paths. A relative `:init-file` resolves in the
saving Emacs's current directory, not against `:base-directory`. Explicit
relative library directories resolve against the saved base. Capture excludes
bundled Emacs Lisp directories and nil path entries. A captured directory is
not proof its packages or dependencies will load.

`job-save` validates required fields, schedule, paths, argument keys, and readable
serialization. It does not fully validate the pipeline's input contract, all
start-option values, presets, provider configuration, or headless tools.

## Durable start arguments

Only these keys are accepted inside `:arguments`:

| Key | Job-facing behavior |
| --- | --- |
| `:goal` | Task goal string. The pipeline must actually incorporate it in prompts/context. |
| `:workspace` | Existing working directory override; omission preserves the workflow default. |
| `:inputs` | Initial blackboard alist or hash table, available before the first prompt. The pipeline reads values with `gptel-runner-get`; they are not automatically prompt text. |
| `:max-requests`, `:max-calls`, `:max-duration` | Per-invocation limits; duration is seconds. Prefer finite task-appropriate limits. These are not token/cost caps. |
| `:max-concurrency` | Concurrent agent and Elisp step limit within this worker. |
| `:allow-writes` | Enable explicitly for a workflow with declared writers. Does not itself create a worktree or restrict tool capabilities. |
| `:allow-unconfirmed-tools` | Explicit tool confirmation policy, following the user's requested autonomy. Background execution does not change its meaning. |
| `:decision-memory` | Override decision-memory behavior; omit to inherit. |
| `:context` | Supported runner context data, such as shared text, file descriptors, or named functions defined by init. See the companion pipeline API for descriptor syntax. |
| `:completion` | Default completion for agent nodes. Omission inherits the workflow setting. Use the symbol `handoff` for tool-based submission (the default for new workflows) or `response` for ordinary responses. An explicit node setting wins. |

No `:driver`, `:callback`, or `:persist` here. Init selects
`gptel-runner-default-driver`; workers start with runner persistence enabled
for their workflow, node chats, and child transcripts. Use readable
Lisp data and named functions available after bootstrap. Buffers, processes,
backend objects, and closures holding editor state are not durable configuration.

Workspace resolution:

1. An explicit `:arguments :workspace` is resolved against the saved base and
   canonicalized at save time. Explicit nil becomes the base directory.
2. If the key is absent, the worker resolves the workflow's default after init.
   A relative default resolves against the saved base; without a workflow default,
   that base supplies the run directory.
3. Per-node overrides still apply, relative to the resulting run workspace.

Keep initial input keys distinct from produced output keys so continuation can
clear workflow outputs without discarding starting inputs. Use the actual
pipeline's contract; `gptel-runner-get` can also read optional or produced values.

## Updating a definition

Register a change under the original ID. Omitted top-level fields retain their
saved values. To edit a single start argument, copy the existing argument
plist first because a supplied `:arguments` replaces it in full.

```elisp
(let* ((gptel-runner-job-directory job-store)
       (job (gptel-runner-job-get job-id))
       (arguments (copy-tree (plist-get job :arguments))))
  (gptel-runner-register-job
   job-id :arguments (plist-put arguments :goal revised-goal))
  (gptel-runner-job-get job-id))
```

To resume inheriting the workflow's workspace, remove `:workspace` from the
copied argument plist before registering the replacement. For a job created
with `gptel-runner-job-save`, use the existing record's `:id` for registration.

Job edits affect future invocations, not an existing invocation's embedded job.
Recovery uses that embedded record. Source files and dependencies remain live
paths, so changing init or workflow code can still affect a later worker restart.
Keep compatible named workflows and stable explicit node IDs for recovery.

Reapply an installed schedule with `gptel-runner-job-schedule` after changing
settings used by its trigger, including the executable or environment.
See [launch and scheduling](launch-and-verify.md).
