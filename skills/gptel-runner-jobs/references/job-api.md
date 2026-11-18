# Job definitions and APIs

Require `gptel-runner-jobs` for job records and worker APIs. Require
`gptel-runner-launchd` for the macOS launcher/scheduler, and
`gptel-runner-jobs-ui` for the jobs view and background dashboard actions.
`gptel-runner` loads the jobs and UI modules too.

## Definition API

| Call | Contract |
| --- | --- |
| `(gptel-runner-job-save job)` | Validate a job plist, write it, and return the normalized saved record. Does not run or install a schedule. |
| `(gptel-runner-job-get id)` | Read a saved job by its string ID from the current store. |
| `(gptel-runner-job-list)` | Return saved jobs sorted by name. Names need not be unique. |
| `(gptel-runner-job-run job &optional trigger)` | Launch a saved record or ID; return an invocation record. Trigger defaults to `manual`. See the launch reference. |
| `(gptel-runner-job-delete id)` | Delete the definition and, on macOS, remove its schedule. Retain invocation history. Does not cancel a running worker. |

`gptel-runner-job-directory` selects the store, normally
`~/.emacs.d/gptel-runner/jobs/`. Bind it around get/list/save/delete and scheduler
operations for another store. A record's `:root` identifies that store;
`job-save` writes to the currently bound store, not an arbitrary supplied `:root`.
`job-run` and `job-command` honor a supplied record's root, but resolving an ID
first still requires the correct store.

Use IDs returned by the APIs. Job ID, invocation ID, command ID, and the
worker's session-local run/call IDs are different identities.

## Saved fields

| Field | Meaning and default |
| --- | --- |
| `:name` | Required nonempty string; a display name, not identity. |
| `:workflow` | Required non-nil symbol naming a workflow. It can be registered only by the worker init; saving does not require editor registration. |
| `:init-file` | Required readable file, stored as an absolute path. Trusted Lisp executed on every worker start, including recovery. |
| `:id` | Omit for a new job; preserve for updates. |
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

No `:driver`, `:callback`, or `:persist` here. Init selects
`gptel-runner-default-driver`; workers start with persistence enabled. Use readable
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

Read, modify, save, and read back under the correct store:

```elisp
(let* ((gptel-runner-job-directory job-store)
       (job (copy-tree (gptel-runner-job-get job-id)))
       (arguments (copy-tree (plist-get job :arguments))))
  (setq arguments (plist-put arguments :goal revised-goal)
        job (plist-put job :arguments arguments))
  (gptel-runner-job-save job)
  (gptel-runner-job-get job-id))
```

Save replaces the record; it is not a general partial update. Only the base
directory and bootstrap fields have explicit fallback to the old definition.
Preserve the full record so omitted environment, arguments, schedule, enabled
state, or executable do not unexpectedly change. Remove a key with `cl-remf`
when omission is intended, especially for workspace inheritance.

Job edits affect future invocations, not an existing invocation's embedded job.
Recovery uses that embedded record. Source files and dependencies remain live
paths, so changing init or workflow code can still affect a later worker restart.
Keep compatible named workflows and stable explicit node IDs for recovery.

Reapply an installed schedule after changing settings used by its trigger,
including schedule, executable, or environment. `job-save` alone only updates
the stored definition. See [launch and scheduling](launch-and-verify.md).

## Definition editor

`M-x gptel-runner-show-jobs` lists definitions. `n` creates a definition, `RET`
edits it, `g` refreshes, and `?` opens the jobs menu. The editor's `C-c C-c`
reads and saves exactly one data plist; `C-c C-k` closes it.

Enter raw data, without an outer quote or executable constructors:

```elisp
(:name "Project review"
 :workflow project-review
 :init-file "/absolute/path/to/worker-init.el"
 :base-directory "/absolute/path/to/project/"
 :arguments (:goal "Review the current changes"
             :inputs ((subject . "current changes"))
             :max-calls 20 :max-requests 60 :max-duration 1800)
 :schedule nil
 :enabled t)
```

Adapt the workflow, paths, and input keys. In this data editor, `(list ...)`,
`(expand-file-name ...)`, and `'project-review` are read as data, not evaluated
into intended values. Ordinary evaluated Lisp can use constructors as in
`SKILL.md`. Preserve `:id` when editing an existing job. Saving in the editor
also does not run the job or apply schedule changes.
