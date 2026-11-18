---
name: gptel-runner-jobs
description: Define, edit, launch, schedule, inspect, and control durable gptel-runner background jobs in Emacs. Use for job records and APIs, worker initialization, invocation history and outputs, command receipts, or background recovery. Pipeline construction and in-process run management belong to gptel-runner-pipelines.
---

# gptel-runner background jobs

Turn the user's task into a durable launch configuration for an existing named
workflow. A **job** is the reusable configuration; an **invocation** is one
execution in a separate `emacs -Q --batch` worker. Keep their IDs distinct from
the worker's session-local run ID.

Reuse pipeline definitions. If a suitable pipeline is missing or needs changes,
use the companion [gptel-runner-pipelines](../gptel-runner-pipelines/SKILL.md)
skill for that work. Reuse the same definition locally and in the worker;
launching in the background does not need a second pipeline or output adapter.
Keep job/bootstrap code short: use the runner's launch, inspection, and command
APIs instead of embedding process management, polling infrastructure, or storage
helpers into the workflow.

Jobs can use gptel-agent presets, nested agents, and discussions through the
standard driver. Verify compatible packages in the initialized worker and read
the [nested-work contract](../gptel-runner-pipelines/references/runtime-integrations.md#gptel-agent-presets-and-nested-work)
when those features are present. Child tool approvals use the normal job command
channel, including multiple pending requests on one node. Question forms and
separate interactive file-review interfaces still require headless adapters.

## Shared pipeline outputs

Both modes publish text with `gptel-runner-write-artifact` or copy existing files
with `gptel-runner-publish-artifact`. They own storage and return the same descriptor.
Use `gptel-runner-get`, `gptel-runner-outputs`, node outcome/state/result/error helpers,
and artifact list/path/text readers on either a run or refreshed invocation record.
Read [the output contract](../gptel-runner-pipelines/references/runner-api.md#managed-outputs-in-every-launch-mode)
when creating output steps. Do not branch on worker globals, keep temporary output
paths for local runs, hand-build descriptors, or add compatibility wrappers for
an old package. If the API is absent, identify the installed/checkout version gap.
The worker configures storage and checkpoints before dispatch; init does not
need custom artifact directories or event hooks. Remote mutations still use
`gptel-runner-job-command` and receipts.

Use the relevant reference for the task:

- [Job definitions and APIs](references/job-api.md): saved fields, defaults,
  inputs, workspaces, updates, deletion, and the job data editor.
- [Worker initialization](references/worker-bootstrap.md): packages, presets,
  backends, environment, and validation in a fresh Emacs.
- [Launch and scheduling](references/launch-and-verify.md): manual runs, the
  batch CLI, launchd schedules, diagnostics, and completion checks.
- [Invocation history and commands](references/invocations-and-commands.md):
  outputs, artifacts, all remote command arguments, receipts, and recovery.

For an existing job or invocation, go directly to its relevant API; creating
another job is not necessary to inspect or control it.

## Discover what can run

Use the agent's Emacs Lisp evaluation tool (such as `Eval`) or `emacsclient` to
inspect the intended running Emacs. Locate the installed runner and compare it
with the development checkout, normally `~/projects/gptel-runner`. Read the
README's background-jobs section and the relevant local source when an API or
option differs from this skill.

```elisp
(list :runner (locate-library "gptel-runner")
      :jobs-library (locate-library "gptel-runner-jobs")
      :loaded-jobs (symbol-file 'gptel-runner-job-save 'defun)
      :workflows
      (when (boundp 'gptel-runner--workflows)
        (mapcar (lambda (name)
                  (let ((workflow (gethash name gptel-runner--workflows)))
                    (list :name name
                          :options (gptel-runner-workflow-options workflow))))
                (hash-table-keys gptel-runner--workflows))))
```

There is currently no public workflow enumeration API. Use the private registry
only for guarded, read-only discovery; do not edit it. Inspect the selected
workflow's root with `gptel-runner-workflow-root` and read its source to identify
its purpose, runner agents/presets, input keys, expected outputs, workspace
overrides, write requirements, budgets, and human gates. A saved result read via
`gptel-runner-get` is not necessarily a required starting input.

On this machine definitions normally live under `~/.emacs.d/pipelines/`, with
`pipeline.el` loading individual files. Search hidden `.el` files too. Inspect
top-level forms before loading a file: loading a definition file must not also
launch its demonstration run. Treat files as available definitions, not proof
of registration in the current Emacs or the future worker.

If runtime access is unavailable, inspect files and report runtime availability
as unverified. If the installed runner lacks jobs, report the version gap and
prepare against the checkout. Do not hot-load only its new jobs module over an
older core; use a consistent package version for the editor, launcher, and worker.
Updating the installed runner is separate work when not already requested.

Once a compatible jobs module is available, inspect existing jobs with
`gptel-runner-job-list`. Match the user's intended job before saving: names are
not unique identifiers. Keep the existing `:id` when updating; otherwise a new
save creates another job.

## Map the task to a launch configuration

Select a suitable existing workflow. Ask only when several candidates imply
materially different work, or a required goal, input, or workspace is missing.
Derive these from the request and existing project context where possible:

- A useful job name and the exact registered workflow symbol.
- A goal preserving the requested outcome, constraints, and acceptance checks.
- Inputs using the pipeline's actual keys, as a readable Lisp alist or hash table.
- The workspace and any required files; retain a suitable workflow default.
- Finite call/request/duration limits, inherited when suitable or overridden
  for the task. Limits apply per invocation; they are not token or cost caps.
- Whether the request is to save, run now, or install a recurring schedule.

The goal is not automatically included in every agent prompt. Check that this
pipeline actually consumes `gptel-runner-run-goal`, its inputs, or supplied
`:context` so the user's task reaches its workers. Do not redesign the pipeline
silently to fill a missing contract.

Prepare a small worker init file by following
[worker initialization](references/worker-bootstrap.md). Loaded editor settings
do not carry over to the worker. Reuse existing backend, preset, tool, and
pipeline setup files that are suitable for batch use.

## Save the job

Use `gptel-runner-job-save`, not hand-written files in the job store. Required
keys are `:name`, `:workflow`, and `:init-file`. Adapt this example using the
discovered symbol, paths, goal, and task-appropriate limits:

```elisp
(require 'gptel-runner-jobs)
(setq my-job
      (gptel-runner-job-save
       (list :name "Project review"
             :workflow selected-workflow
             :init-file worker-init-file
             :base-directory task-directory
             :arguments
             (list :goal task-goal
                   :workspace task-directory
                   :max-calls 20 :max-requests 60 :max-duration 1800))))
(setq my-job-id (plist-get my-job :id))
(gptel-runner-job-get my-job-id)
```

`selected-workflow`, `worker-init-file`, `task-directory`, and `task-goal` stand
for values resolved for this request. Add `:inputs` only for the pipeline's
actual inputs. For intended write agents, add `:allow-writes t`; without it the
runner rejects the workflow. Retain tool confirmation unless the user has
authorized unconfirmed tools. Background execution does not grant that permission.

The durable `:arguments` allowlist is:

```elisp
(:goal :workspace :inputs :max-requests :max-calls :max-concurrency
 :max-duration :allow-writes :allow-unconfirmed-tools :decision-memory :context)
```

Do not put `:driver`, `:callback`, or `:persist` there. Select the driver in the
init file; the job worker enables persistence. Use serializable values and named
functions defined by the init file, not closures capturing editor state.

Omit `:arguments :workspace` to use the pipeline's default. Explicit nil is
normalized to the job's base directory and overrides the pipeline default.
`:base-directory` anchors relative workflow defaults;
explicit job workspace paths are normalized when saved. Per-node overrides
still apply. Jobs do not create worktrees or isolated copies of files.

Other job-level keys are `:id`, `:schedule`, `:environment`, `:emacs`, `:enabled`,
`:load-path`, and `:preload-features`. Defaults capture the current executable,
external library paths, and configured preload features. See the bootstrap
reference before overriding them. `:enabled` defaults to t, but saving neither
installs a schedule nor executes a pipeline. It is not a manual-run lock.

For an update, copy `gptel-runner-job-get`'s record, change the intended fields,
and save it with the same ID. Save is not a general partial-patch API. Re-read
the saved record and check its revision and relevant fields. A job update affects
future invocations; existing invocations retain their embedded job configuration.
Pass a freshly read job record or its ID when launching; passing an older record
uses that record's configuration. The embedded configuration preserves values
and paths, not copies of the init file, pipeline source, packages, or credentials.

## Verify and finish the requested action

Validate the init file in a fresh batch Emacs with the saved bootstrap settings,
without starting the real workflow; the bootstrap reference explains the checks.
`job-save` validates the record but does not prove that its workflow or presets
will load, its inputs meet the task contract, or its provider will work.

For a small end-to-end output check, use the checkout's
`examples/artifact-pipeline.el`, workflow `artifact-pipeline`, with
`:preload-features nil` in a temporary job store/workspace. It also runs directly
with `gptel-runner-start`. Use `examples/background-jobs.el`, workflow
`background-example`, when parallel fake agents or feedback gates are relevant.
Run the chosen example with the generic batch entry point in the [launch reference](references/launch-and-verify.md).
Verify durable outputs and a closed, succeeded invocation. This network-free check does
not establish readiness of real providers or the selected pipeline's presets.
Do not create test jobs in the user's normal store or install test schedules.

Save-only requests end after read-back and validation. When running or scheduling
was requested, follow [launching, scheduling, and verification](references/launch-and-verify.md)
and complete that action within the existing authorization. A request to create
a job alone does not imply an immediate model run or recurring execution.

Report the saved job name/ID, selected pipeline, init-file path, effective
workspace, and whether it was saved, launched, or scheduled. Include relevant
validation limits and any invocation ID or schedule details. Provide
`M-x gptel-runner-show-jobs` for job definitions and
`M-x gptel-runner-show-dashboard` for invocation history when useful.
