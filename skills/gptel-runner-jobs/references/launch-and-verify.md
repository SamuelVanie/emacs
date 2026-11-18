# Launch, schedule, and verify a saved job

Use only the actions covered by the user's request. A saved definition, installed
schedule, launched worker, and completed invocation are different outcomes.

## Run now

On macOS the default launcher uses launchd to start a worker that survives the
editor closing:

```elisp
(setq my-invocation (gptel-runner-job-run my-job-id))
(setq my-invocation-id (plist-get my-invocation :id))
```

One invocation per job can be active within its store. An overlapping trigger
produces a closed
`skipped` record with reason `overlap`, not a queued execution. Inspect it rather
than retrying or saving another job to bypass the lock. Different jobs can run
concurrently; declared background writers using the same store coordinate
canonical workspaces. Different stores do not share these locks. This does not
isolate filesystem effects from the editor or other programs.

For a generic shell run, use the saved executable, a compatible runner directory,
and the returned job ID/root (quote actual shell arguments appropriately):

```sh
/absolute/path/to/emacs -Q --batch -L /absolute/path/to/gptel-runner \
  -l gptel-runner-jobs -f gptel-runner-jobs-batch-main -- \
  run JOB-ID --root /absolute/path/to/job-store/
```

`run` owns the worker until completion, including human-input waits. Exit 0
means success **or a skipped duplicate**, 1 means another terminal pipeline
outcome, and 2 means startup/storage failure. Inspect history, not exit code
alone. On macOS `trigger` instead dispatches a detached worker; it does not wait
for pipeline completion. Use these runner APIs rather than inventing an Emacs
server or an unrelated scheduling mechanism.

`trigger` checks the saved `:enabled` flag; `run` and manual `job-run` do not.
The third CLI form, `worker INVOCATION-DIRECTORY`, is for a launcher handing
off an already-reserved invocation. It is not a shortcut for creating a run;
the worker checks its reservation before adopting it.

`gptel-runner-job-launch-function` accepts an invocation record and starts its
worker. Its default is macOS launchd; other systems need an explicit launcher
for `job-run` and remote recovery, or the blocking CLI `run` for initial runs.
`gptel-runner-jobs-worker` accepts a reserved invocation directory and returns
the worker exit code without calling `kill-emacs`; `gptel-runner-jobs-batch-main`
parses CLI arguments and exits. Custom launchers must preserve reservation and
ownership handoff; use the existing adapter and process tests as the contract.

## Install a recurring schedule

On macOS, apply the requested rule to a registered job through the public
scheduling API. The example installs a recurring Monday 09:00 local trigger.

```elisp
(require 'gptel-runner-jobs)
(setq my-job
      (gptel-runner-job-schedule my-job-id '(:calendar ((:weekday 1 :hour 9 :minute 0)))))
(require 'gptel-runner-launchd)
(gptel-runner-launchd-diagnostics my-job)
```

If the definition already contains the desired `:schedule`, call
`(gptel-runner-job-schedule my-job-id)` to activate it. Passing a rule saves
and installs it for future triggers. Registration alone saves the rule.
Read the returned job's `:scheduler` status and check launchd diagnostics to
verify that the schedule is active.

Supported schedules:

| Form | Meaning |
| --- | --- |
| `(:interval 900)` | Positive integer seconds between triggers |
| `(:calendar ((:hour 9 :minute 0)))` | Daily at 09:00 in the Mac's local timezone |
| `(:calendar ((:weekday 1 :hour 9 :minute 0)))` | Monday at 09:00 local time |

Multiple calendar rules are allowed. Fields are `:minute` 0–59, `:hour` 0–23,
`:day` 1–31, `:weekday` 0–7, and `:month` 1–12; 0 and 7 mean Sunday. Omitted
fields are wildcards. When both day-of-month and weekday are present, launchd
matches either, not their intersection. Confirm the host timezone when translating
the user's requested time; this API has no timezone field or future one-off mode.
Do not approximate a one-off request with an indefinitely recurring schedule.

User LaunchAgents require the login session. Calendar triggers missed during
sleep coalesce on wake; interval triggers can be missed. Explain these limits
when relevant to requested timing. Do not promise execution while powered off.

Use `(gptel-runner-job-unschedule my-job-id)` to remove the installed trigger.
The saved rule remains available for later scheduling, and active workers
continue. To delete the definition itself use `gptel-runner-job-delete`.
Both operations preserve invocation history. The built-in schedule installer
requires macOS.

Scheduling and unscheduling accept either an ID in the current job store or a
saved record, and read the latest definition. Installation sets `:enabled t`.
Removing the trigger sets it to nil. Both return an updated job record with
scheduler status. After an error, inspect the current record and diagnostics
because installation can fail after changing the service.

`gptel-runner-launchd-diagnostics` returns `:label`, `:file`, `:saved-status`,
and raw launchctl `:exit`/`:output`. Treat the text as human diagnostics, not a
stable machine-readable API. Trigger logs are under `scheduler/JOB-ID/` in the
store; worker logs are in the invocation directory.

`gptel-runner-launchd-start-worker` is the detached-launch adapter for an
already-reserved invocation. It waits for worker ownership adoption, not task
completion. `gptel-runner-launchd-reap` unloads exited, superseded, or confirmed
dead package-owned worker services while preserving history. It also handles
uncertain launches only after confirming absence. Normal launches and jobs-view
refreshes already arrange cleanup; do not manually remove active registrations.

Relevant settings are `gptel-runner-launchd-directory` (normally
`~/Library/LaunchAgents/`), `gptel-runner-launchctl-program` (normally
`/bin/launchctl`), and `gptel-runner-launchd-startup-timeout` (15 seconds).
Services belong to the current user's `gui/UID` login domain; trigger and worker
services have independent lifetimes. A startup timeout stops the exact worker
service before releasing its reservation. Failed stop confirmation leaves a
visible `launch-uncertain` invocation instead of allowing an overlapping retry.

## Read back the outcome

Bind `gptel-runner-job-directory` to the job's actual `:root` when it is not the
current store. Refresh the manifest; the record returned at launch is not live:

```elisp
(setq my-invocation (gptel-runner-invocation-get my-invocation-id))
(list :id (plist-get my-invocation :id)
      :phase (plist-get my-invocation :phase)
      :state (plist-get my-invocation :state)
      :reason (plist-get my-invocation :reason)
      :error (plist-get my-invocation :error))
;; Read outputs when the invocation has published them:
(gptel-runner-outputs my-invocation)
```

For requested completion monitoring, use bounded, spaced status checks that yield
to Emacs's event loop. A handoff-mode node ending on ordinary prose waits for
input. Inspect its published chat and `? m` message log. On the waiting node,
`? f` sends a worker turn and `? a` accepts the response. At an explicit human
gate, approval, startup error, or failed outcome, report the specific state and
needed action. A `succeeded` state with
`:phase closed` establishes the worker finished its history; also inspect required
outputs/artifacts to verify the user's task. A fake run only verifies job plumbing.

External invocations are data owned by another process. Do not load their
checkpoint into the editor to inspect or control them. Use the dashboard or
`gptel-runner-job-command` for requested follow-up actions, and inspect command
receipts with `gptel-runner-job-command-status`; a queued command is not yet applied.
In-process `gptel-runner-run` control functions do not accept invocation records.
Read [invocation history and commands](invocations-and-commands.md) for the exact
operation arguments, receipt states, call addressing, and recovery requirements.
