# Prepare a fresh worker

Read this before writing or selecting a job's `:init-file`. The worker loads
trusted Lisp in a fresh `emacs -Q --batch` process. It does not load the editor's
normal init or inherit its live presets, backend objects, variables, and hooks.

## Reuse configuration without launching work

Inspect existing configuration and extract or reuse only the necessary batch
setup. Keep the user's pipeline definitions intact. A typical init file needs:

1. The runner and any extra packages needed by its tools or helper functions.
2. Backend/model configuration and a noninteractive credential lookup.
3. Custom agent/skill directories, followed by `gptel-agent-update` if changed.
4. The selected pipeline's preset definitions and tool registrations.
5. The existing pipeline file, including its runner-agent registrations.
6. `gptel-runner-default-driver` set to the intended driver, normally
   `(gptel-runner-gptel-driver-create)` for a real job.

The worker starts the workflow itself after init completes. Do not call
`gptel-runner-start`, `gptel-runner-job-run`, or schedule installation from init.
Avoid loading the entire interactive `init.el`: it may install packages, change
UI state, prompt, or launch unrelated work. Check reused files for such effects.
Initialization runs again for recovery, so it must be repeatable.

Keep credentials in the user's existing credential mechanism. Configure their
lookup in the worker without printing secrets or copying them into job records.
Encrypted stores or tools requiring a minibuffer may need a different supported
noninteractive setup. Report the specific missing prerequisite when necessary.

## Captured bootstrap settings

New jobs normally capture `gptel-runner-job-preload-features`, defaulting to
`(gptel gptel-agent)`. Before init, the worker requires these features and runs
`gptel-agent-update` if `gptel-agent` is included. Init then has final authority
over preset overrides. If init adds `gptel-agent-dirs`, refresh discovery before
loading custom overrides. Additional agent files do not automatically create
the runner-agent registrations referenced by pipeline steps.

Explicit `:preload-features nil` disables this automatic preload/discovery. Use
it for fake-driver or deliberately self-contained workers, not as a workaround
for missing dependencies in a real job.

Saving captures `gptel-runner-job-load-path` followed by the editor's external
`load-path`, normalized to absolute local directories. Bundled Emacs Lisp paths
are omitted. Workers prepend the captured paths to their own path before
preloading features. This transports library locations, not loaded settings.

Prefer saving from the configured Emacs so its package paths are captured. For
a package available only in a checkout, add its directory through the job's
bootstrap configuration. An explicit job `:load-path` **replaces** automatic
capture: include all needed external dependencies, not just gptel-runner. Do not
assume adding a path in init can fix a preload failure that happens before init.

Updates preserve existing `:load-path` and `:preload-features` unless explicitly
replaced, including replacement with nil. Re-saving an older job from configured
Emacs captures missing bootstrap fields for future invocations; already-created
invocations keep their original bootstrap metadata.

Use an absolute executable path for `:emacs`; it defaults to the saving Emacs's
executable. Scheduled workers do not inherit an interactive shell's setup.
Use absolute external-tool paths or provide `:environment`, for example:

```elisp
:environment '(("PATH" . "/opt/homebrew/bin:/usr/bin:/bin"))
```

Choose entries for the actual tools, rather than copying this sample blindly.
Backend configuration and credential access also need to work in that environment.

## Preflight without provider calls

Check the installed version before choosing validation APIs. There is no public
job dry-run API. Do not use `gptel-runner-job-run` or `gptel-runner-start` as an
inspection call: they dispatch work.

Review the init and files it loads first. Then use a separate batch Emacs with
the saved executable, a consistent runner package, and a small temporary check
file that reproduces these bootstrap conditions:

- Read the job with `gptel-runner-job-get` from its actual `:root` store.
- Set `default-directory` to explicit `:arguments :workspace`, otherwise the
  saved `:base-directory`. Prepend the saved `:load-path`.
- Apply the saved environment and derive `exec-path` from its `PATH` if supplied.
- Set `gptel-runner-headless` non-nil. Disable automatic native compilation and
  trampoline generation as the worker does.
- Require each saved preload feature, refresh gptel-agent when included, and
  load the actual init file in that order.
- Inspect registration of the selected workflow, its referenced runner agents,
  effective workspaces/context files, and selected driver. For real jobs inspect
  the required presets, their inherited tools/settings, backend/model selection,
  and credential availability without exposing credential values.

Use the companion skill's
[preset inspection reference](../../gptel-runner-pipelines/references/presets.md)
when resolving preset inheritance or dynamic settings. Discovery in the editor
and discovery in the initialized worker are separate checks.

Keep this check from starting workflows or sending requests, for example by
temporarily replacing the start/request entry points with functions that signal
an error during the check. This is a validation guard, not a sandbox for
untrusted init code. Do not apply every preset just to inventory it: preset hooks
may have side effects. A successful load proves initialization, not live provider
access or headless compatibility of every tool.

Human gates and tool approvals remain possible in background work. Workers wait
for the dashboard/command channel; direct synchronous minibuffer tools may fail
with unsupported input. Do not remove confirmations merely to make a schedule
unattended. Resolve any mismatch between the requested autonomy and the pipeline's
actual interaction requirements before launching it.
