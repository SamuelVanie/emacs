# Discover and select available presets

Choose presets before finalizing a workflow. A runner agent registration stores
a reference; it neither creates the referenced gptel preset nor checks all its
tools. Keep these names distinct:

| Name | Meaning |
| --- | --- |
| Step `:agent 'reviewer` | A runner agent registered with `gptel-runner-register-agent` |
| Registration `:preset 'mayuri-reviewer` | A gptel preset providing instructions and settings |
| A gptel-agent file/subagent name | A separately discovered definition; not necessarily a registered gptel preset |

## 1. Inspect the running Emacs

Use an available Emacs Lisp evaluation tool, such as gptel-agent's `Eval` when
enabled, or `emacsclient --eval`. Query the target Emacs session; if there are
multiple servers, use the intended socket/server. A fresh `emacs -Q` session
does not load the user's presets. Do not start a second Emacs or reload the
whole init file just to discover settings.

Start with this read-only query (no provider request or preset application):

```elisp
(list :gptel-loaded (featurep 'gptel)
      :source (locate-library "gptel")
      :presets
      (when (boundp 'gptel--known-presets)
        (mapcar (lambda (entry)
                  (list :name (car entry)
                        :description (plist-get (cdr entry) :description)
                        :parents (plist-get (cdr entry) :parents)))
                gptel--known-presets)))
```

For a shell-capable agent, the shorter names-only query is:

```sh
emacsclient --eval '(and (boundp (quote gptel--known-presets)) (mapcar (function car) gptel--known-presets))'
```

The inspected gptel versions have no public enumeration function.
`gptel--known-presets` is a private alist: use it only for guarded, read-only
discovery after checking the installed version. Do not mutate it or add a private
API dependency to generated workflow code. Use the public `gptel-get-preset` to
read a selected preset and `gptel-make-preset` to define one when needed.

An unavailable server, an unloaded gptel, and an empty registry are different
outcomes. If runtime access is unavailable, inspect definition files and report
availability as unverified; do not label those files as loaded presets. If gptel
is unloaded, load the intended package/configuration through the user's normal
setup before relying on the registry. `require` alone does not load personal
preset files.

## 2. Inspect candidates, including inheritance

Use a discovered symbol in place of this example name:

```elisp
(let* ((name 'mayuri-reviewer)
       (spec (gptel-get-preset name)))
  (unless spec (user-error "Preset is not loaded: %S" name))
  (mapcar (lambda (key) (cons key (plist-get spec key)))
          '(:description :parents :system :model :use-tools :confirm-tool-calls)))
```

Read the candidate's `:tools` definition too. For actual tool objects, inspect
`gptel-tool-name` and its description rather than dumping function bodies.
For a literal tool name, category, or category/name path, `gptel-get-tool` resolves
the registration and signals an error if missing. Categories can expand to more
than one tool; inspect their actual capabilities. Do not assume a read-mode runner
agent has a read-only toolset.

`gptel-get-preset` returns the stored specification, not a flattened effective
configuration. Follow each `:parents` reference, in order, and inspect the child's
overrides. Distinguish an omitted field from explicit nil using `plist-member`:
omitted model, backend, or tools can fall through to parent/current defaults.
Inspect `:append`/`:prepend` composition and dependencies before treating a tools
list or system prompt as complete.

Settings can contain `:eval`, `:function`, dynamic system functions, and
`:pre`/`:post` hooks. Reading them does not evaluate them. Do not apply every preset
or use `gptel-with-preset` as a supposedly read-only inventory: application can run
hooks, connect services, or change state. Inspect the definitions, mark unresolved
settings as dynamic, and validate selected runtime behavior only as needed.

Check the backend by name and model choice without printing backend objects or
authentication fields. `gptel-get-backend` resolves a backend name; return its
presence/name, not the object. For defaults, inspect the intended worker's relevant
settings rather than assuming the current chat's buffer-local values carry over.

Match the preset against the node's job:

- Instructions permit the intended role and preserve the shared goal/decisions.
- Read, edit, execution, and external-access tools fit the assignment.
- Required tool registrations and optional packages are available.
- Model/backend and dynamic setup dependencies are identified.
- Output instructions agree with the node parser/schema and gate behavior.

A reviewer preset may insist on a prose report while the starter requires JSON.
An additional task prompt does not reliably override conflicting system
instructions. Adapt the consuming parser or define a compatible preset explicitly;
do not silently overwrite an existing user preset. Inheriting a parent with
`:system "..."` can replace its full prompt; preserve required role instructions
deliberately when composing a new one.

## 3. Find definitions that are not loaded

On this machine, useful starting points are `~/.emacs.d/presets/`,
`~/.emacs.d/config/gptel-custom-functions.el`, and the gptel sections of
`~/.emacs.d/init.el`. Search for `gptel-make-preset`, preset names, and explicit
load forms. File names need not equal preset symbols, and a file on disk can be
intentionally absent from the startup configuration. Treat these paths as hints,
not a fixed inventory.

For gptel-agent, inspect the configured `gptel-agent-dirs` and its Markdown/Org
definitions. The public `gptel-agent-read-file` reads one file and returns
`(NAME . PLIST)`; inspect its supported arguments in the installed version.
An agent file, an enabled subagent, and a gptel preset are different things.
In the locally inspected version, `gptel-agent-update` registers the main
`gptel-agent`, `gptel-plan`, and `ask` presets, not every discovered agent file.
Do not assume a `researcher` file makes `:preset 'researcher` work.

If a suitable definition exists but is unloaded, inspect it and load the intended
definition or arrange its normal load dependency within the authorized workflow
setup. Loading Lisp executes code; avoid bulk-loading all configuration files.
Refresh gptel-agent definitions only when needed, then query the preset registry
again. For an unregistered agent file, explicitly construct a supported preset
or inline preset plist from its inspected definition, resolving templates and
agent-specific dependencies; do not copy private registry internals blindly.

When nothing suitable exists, define a workflow-specific preset with verified
tools/model settings, or identify the missing capability if it requires user
input. Do not invent tool names or leave starter placeholders in a live workflow.

## 4. Register and verify the mapping

Once a compatible preset is loaded, the runner name can be different:

```elisp
(unless (gptel-get-preset 'mayuri-reviewer)
  (user-error "Load the mayuri-reviewer preset definition first"))
(gptel-runner-register-agent
 'reviewer :preset 'mayuri-reviewer :workspace-mode 'read)
;; Set the step's parser/output contract to match this preset's inspected output.
```

Keep a concise mapping of node, runner agent, preset symbol, purpose, capabilities,
and load prerequisites. Reuse a runner registration only after confirming its
preset, mode, parser, and context match; do not overwrite unrelated registrations.
For diagnosis, a guarded read of `gptel-runner--agents` can inspect existing
registrations, but generated workflows should use the public registration API.

Keep load order explicit: load gptel and required tools, load/define selected
presets, register runner agents, then define/start the workflow. Durable resume
needs those definitions again in the new Emacs session. Fake-driver tests validate
routing and prompts; they do not prove preset existence, tool resolution, backend
credentials, or provider availability. Do not use paid live requests for discovery.

Continue with [operational preflight and start](operations.md#load-and-preflight)
once the mappings are verified. Loading definitions and starting provider work
are separate actions; inspect a definition file for top-level start forms first.
