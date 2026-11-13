# Manual Herdr sessions

Use **Auto-restore · Save · Restore** below the Spaces header, or press **Ctrl+A, S**
to open the session manager. Save writes a snapshot immediately; Restore opens
the snapshot selector and preview. Auto-restore is off until you enable it and
the toggle persists in Herdr's shared mod state.

When enabled, the sidebar checks after startup and every 15 seconds while the
local Core is ready. It resumes the active saved snapshot only when its original
terminal identities are still present and their processes have recovered. Live
agents remain alone; closed terminals and reused addresses are not restored
automatically. Missing references, changed layouts or busy replacements require
attention. The sidebar must be loaded for automatic restore to run.

Saving stays manual, including when closing the window or quitting. No close
confirmation or background autosave is installed.

Choose **Save current** before restarting the laptop. After opening Herdr again,
use **Preview restore**, then **Restore saved**. Existing live terminals stay running.
Expand **Saved sessions** to choose one of the three retained snapshots by date,
time and counts. The newest is selected by default. Preview and Restore use that
selection; choosing another snapshot clears the previous preview. Existing live
work and changed layouts still trigger the same restore protections.

The snapshot preserves local spaces, tab names, pane splits, working folders,
and exact Codex, Claude and Pi conversation references. Ordinary commands return
as shells; running tasks and in-memory agent/subagent work cannot survive reboot.
Focus, window placement and themes remain Herdr's own settings. Agent resume uses
the installed CLI's defaults, not every flag from its previous launch.

## First use

Claude references come from its process registry. Pi requires the extension below:
run `/reload` in already-running Pi tabs once, then save again. New Pi processes
load it automatically.

For Codex, get the exact Session UUID with `/status` in each tab, then bind it:

```sh
hctl session bind t1 --agent codex --session SESSION-UUID
hctl session save
```

Use the correct terminal address from `herdr-core ls --json`. Rebind after switching
Codex conversations in the same process. Bindings expire when that process changes.
Do not reboot until the saved-session screen has no unresolved agents.

## CLI and storage

```sh
hctl session save
hctl session list
hctl session status
hctl session restore          # preview, no changes
hctl session restore --apply
hctl session restore --snapshot SNAPSHOT-ID        # preview a selected save
hctl session restore --snapshot SNAPSHOT-ID --apply
hctl pane resize left         # focused Herdr App pane
hctl pane resize right --pane PANE-ID
```

`hctl` is the entry point for custom Herdr controls. Add future command groups here.
App buttons and keyboard resizing use `hctl`.
Resize moves a split boundary in 5% steps; it currently supports local weighted splits.

Private snapshots live in `~/.local/state/herdr-app-sessions/`, outside this repo.
They contain paths, layout and session references, never conversation contents.
Directories are private and JSON files use mode 600. Native agent session files
must remain on disk. `latest.json` is the active snapshot; `snapshots/` keeps only
the latest three saves. Older archives are removed after a successful save.
`before-restore.json` backs up the Core document before a restore. Do not overwrite
the saved snapshot with fresh recovery shells after a reboot: restore first.

Restore rejects changed saved pane layouts, missing folders/references and busy
replacement terminals. Extra spaces/tabs remain. Recovery shells are unlabelled
and unkept, but left alive. Failed document commits attempt rollback of created
terminals and recovery-shell metadata. No remote-machine restore is supported.

## Restore into legacy Herdr TUI

This copies a snapshot into a separate, explicitly named legacy Herdr session.
It recreates workspaces, named tabs, weighted terminal splits and saved working
directories, then resumes Codex, Claude and Pi using the saved references.
It does not move running processes, transfer scrollback or restore App mod panes.
Ordinary commands return as shells. Remote spaces and fixed-size splits are rejected.

Save while the agents still have their exact session references, then exit the
original agents before applying a TUI restore. Preview reports saved agents still
running on this boot; apply refuses those duplicates. Do not manually resume the
same conversations elsewhere during migration.

```sh
hctl session restore --target tui --tui-session restored   # read-only preview
herdr --session restored                                 # start/open target TUI
# In a separate shell, or a plain shell pane in that named session:
hctl session restore --target tui --tui-session restored --apply
```

Add `--snapshot SNAPSHOT-ID` to select an older save. The target server must already
be running and have no agents; existing shell workspaces are preserved. The helper
never stops Core, changes its App layout, or starts/stops a TUI server. Creation
uses `--no-focus`; select the restored workspace in the TUI after completion.

`tui-NAME.json` in the private snapshot directory journals created workspace/pane
IDs and launch progress. Repeating apply does not create duplicates. On a partial
failure, inspect the target and journal: created panes and any started agents stay
alive, and automatic replay is refused. Do not delete a journal and retry over a
partially restored session. The App restore checkpoint and snapshots stay unchanged.

## Install from this checkout

From the repository root, deploy with GNU Stow:

```sh
stow --simulate --no-folding -vR herdr -d . -t "$HOME"
stow --no-folding -vR herdr -d . -t "$HOME"
```

`./bootstrap.sh --stow` includes this package too. Stow installs the app configs,
`~/.local/bin/hctl`, and the Pi extension at
`~/.pi/agent/extensions/herdr-session-registry.ts`. Source for the extension lives
in `herdr/.pi/agent/extensions/`. The `session-manager/` documentation and tests,
generated mod types/configs, Python caches and runtime locks are excluded by
`herdr/.stow-local-ignore`. Private saved sessions are not part of this package.

The custom `legacy-prefix` and `space-agents` mods provide the shortcut and button.
Use Python 3, herdr-core, and the native agent executables on PATH.
Restore falls back to the interactive login shell's PATH when an agent is absent
from the desktop app's PATH, and forwards it to Core for runtimes such as mise Node.
All agent executables are resolved before starting any restored terminals.
Validate with
`python3 -m unittest discover -s herdr/session-manager/tests` from the repo root,
and `herdr-app check` for each changed mod. Then open the popup in Herdr; the
checker does not exercise asynchronous states. Never stop live user agents or
restore over their sessions just to test this feature.
