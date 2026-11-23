# Personal dotfiles

This checkout can live anywhere, standalone or inside a dotfiles collection.
Edit tracked source files; do not assume a checkout name or absolute path.

## Deployment

`deploy.sh` is the self-contained deployment entry point; the Makefile supplies
short aliases. GNU Stow and Bash are required; Make is optional.

- `make check` / `./deploy.sh check`: read-only conflict check and deployment preview.
- `make deploy` / `./deploy.sh install`: refresh managed links after preflight.
- `make doctor` / `./deploy.sh status`: verify installed links.
- `make uninstall` / `./deploy.sh uninstall`: remove only owned links.
- `make help` or no script arguments: show usage without deploying.
- `TARGET=/existing/home` in Make or `--home PATH` selects the home directory.
- `PLATFORM=darwin|linux` or `--platform` selects package defaults.
- `PACKAGES="zsh tmux"` or `--packages zsh tmux` selects explicit packages.

Package lists live directly in deploy.sh. Tooling, caches and
reference directories are not packages. System hibernation setup remains an
explicit sudo operation in setup.sh; never include it in normal deployment.

Never adopt files, overwrite conflicts, or delete unrelated links. Preserve live
configuration differences and local edits. Keep secrets, runtime snapshots and
private work configuration out of this public repository. Deployment preflights
changes but is not an atomic transaction; failures may require repair and rerun.

## Validation and migration

Validate deployment changes against a temporary home, not the user's live
configuration. Check shell syntax and run check/install/status/uninstall there.
Linux package selection checks on macOS do not prove native Linux behavior.

The optional parent Makefile and instructions live in the parent directory. Each child repo
must remain independently deployable. Moving repos is separate: inventory links,
back up live differences, prepare rollback, then move and retarget owned links.
