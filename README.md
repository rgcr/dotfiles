# My Personal Configuration

I use [**GNU Stow**](https://www.gnu.org/software/stow/) to handle my dotfiles.

### 📦 Installation

- macOS: `brew install stow`
- Arch Linux: `pacman -S stow`

---

## Requirements

### `vim-plug` for Vim

```bash
mkdir -p ~/.vim/autoload
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

### [`antidote`](https://github.com/mattmc3/antidote) for ZSH

- macOS: `brew install antidote`
- Arch Linux: `pacman -S zsh-antidote`

### SwayNotificationCenter for Sway

- Arch Linux: `pacman -S swaync`


---

## Installation

### 🔗 Link the config you want

```bash
stow --no-folding -d . -t ~ -vR <config>
```

Example:

```bash
stow --no-folding -d . -t ~ -vR zsh
```

Or deploy all normal home dotfiles with:

```bash
./deploy.sh install
```

### To remove a specific config

```bash
stow --no-folding -d . -t ~ -vD <config>
```

### To remove all configs

```bash
stow --no-folding -d . -t ~ -vD *
```

Run `./deploy.sh` with no parameters to show usage.

---

## Notes

### `i3-hibernate` config requires `sudo` privileges

```bash
./deploy.sh install --i3-hibernate
```

### `sway-hibernate` system sleep config requires `sudo` privileges

```bash
./deploy.sh install --sway-hibernate
```

This uses `suspend-then-hibernate`: the laptop suspends first, then writes RAM to
disk after 30 minutes so the session can survive a drained battery. This requires
working Linux hibernation support, including a swap partition or swap file
configured as the kernel resume device.

# Portable deployment

Run `make help`, `make check`, `make deploy`, or `make doctor` from this checkout.
Use `make -C /path/to/checkout ...` from elsewhere. The checkout can be standalone
or inside a dotfiles collection. `TARGET=/existing/home` changes the target home;
`PLATFORM=darwin|linux` selects package defaults. No directory migration is required.
The Makefile calls the self-contained `deploy.sh`; package lists live in that script.
You can also run `./deploy.sh check`, `install`, `status`, or `uninstall`, with
`--home /existing/home` and optional `--platform darwin|linux` / `--packages zsh tmux`.
Requires Bash and GNU Stow; Make is optional.
Conflicts are reported without adopting or overwriting files. Checks run before
deployment, but applying changes is not an atomic transaction.
See [agent instructions](AGENTS.md). In a grouped setup, the parent directory has
its own Makefile and agent instructions.

