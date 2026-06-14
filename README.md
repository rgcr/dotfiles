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
./bootstrap.sh --stow
```

### To remove a specific config

```bash
stow --no-folding -d . -t ~ -vD <config>
```

### To remove all configs

```bash
stow --no-folding -d . -t ~ -vD *
```

Run `./bootstrap.sh` with no parameters to show usage.

---

## Notes

### `i3-hibernate` config requires `sudo` privileges

```bash
./bootstrap.sh --i3-hibernate
```

### `sway-hibernate` system sleep config requires `sudo` privileges

```bash
./bootstrap.sh --sway-hibernate
```

This uses `suspend-then-hibernate`: the laptop suspends first, then writes RAM to
disk after 30 minutes so the session can survive a drained battery. This requires
working Linux hibernation support, including a swap partition or swap file
configured as the kernel resume device.

In Sway, the power button is handled by a `swaynag` confirmation prompt. Lid close
still uses `suspend-then-hibernate` immediately.

Reboot after deploying hibernate config so `systemd-logind` reads the new lid and
power-key settings. Avoid restarting `systemd-logind` from inside the graphical
session because it can disrupt the active desktop session.
