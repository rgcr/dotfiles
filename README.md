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

### To remove a specific config

```bash
stow --no-folding -d . -t ~ -vD <config>
```

### To remove all configs

```bash
stow --no-folding -d . -t ~ -vD *
```

> ⚠️ **Do not use `bootstrap.sh`**, as it may break your current configuration.

---

## Notes

### `i3-hibernate` config requires `sudo` privileges

```bash
sudo rsync -rvzh i3-hibernate/ /
```
