# My Personal Configuration

I use [**GNU Stow**](https://www.gnu.org/software/stow/) to handle my dotfiles.

### 📦 Installation
- **macOS**:  
  ```bash
  brew install stow
  ```

- **Arch Linux**:  
  ```bash
  pacman -S stow
  ```

---

## Requirements

### 📝 `vim-plug` for Vim

```bash
mkdir -p ~/.vim/autoload
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

### 🧬 `antibody` for ZSH (optional)

*(May need to install manually as it's not in all package managers)*

```bash
curl -sL git.io/antibody | sh -s
```

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

### ⚙️ ZSH config requires `antibody` to install plugins

Once deployed, install plugins with:

```bash
antibody -sync --force
```

### 💡 `i3-hibernate` config requires `sudo` privileges

```bash
sudo rsync -rvzh i3-hibernate/ /
```
