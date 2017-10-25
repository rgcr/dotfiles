My personal configuration
=========================

Requirements
------------

* [stow](https://www.gnu.org/software/stow/)

    **Installation from Mac:** `brew install stow`

    **Installation from Arch Linux:** `pacman -S stow`


Contains dotfiles for
------------
* `zsh`: I use [zplug](https://github.com/zplug/zplug) as plugin manager
* `vim`: I use [vim-plug](https://github.com/junegunn/vim-plug) as plugin manager
* `tmux`
* `i3`
* `slate`
* `ag`


Installation
-------------

1. `cd ~; git clone https://github.com/rgcr/dotfiles`
2. `cd dotfiles; chmod +x bootstrap.sh; ./boostrap.sh`



**Note:** If you want to copy only certain configuration just run `stow -R config-that-i-want`

**Example:**
  `stow -R zsh`



My terminal
-----------
<img alt="terminal" src="https://user-images.githubusercontent.com/1203422/32017597-d1eaacf8-b98c-11e7-9102-ab540e14197b.png">

