#!/usr/bin/env bash

BACKUPDIR=${HOME}/.dotfiles.bak

_has(){
    command type "${1}" > /dev/null 2>&1
}

_die(){
    >&2 echo "$@"
    exit 1
}

_backup(){
    mkdir -p ${BACKUPDIR}
    for f in ${HOME}/.zshrc* ${HOME}/.vim* ${HOME}/.i3* ${HOME}/.tmux* ${HOME}/.zplug*; do
        mv -vf ${f} ${BACKUPDIR}/ 2>/dev/null
    done
}

_deploy(){
    # deploy dotfiles with stow
    printf "\nRestow dotfiles\n"
    for d in $(find . -mindepth 1 -maxdepth 1 ! -path ./.git ! -path i3-hibernate -type d -printf "%f\n"); do
        stow -v -R ${d} -d . -t ~
    done
}

_caveat(){
    cat <<_EOL_

- i3-hibernate requries root permisions
    sudo rsync -rvzh i3-hibernate/ /

- Install antibody
    curl -sL git.io/antibody | sh -s

- Install vim-plug on vim
    mkdir -p ~/.vim/autoload;
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

- Install vim-plug on neovim
    mkdir -p ~/.config/nvim/;
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

- Install all plugins automatically:
    vim +PlugInstall +qall


_EOL_
}

##

if ! _has "stow"; then
    _die '"stow" not found, you need to install stow'
fi

rm -fv ${HOME}/.zcompdump*

# _backup
_deploy
_caveat

