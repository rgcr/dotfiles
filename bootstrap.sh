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
    for d in $(find . -mindepth 1 -maxdepth 1 ! -path ./.git -type d -printf "%f\n"); do
        stow -R ${d}
    done
}

_caveat(){
    cat <<_EOL_

- Install all plugins automatically:
    vim +PlugInstall +qall

- Install vim-plug on vim
    mkdir -p ~/.vim/autoload;
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

- Install vim-plug on neovim
    mkdir -p ~/.config/nvim/;
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

_EOL_
}

##

if ! _has "stow"; then
    _die '"stow" not found, you need to install stow'
fi

rm -fv ${HOME}/.zcompdump*

_backup
_deploy
_caveat

