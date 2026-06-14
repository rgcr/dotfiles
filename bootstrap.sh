#!/usr/bin/env bash

BACKUPDIR=${HOME}/.dotfiles.bak

_has(){
    command type "${1}" > /dev/null 2>&1
}

_die(){
    >&2 echo "$@"
    exit 1
}

_confirm(){
    read -r -p "${1:-Are you sure? [y/N]} " response
    case "$response" in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
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
    for d in $(find . -mindepth 1 -maxdepth 1 ! -path ./.git ! -path i3-hibernate ! -path sway-hibernate -type d -printf "%f\n"); do
        stow --no-folding -vR ${d} -d . -t ~
    done
}

_deploy_i3_hibernate(){
    printf "\nDeploy i3 hibernate system config\n"
    sudo rsync -rvzh i3-hibernate/ /
    _hibernate_msg
}

_deploy_sway_hibernate(){
    printf "\nDeploy Sway hibernate system config\n"
    sudo rsync -rvzh sway-hibernate/ /
    _hibernate_msg
}

_hibernate_msg(){
    cat <<_EOL_

Hibernate config copied.
Reboot when convenient so systemd-logind picks up the new lid and power-key
settings. Avoid restarting systemd-logind from inside your graphical session.

_EOL_
}

_help(){
    cat <<_EOL_
Usage:
    ./bootstrap.sh --stow
    ./bootstrap.sh --i3-hibernate
    ./bootstrap.sh --sway-hibernate
    ./bootstrap.sh --help

Options:
    --stow              Deploy normal home dotfiles with GNU Stow.
    --i3-hibernate      Deploy i3 system hibernate config to /etc.
    --sway-hibernate    Deploy Sway system hibernate config to /etc.
    -h, --help          Show this help.

Notes:
    Hibernate configs require sudo privileges and are not deployed with stow.
    Reboot after deploying hibernate configs so systemd-logind reads them.

Requirements:
    - Install antibody:
    curl -sL git.io/antibody | sh -s

    - vim-plug for VIM:
    mkdir -p ~/.vim/autoload;
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    - Install all Vim plugins automatically:
    vim +PlugInstall +qall

_EOL_
}

_require_stow(){
    if ! _has "stow"; then
        _die '"stow" not found, you need to install stow'
    fi
}

if [ "$#" -eq 0 ]; then
    _help
    exit 0
fi

while [ "$#" -gt 0 ]; do
    case "$1" in
        --stow)
            _require_stow
            _confirm && _deploy
            ;;
        --i3-hibernate)
            _deploy_i3_hibernate
            ;;
        --sway-hibernate)
            _deploy_sway_hibernate
            ;;
        -h|--help)
            _help
            ;;
        *)
            _help
            _die "Unknown option: $1"
            ;;
    esac
    shift
done
