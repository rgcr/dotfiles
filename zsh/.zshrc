#!/usr/bin/env zsh

WORKPROFILE="${HOME}/.zshrc.work"
MYPROFILE="${HOME}/.zshrc.local"
ALIASES="${HOME}/.zshrc.aliases"
FUNCTIONS="${HOME}/.zshrc.functions"

ZGEN_RESET_ON_CHANGE=(${MYPROFILE} ${ALIASES} ${FUNCTIONS})

# I hate which with the whence function
command which which>/dev/null 2>&1 && {
    alias which="$(command which which)"
}

_has(){
    command type "$1" > /dev/null 2>&1
}

# DETECT OS
case $(uname) in
    {Linux})
        IS_LINUX="true"
        #
        _has "pacman" && HAS_PACMAN="true"
        ;;
    Darwin)
        IS_MAC="true"
        _has "brew" && HAS_BREW="true";
        ;;
    *)
        ;;
esac

[ -e "${HOME}/.zgen/zgen.zsh" ] && source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then

    # Load my zshrc files {
    [ -f "${MYPROFILE}" ] && zgen load "${MYPROFILE}" 2>/dev/null;
    [ -f "${ALIASES}" ] && zgen load "${ALIASES}" 2>/dev/null;
    [ -f "${FUNCTIONS}" ] && zgen load "${FUNCTIONS}" 2>/dev/null;
    # }

    # Load work profile {
    [ -f "${WORKPROFILE}" ] && zgen load "${WORKPROFILE}" 2>/dev/null;
    # }

    zgen oh-my-zsh

    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/gitfast
    zgen oh-my-zsh plugins/vi-mode
    zgen oh-my-zsh plugins/heroku
    zgen oh-my-zsh plugins/vagrant
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose
    zgen oh-my-zsh plugins/tmux

    zgen oh-my-zsh plugins/virtualenvwrapper
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/django

    zgen oh-my-zsh plugins/rvm

    zgen oh-my-zsh plugins/node
    zgen oh-my-zsh plugins/npm
    zgen oh-my-zsh plugins/yarn

    zgen oh-my-zsh plugins/tmuxinator

    [ "${IS_MAC}" = "true" ] && zgen oh-my-zsh plugins/osx
    [ "${HAS_BREW}" = "true" ] && zgen oh-my-zsh plugins/brew
    [ "${HAS_PACMAN}" = "true" ] && zgen oh-my-zsh plugins/archlinux

    zgen load rimraf/k
    zgen load rupa/z
    zgen load sharat87/zsh-vim-mode
    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-syntax-highlighting

    # Load my theme {
    zgen load $HOME/.rogerthat.zsh-theme 2>/dev/null
    # }

    # save all to init script
    zgen save
fi

###################################################################
# # "zsh-history-substring-search" plugin
# # => Key bindings (for UP and DOWN arrow keys)
zmodload zsh/terminfo
if [ "${IS_MAC}" = "true" ]; then
    #bindkey "$terminfo[cuu1]" history-substring-search-up
    #bindkey "$terminfo[cud1]" history-substring-search-down
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
fi
