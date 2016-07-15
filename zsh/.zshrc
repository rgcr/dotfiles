#!/bin/zsh

WORKPROFILE="${HOME}/.zshrc-work_profile"
LOCALPROFILE="${HOME}"/.zshrc.local

# I hate which with whence function
if [ -x "/usr/bin/which"  ]; then
    alias which='/usr/bin/which';
fi

# DETECT OS
case $(uname) in
    Linux)
        IS_LINUX=1 ;;
    Darwin)
        IS_MAC=1 ;;
    *)
        ;;
esac

[ -x "$(which brew 2>/dev/null)" ] && HAS_BREW=1;
[ -x "$(which pacman 2>/dev/null)" ] && HAS_PACMAN=1;

[ -e "${HOME}/.zgen/zgen.zsh" ] && source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then

    ################################################################
    # Load my profile
    [ -f "$LOCALPROFILE" ] && zgen load "$LOCALPROFILE" 2>/dev/null;
    # Load work profile
    [ -f "$WORKPROFILE" ] && zgen load "$WORKPROFILE" 2>/dev/null;
    ################################################################

    zgen oh-my-zsh

    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/gitfast
    zgen oh-my-zsh plugins/vi-mode
    zgen oh-my-zsh plugins/heroku
    zgen oh-my-zsh plugins/vagrant
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/tmux

    zgen oh-my-zsh plugins/virtualenvwrapper
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/django

    zgen oh-my-zsh plugins/rvm

    zgen oh-my-zsh plugins/node
    zgen oh-my-zsh plugins/npm

    [ "$IS_MAC" -eq 1 ] && zgen oh-my-zsh plugins/osx
    [ "$HAS_BREW" -eq 1 ] && zgen oh-my-zsh plugins/brew
    [ "$HAS_PACMAN" -eq 1 ] && zgen oh-my-zsh plugins/archlinux

    zgen load rimraf/k
    zgen load rupa/z
    zgen load sharat87/zsh-vim-mode
    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-syntax-highlighting

    ################################################################
    # Load my theme
    zgen load $HOME/.rogerthat.zsh-theme 2>/dev/null
    ################################################################

    # save all to init script
    zgen save
fi

###################################################################
# # "zsh-history-substring-search" plugin
# # => Key bindings (for UP and DOWN arrow keys)
zmodload zsh/terminfo
if [ "$IS_MAC" -eq 1 ]; then
    #bindkey "$terminfo[cuu1]" history-substring-search-up
    #bindkey "$terminfo[cud1]" history-substring-search-down
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
fi
