
_has(){
    command type "$1" > /dev/null 2>&1
}

# DETECT OS
case $(uname) in
    Linux)
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

# install zplug if it does not exist
[ ! -d ~/.zplug ] && {
	git clone https://github.com/zplug/zplug ~/.zplug;
	source ~/.zplug/init.zsh && zplug update --self
}

# Essential
source ~/.zplug/init.zsh

#############################################
# Plugins
#############################################

zplug "plugins/git",            from:oh-my-zsh
zplug "plugins/gitfast",        from:oh-my-zsh
zplug "plugins/vi-mode",        from:oh-my-zsh
zplug "plugins/heroku",         from:oh-my-zsh, lazy:1
zplug "plugins/vagrant",        from:oh-my-zsh, lazy:1
zplug "plugins/docker",         from:oh-my-zsh, lazy:1
zplug "plugins/docker-compose", from:oh-my-zsh
zplug "plugins/tmux",           from:oh-my-zsh
zplug "plugins/tmuxinator",     from:oh-my-zsh
zplug "sharat87/zsh-vim-mode"
zplug "rimraf/k"
zplug "rupa/z",                 use:z.sh

# mac
zplug "plugins/osx",            from:oh-my-zsh,  if:"[[ $OSTYPE == *darwin* ]]"
zplug "plugins/brew",           from:oh-my-zsh,  if:"[[ $OSTYPE == *darwin* ]]"
# arch linux
[ "${HAS_PACMAN}" = "true" ] && {
    zplug "plugins/archlinux", from:oh-my-zsh
}

# ruby
zplug "plugins/rvm",            from:oh-my-zsh, lazy:1

# python
zplug "plugins/pip",            from:oh-my-zsh
zplug "plugins/django",         from:oh-my-zsh

[ -z "${PIPENV_ACTIVE}" ] && {
    zplug "plugins/virtualenvwrapper", from:oh-my-zsh
}

# nodejs
## lazy loading is around 70x faster
export NVM_LAZY_LOAD=true
zplug "lukechilds/zsh-nvm",     lazy:1

zplug "plugins/node",           from:oh-my-zsh
zplug "plugins/npm",            from:oh-my-zsh
zplug "plugins/yarn",           from:oh-my-zsh


zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# my theme
zplug "rgcr/dotfiles", use:"zsh/.rogerthat.zsh-theme", as:theme
#zplug "${HOME}/dotfiles/zsh", from:local, use:".rogerthat.zsh-theme", as:theme

# Install packages that have not been installed yet
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load

#############################################
# 				Key bindings
#############################################

# UP and DOWN arrow keys
zmodload zsh/terminfo
if [ "${IS_MAC}" = "true" ]; then
    #bindkey "$terminfo[cuu1]" history-substring-search-up
    #bindkey "$terminfo[cud1]" history-substring-search-down
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
fi

# search on history with ctrl+r
bindkey '^R' history-incremental-search-backward

#############################################
# 				Options
#############################################

# {{{
# vi mode
set -o vi
# Ignore <C-d> logout
setopt IGNORE_EOF
# Disable beeps
setopt NO_BEEP
# {a-c} -> a b c
setopt BRACE_CCL
# History ignoreb dups
setopt HIST_IGNORE_DUPS
# History reduce spaces
setopt HIST_REDUCE_BLANKS
# History ignore if there is a space
setopt HIST_IGNORE_SPACE
# History save time stamp
setopt EXTENDED_HISTORY
# Expand history
setopt HIST_EXPAND
# Better jobs
setopt LONG_LIST_JOBS
# Add "/" if completes directory
setopt MARK_DIRS
# Print exit value if return code is non-zero
#setopt print_exit_value
# Enable comment string
setopt INTERACTIVE_COMMENTS
# List completion
setopt AUTO_LIST
setopt MENU_COMPLETE
# Check original command in alias completion
setopt COMPLETE_ALIASES
# silent no match
unsetopt NOMATCH

# menu completion
zstyle ':completion:*' menu select
# enable colors
export CLICOLOR=1
if [ "${IS_MAC}" = "true" ]; then
    #export LSCOLORS="exfxcxdxbxegedabagacad"
    export LSCOLORS="ExGxFxDxCxDxDxhbhdacEc"
    LS_COLORS="${LSCOLORS}"
else
    export LS_COLORS="di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32"
fi

#zstyle ':completion:*' list-colors ${LS_COLORS}
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}





# Share zsh histories
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=50000
setopt inc_append_history
setopt share_history


# }}}


#############################################
# 				Alias
#############################################

# General {{{
# I hate the 'whence' function
command which which>/dev/null 2>&1 && {
    alias which="$(command which which)"
}
#if _has "nvim"; then
    #alias vim='nvim'
#fi
alias ..='cd ..'
alias ...='cd ../../'
alias ll='ls -l'
alias lla='ls -la'
alias week='date +%V'
# }}}

# edit configs {{{
alias vv='vim ~/.vimrc'
alias vt='vim ~/.tmux.conf'
alias vz='vim ~/.zshrc'
#alias vzl='vim ~/.zshrc.local'
#alias vza='vim ~/.zshrc.aliases'
#alias vzf='vim ~/.zshrc.functions'
# }}}

# source zshrc
alias sz='source ~/.zshrc'

# python {{{
alias py='python'
alias lsvirtualenv='lsvirtualenv -b'
# }}}

# Django {{{
alias pm='python manage.py'
alias pmmm='python manage.py makemigrations'
alias pms='python manage.py syncdb'
alias pmm='python manage.py migrate'
alias pmsh='python manage.py shell'
alias pmshp='python manage.py shell_plus'
# }}}

# translate-shell {{{
alias etos='trans -brief en:es'
alias stoe='trans -brief es:en'
# }}}

# vifm
alias vifm='vifm . .'

# vim-fzf
alias vo='vim $(fzf)'

# search alias quickly
alias sal='alias 2>&1| grep '

# better jobs
alias j="jobs -l"

alias pgrep='pgrep -fl'

[ "${IS_MAC}" = "true" ] && {
    alias b='brew'
}
#############################################
# 				Environment
#############################################
[ "${IS_MAC}" = "true" ] && {
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8
}
export EDITOR=vim
export WORKON_HOME=$HOME/.venvs
export NVM_DIR=~/.nvm
# FZF {{{
if type "fzf" >/dev/null 2>&1; then
    export FZF_DEFAULT_COMMAND='ag -g ""'
fi
# }}}



############################################
# 				Functions
#############################################

function history-all { history -E 1 }
function take() { mkdir -p $1; cd $1 }


# Django {{{
pmrs() { PORT=${1:-8000}; python manage.py runserver 0.0.0.0:${PORT}    }
pmrsp() { PORT=${1:-8000}; python manage.py runserver_plus 0.0.0.0:${PORT}  }
# }}}

# Proxy {{{
setproxy() {
    p=${1}; np=${2:-localhost}
    no_proxy=$np; NO_PROXY=$np;
    http_proxy=$p; HTTP_PROXY=$p; https_proxy=$p;
	HTTPS_PROXY=$p; ftp_proxy=$p; FTP_PROXY=$p;
    export http_proxy https_proxy HTTP_PROXY HTTPS_PROXY ftp_proxy FTP_PROXY no_proxy NO_PROXY;
}

noproxy() {
    unset http_proxy https_proxy ftp_proxy no_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY NO_PROXY;
    printf "unset proxy!\n";
}
# }}}

# youtube-dl, download mp3
ymp3(){
    youtube-dl --extract-audio --audio-format mp3 --audio-quality 5 --ignore-errors "${@}"
}

# mplayer
playall() {
    # play music recursively with mplayer
    # e.g.
    #   playall  # play all mp3 in the current directory
    #   playall -t mp3 .
    #   playall -t avi /home
    #   playall -f mp4 /tmp /home .
    local _p_params=""; local _p_ext="mp3";

    while [ $# -gt 0  ]; do
        case $1 in
            -t|-type|--type) _p_ext="$2" ; shift ;;
            -h|-help|--help)
                printf "usage: playall [-t|--type <filetype>] [path] ...\n" ;;
            -*) _p_params="${_p_params}${1}" ;;
            *) break ;;
        esac
        shift
    done
    [ ! -z "${@}" ] && \
        find "${@}" -name "*.${_p_ext}" -exec mplayer "${_p_params}" '{}' +
}

# tmuxinator
mux(){
    [ "${1}" = "show" ] && {
        { [ ! -z "${2}" -a -e "${HOME}/.tmuxinator/${2}.yml" ] \
			&& cat "${HOME}/.tmuxinator/${2}.yml" } \
			|| echo "Project ${2} not found" 1>&2
    } || {
        command tmuxinator "${@}"
    }
}

# fzf {{{
# open selected file with the default editor
fe() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# fkill - kill process
fkill() {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

# fh - repeat history
fh() {
  eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}
# }}}

# create git ignore files
gi() {
	curl -L -s https://www.gitignore.io/api/${@}
}

# get lyrics
lyric(){
    curl -s --get "https://makeitpersonal.co/lyrics" --data-urlencode "artist=$1" --data-urlencode "title=$2"
}


############################################
# 				Source
#############################################

[ ! -e "${HOME}/.zshrc.local" ] || source "${HOME}/.zshrc.local"


