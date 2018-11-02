############################################
#             Functions
#############################################

zsh_setprompt() {
	autoload -U colors zsh/terminfo
	colors

	autoload -Uz vcs_info
	zstyle ':vcs_info:*' enable git hg
	zstyle ':vcs_info:*' check-for-changes true
	zstyle ':vcs_info:git*' formats "%{${fg[cyan]}%}[%{${fg[green]}%}%s%{${fg[cyan]}%}][%{${fg[blue]}%}%r/%S%%{${fg[cyan]}%}][%{${fg[blue]}%}%b%{${fg[yellow]}%}%m%u%c%{${fg[cyan]}%}]%{$reset_color%}"

	setopt prompt_subst

	if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]; then 
		p_host='%F{yellow}%M%f'
	else
		p_host='%F{cyan}%M%f'
	fi

	PS1=${(j::Q)${(Z:Cn:):-$' %F{cyan}[%f %(!.%F{red}%n%f.%F{cyan}%n%f) %F{cyan}@%f ${p_host} %F{cyan}][%f %F{blue}%~%f %F{cyan}]%f %(!.%F{cyan}%#%f.%F{cyan}%#%f) " " '}}

	PS2=$'%_>'
	RPROMPT=$'${vcs_info_msg_0_}'
}

zsh_init_plugins(){
    antibody bundle < $1 > ~/.zsh_plugins.sh
    chmod 755 ~/.zsh_plugins.sh
}

# function to edit config files
e(){
    [ -z "${1}" ] && { 2>&1 echo "usage: e <configfile>"; return 1}
    case "${1}" in
        vim)   "${EDITOR}" ~/.vimrc      ;;
        zsh)   "${EDITOR}" ~/.zshrc      ;;
        tmux)  "${EDITOR}" ~/.tmux.conf  ;;
        slate) "${EDITOR}" ~/.slate      ;;
        i3)    "${EDITOR}" ~/.i3/config  ;;
    esac
}


# better history
function history-all { history -E 1 }

function take() { mkdir -p $1; cd $1 }

# cli calculator
calc() { echo "${@}" | bc -l }

# man with colors
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

## shortcut for creating/attaching named sessions
t() {
  if [ -z "$1" ]; then
      tmux list-sessions
  else
      tmux has -t $1 && tmux attach -t $1 || tmux new -s $1
  fi
}


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
    #   playall -t mp4 /tmp /home .
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

# tmuxinator 'alias' funciton,
# adds a 'show' argument to show a configuration without open the editor
unalias mux 2> /dev/null
mux(){
    case "${1}" in
        "show")
	        cat "${HOME}/.config/tmuxinator/${2}.yml" 2>/dev/null
        ;;
        *)
            command tmuxinator "${@}" 2>/dev/null
	;;
    esac
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


# creates ignore files for git
gi() {
    curl -L -s https://www.gitignore.io/api/${@}
}

# get lyrics
lyric(){
    { [ -z "{$1}" ] || [ -z "${2}" ] } && { 2>&1 echo 'usage lyric <artist> <title>'; return 1 }
    curl -s --get "https://makeitpersonal.co/lyrics" --data-urlencode "artist=$1" --data-urlencode "title=$2"
}
