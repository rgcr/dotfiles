############################################
#             Functions
#############################################

#     autoload -U colors zsh/terminfo
#     colors
#
#     autoload -Uz vcs_info
#     zstyle ':vcs_info:*' enable git hg
#     zstyle ':vcs_info:*' check-for-changes true
#     zstyle ':vcs_info:git*' formats "%{${fg[cyan]}%}[%{${fg[green]}%}%s%{${fg[cyan]}%}][%{${fg[blue]}%}%r/%S%%{${fg[cyan]}%}][%{${fg[blue]}%}%b%{${fg[yellow]}%}%m%u%c%{${fg[cyan]}%}]%{$reset_color%}"
#
#     setopt prompt_subst
#
#     if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]; then
#         p_host='%F{yellow}%M%f'
#     else
#         p_host='%F{cyan}%M%f'
#     fi
#
#     PS1=${(j::Q)${(Z:Cn:):-$' %F{cyan}[%f %(!.%F{red}%n%f.%F{cyan}%n%f) %F{cyan}@%f ${p_host} %F{cyan}][%f %F{blue}%~%f %F{cyan}]%f %(!.%F{cyan}%#%f.%F{cyan}%#%f) " " '}}
#
#     PS2=$'%_>'
#     RPROMPT=$'${vcs_info_msg_0_}'
# }


#######################################
# => tmux
#######################################
## shortcut for creating/attaching named sessions
t() {
    [ -z "$1" ] && { tmux list-sessions; return 1; }

    local _session="${1}"; shift
    tmux attach -t "${_session}" "${@}" 2>/dev/null || \
        { echo "Starting new session..." && tmux new -s "${_session}" "$@" }
}

# tmuxinator 'alias' funciton,
# adds a 'show' argument to show a configuration without open the editor
unalias mux 2>/dev/null
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
# }}}


#######################################
# => python
#######################################
# {{{
# create flake config
flake8-mkconfig(){
    cat <<__EOF__ > .flake8
[flake8]
ignore = E116
# E116: unexpected indentation (comment)
# E402:  Module level import not at top of file
__EOF__
    echo ".flake8 was created"
}

# install dev packages
py-devpackages(){
    pipenv install --dev jedi flake8 ipython # yapf
    pipenv install --dev --pre black
}

# Django - runserver
pmrs() { PORT=${1:-8000}; python manage.py runserver 0.0.0.0:${PORT}    }
pmrsp() { PORT=${1:-8000}; python manage.py runserver_plus 0.0.0.0:${PORT}  }
# }}}


#######################################
# => mariadb / mysql
#######################################
# {{{
mdb(){
    DBHOST="${DBHOST:-127.0.0.1}"
    DBUSER="${DBUSER:-root}"
    DBPASSWORD="${DBPASSWORD}"
    
    local _login=""
    if [ -n "${DBPASSWORD}" ]; then
        _login="-p${DBPASSWORD}"
    fi
        
    if [ "${DBUSER}" = "root" ]; then
        sudo mysql -h "${DBHOST}" -u "${DBUSER}" "${_login}" "${@}"
    else
        mysql -h "${DBHOST}" -u "${DBUSER}" "${_login}" "${@}"
    fi
}

mdb-list-users(){
    mdb -e "SELECT user, host FROM mysql.user;"
}

mdb-create-user(){
    [ -z "${1}" ] \
        && echo "usage: mdb-create-user <username> <password> [host]" \
        && return 1
    mdb -e "CREATE USER IF NOT EXISTS ${1}@${3:-localhost} IDENTIFIED BY '${2}'";
}

mdb-drop-user(){
    [ -z "${1}" ] \
        && echo "usage: mdb-drop-user <username> [host]" \
        && return 1
    mdb -e "DROP USER IF EXISTS ${1}@${2:-localhost};"
}

mdb-show-permissions(){
    [ -z "${1}" ] \
        && echo "usage: mdb-show-permissions <username> [host]" \
        && return 1
    mdb -e "SHOW GRANTS FOR ${1}@${2:-localhost};"
}

mdb-grant-all(){
    [ -z "${1}" ] \
        && echo "usage: mdb-grant-all <db.table> <username> [host]" \
        && echo "" \
        && echo '  e.g.  $ mdb-grant-all mydb.mytable myuser' \
        && echo '  e.g.  $ mdb-grant-all "*.*" myuser localhost' \
        && return 1
    mdb -e "GRANT ALL PRIVILEGES ON ${1} TO ${2}@${3:-localhost};"
}

mdb-revoke-all(){
    [ -z "${1}" ] \
        && echo "usage: mdb-revoke-all <username>" \
        && return 1
    mdb -e "REVOKE ALL PRIVILEGES, GRANT OPTION FROM ${1};"
}

mdb-show-databases(){
    mdb -e "SHOW DATABASES;"
}

mdb-show-tables(){
    [ -z "${1}" ] \
        && echo "usage: mdb-shot-tables <db>" \
        && return 1
    mdb -e "USE ${1}; SHOW TABLES;"
}
#}}}


#######################################
# => fzf
#######################################
# {{{
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
    eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) \
        | fzf +s --tac | sed 's/ *[0-9]* *//')
}
# }}}


#######################################
# => git
#######################################

# creates ignore files for git
gi() {
    curl -L -s https://www.gitignore.io/api/${@}
}


#######################################
# => utils
#######################################

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

# function to edit config files
e(){
    [ -z "${1}" ] && { 2>&1 echo "usage: e <configfile>"; return 1}
    case "${1}" in
        vim)   "${EDITOR}" ~/.vimrc                ;;
        nvim)  "${EDITOR}" ~/.config/nvim/init.vim ;;
        zsh)   "${EDITOR}" ~/.zshrc                ;;
        tmux)  "${EDITOR}" ~/.tmux.conf            ;;
        slate) "${EDITOR}" ~/.slate                ;;
        i3)    "${EDITOR}" ~/.config/i3/config            ;;
    esac
}

# better history
history-all() { history -E 1 }

take() { mkdir -p $1; cd $1 }

# cli calculator
calc() { echo "${@}" | bc -l }

# set proxy envs
setproxy() {
    p=${1}; np=${2:-localhost}
    no_proxy=$np; NO_PROXY=$np;
    http_proxy=$p; HTTP_PROXY=$p; https_proxy=$p;
    HTTPS_PROXY=$p; ftp_proxy=$p; FTP_PROXY=$p;
    export http_proxy https_proxy HTTP_PROXY HTTPS_PROXY \
        ftp_proxy FTP_PROXY no_proxy NO_PROXY;
}

# unset proxy
noproxy() {
    unset http_proxy https_proxy ftp_proxy no_proxy \
        HTTP_PROXY HTTPS_PROXY FTP_PROXY NO_PROXY;
    printf "unset proxy!\n";
}

# init antibody plugins
antibody_init(){
    _bundles=${1:-${HOME}/.zsh/bundles.txt}
    antibody bundle < ${_bundles} > ~/.zsh_plugins.sh
    chmod 755 ~/.zsh_plugins.sh
}

# play music recursively with mplayer
playall() {
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

# youtube-dl, download mp3
ymp3(){
    youtube-dl --extract-audio --audio-format mp3 \
        --audio-quality 5 --ignore-errors "${@}"
}

# get lyrics
lyric(){
    { [ -z "{$1}" ] || [ -z "${2}" ] } && \
        { 2>&1 echo 'usage lyric <artist> <title>'; return 1 }
    curl -s \
        --get "https://makeitpersonal.co/lyrics" \
        --data-urlencode "artist=$1" \
        --data-urlencode "title=$2"
}
