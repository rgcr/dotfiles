############################################
#           Helper Functions
############################################

# colors
_color_red='\033[1;31m'
_color_green='\033[1;32m'
_color_yellow='\033[1;33m'
_color_blue='\033[1;34m'
_color_magenta='\033[1;35m'
_color_cyan='\033[1;36m'
_color_reset='\033[0m'

_print_error() {
    2>&1 echo -e "${_color_red}${@}${_color_reset}"
}

_print_info() {
    echo -e "${_color_cyan}${@}${_color_reset}"
}

_print_warning() {
    2>&1 echo -e "${_color_yellow}${@}${_color_reset}"
}

# function to add paths to the PATH variable and avoid duplicates
__add_to_path () {
    case ":$PATH:" in
        *":$1:"*) :;; # already there
        *) PATH="$1:$PATH";; # or PATH="$PATH:$1"
  esac
}

__remove_from_path(){
    local filtered_path=$(echo $PATH | tr ':' '\n' | grep -wv "${1}" | tr '\n' ':' | sed 's/:$//')
    export PATH=${filtered_path}
}

#######################################
# => PATH
#######################################

# wrapper function to manage the path env and avoid duplicates
path(){
    case "$1" in
        ls) _print_info $PATH | tr ':' '\n' ;;
        add) [ ! -z "${2}" ] && __add_to_path "${2}" ;;
        rm) [ ! -z "${2}" ] && __remove_from_path "${2}" ;;
        *) 2>&1 echo "usage: path [ls | add <path>| rm <path>]"
    esac

}

#######################################
# => tmux
#######################################

## shortcut for creating/attaching named sessions
t() {
    [ -z "$1" ] && {
        tmux list-sessions 2>/dev/null || _print_warning "There are no sessions" ; return 1;
    }

    local _session="${1}"; shift
    tmux attach -t "${_session}" "${@}" 2>/dev/null || \
        { _print_info "Starting new session..." && tmux new -s "${_session}" "$@" }
}

# tmuxinator 'alias' funciton,
# adds a 'show' argument to show a configuration without open the editor
mux(){
    case "$1" in
        rm)
            shift
            if [ -z "$1" ]; then
                2>&1 echo "Usage: mux rm <project_name>"
                return 1
            fi
            rm -iv ~/.config/tmuxp/${1}.yaml 2>/dev/null
            rm -iv ~/tmuxp/${1}.yaml 2>/dev/null
            return 0
            ;;
        *)
            # if first argument is a session name then attach to it
            if [[ $# -eq 1 ]] && command tmuxp ls | grep -Fx $1 &>/dev/null; then
                command tmuxp load "${1}"
                return 0
            else
                command tmuxp "$@"
            fi
            ;;
    esac
}
# }}} --end tmux functions


#######################################
# => python
#######################################
# {{{

# create flake config
# flake8-mkconfig(){
#     cat <<__EOF__ > .flake8
# [flake8]
# ignore = E116
# # E116: unexpected indentation (comment)
# # E402:  Module level import not at top of file
# __EOF__
#     echo ".flake8 was created"
# }

uv(){
    command uv "$@"
    if [ $? -eq 0 -a "${1}" = "init" -a "${2}" != "--help" ]; then
        cat <<__EOF__ > .pyrightconfig.json
{
  "typeCheckingMode": "basic",
  "reportMissingImports": true,
  "reportMissingModuleSource": true,
  "venvPath": ".",
  "venv": ".venv",
  "extraPaths": ["./src"]
}
__EOF__
    fi
}

# Django - runserver
pmrs() { PORT=${1:-8000}; python manage.py runserver 0.0.0.0:${PORT}    }
pmrsp() { PORT=${1:-8000}; python manage.py runserver_plus 0.0.0.0:${PORT}  }

# }}} -- end python functions

# Find and activate uv-created python virtual environments relative to cwd
activate() {
    setopt localoptions ksharrays

    local requested=""
    local show_help=0 show_list=0
    local -a python_bins venvs unique_venvs
    local -A seen
    local python_bin venv_dir target activate_script

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                show_help=1
                ;;
            -l|--ls)
                show_list=1
                ;;
            *)
                requested="$1"
                shift
                break
                ;;
        esac
        shift
    done

    if (( show_help )); then
        echo "usage: activate [-l|--ls] [venv]"
        return 0
    fi

    while IFS= read -r python_bin; do
        [[ -z "$python_bin" ]] && continue
        local bin_dir="${python_bin%/python}"
        venv_dir="${bin_dir%/bin}"
        [[ -d "$venv_dir" ]] && venvs+=("$venv_dir")
    done < <(find . \( -type f -o -type l \) -path '*/bin/python' 2>/dev/null)

    for venv_dir in "${venvs[@]}"; do
        [[ -z "$venv_dir" ]] && continue
        if [[ -z "${seen[$venv_dir]}" ]]; then
            seen[$venv_dir]=1
            unique_venvs+=("$venv_dir")
        fi
    done
    venvs=("${unique_venvs[@]}")

    if [[ ${#venvs[@]} -eq 0 ]]; then
        _print_warning "No uv virtual environments found under $(pwd)"
        return 1
    fi

    if (( show_list )); then
        printf '%s\n' "${venvs[@]}"
        return 0
    fi

    if [[ -n "$requested" ]]; then
        if [[ -d "$requested" && -f "$requested/bin/activate" ]]; then
            target="$requested"
        else
            for venv_dir in "${venvs[@]}"; do
                if [[ "$venv_dir" == "$requested" || "$(basename "$venv_dir")" == "$requested" ]]; then
                    target="$venv_dir"
                    break
                fi
            done
        fi

        if [[ -z "$target" ]]; then
            _print_warning "Virtual environment '$requested' not found under $(pwd)"
            return 1
        fi
    else
        if [[ ${#venvs[@]} -eq 1 ]]; then
            target="${venvs[0]}"
        else
            if ! command -v fzf >/dev/null 2>&1; then
                _print_warning "fzf is required to interactively pick a virtual environment"
                _print_info "Use 'activate <venv>' to activate one of:"
                printf '  %s\n' "${venvs[@]}"
                return 1
            fi

            target=$(printf '%s\n' "${venvs[@]}" | fzf --prompt="Activate venv > " --height=40% --reverse)
            [[ -z "$target" ]] && return 1
        fi
    fi

    activate_script="$target/bin/activate"
    if [[ ! -f "$activate_script" ]]; then
        _print_error "Activate script not found at $activate_script"
        return 1
    fi

    _print_info "Activating $target"
    source "$activate_script"
}


#######################################
# => fzf
#######################################
# {{{

# fd - cd to selected directory
fcd() {
  local dir
  # dir=$(find ${1:-.} -type d 2>/dev/null | fzf --prompt="Cd to > ")
  dir=$(find ${1:-.} -type d ! \( -path '*/.venv*' -o -path '*/node_modules*' -o -path '*/.git*' \) | fzf --prompt="Cd to > ")
  [[ -n "$dir" ]] && cd "$dir"
}
alias ccd='fcd'


# fkill - kill process
fkill() {
  local pid=$(ps -eo pid,comm --no-headers | \
      fzf --prompt="Kill process > " --header="Select a process" --preview='ps -p {1} -o pid,ppid,cmd' \
      --preview-window=up:3:wrap | \
      awk '{print $1}')

  [[ -z "$pid" ]] && return

  kill "${pid}" && _print_info "Process killed <${pid}>"
}

# fh - repeat history
fh() {
    eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) \
        | fzf +s --tac | sed 's/ *[0-9]* *//')
}

# fzf / yay - search and install packages
fyay() {
    selected=$(comm -23 \
        <(expac -S '%n\t%d' | sort) \
        <(expac '%n' | sort | sed 's/$/\t/') | \
        awk -F'\t' '{ printf "\033[1;36m%s\033[0m\t\033[1;33m%s\033[0m\n", $1, $2 }' | \
        fzf --ansi \
            --prompt="Available (not installed) > " \
            --header="Select a package to install" \
            --preview='pacman -Si $(echo {} | cut -f1 | sed "s/\x1b\[[0-9;]*m//g")' \
            --delimiter='\t' \
            --with-nth=1,2 \
            --border \
            --height=60% \
            --layout=reverse \
            --preview-window=right:40%
    )
    [[ -z "${selected}" ]] && return
    pkg=$(echo "$selected" | cut -f1 | sed 's/\x1b\[[0-9;]*m//g')
    yay -S "${pkg}"
}

# fzf / yay - search and remove installed packages
fyay_rm() {
    selected=$(expac '%n\t%v\t%d' | sort | \
        awk -F'\t' '{
            printf "\033[1;36m%s\033[0m\t\033[0;32m%s\033[0m\t\033[1;33m%s\033[0m\n", $1, $2, $3
        }' | \
            fzf --ansi \
            --prompt="Installed packages > " \
            --header="Select a package to remove" \
            --delimiter='\t' \
            --with-nth=1,2,3 \
            --preview='pacman -Qi $(echo {} | cut -f1 | sed "s/\x1b\[[0-9;]*m//g")' \
            --border \
            --height=60% \
            --layout=reverse \
            --preview-window=right:40%
        )
        [[ -z "$selected" ]] && return
        pkg=$(echo "$selected" | cut -f1 | sed 's/\x1b\[[0-9;]*m//g')
        yay -Rsn "$pkg"
}
# }}} -- end fzf functions


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
    if command -v bat >/dev/null 2>&1; then
        MANWIDTH=$(tput cols)
        TERM=xterm-256color command man "$@" \
            | col -bx \
            | bat --language=man \
            --style=plain \
            --paging=always \
            --terminal-width=$MANWIDTH \
            --theme=TwoDark
    else
        TERM=xterm-256color command man "$@"
    fi
}

# expac function to search packages
yq(){
    _yellow='\033[1;33m'
    _reset='\033[0m'
    _cyan='\033[1;36m'
     expac -Ss "%r/%n %v\t%d ${1}" \ |
         awk -F'\t' '{ printf "\033[1;36m%s\033[0m\t\033[1;33m%s\033[0m\n", $1, $2 }'

}

# function to edit config files
conf(){
    [ -z "${1}" ] && { 2>&1 printf "usage: conf [zsh|tmux|slate|i3|vim|nvim|emacs]"; return 1}
    case "${1}" in
        zsh)
            "${EDITOR}" \
                ~/.zshrc ~/.aliases.sh \
                ~/.completions.sh ~/.exports.sh ~/.functions.sh
            ;;
        nvim)
            cd ~/.config/nvim/
            "${EDITOR}" init.lua
            ;;
        vim)   "${EDITOR}" ~/.vimrc            ;;
        tmux)  "${EDITOR}" ~/.tmux.conf        ;;
        slate) "${EDITOR}" ~/.slate            ;;
        i3)    "${EDITOR}" ~/.config/i3/config ;;
        emacs) "${EDITOR}" ~/.emacs.d/init.el  ;;
    esac
}

# create dir and cd to it
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
antibody-sync(){
    _bundles="${HOME}/.zsh/bundles.txt"
    _plugins_file="${HOME}/.zsh_plugins.sh"
    case "$1" in
        -f|--force)
            antibody bundle < ${_bundles} > ${_plugins_file}
            chmod 755 ${_plugins_file}
            printf ">> ${_plugins_file} was modified\n\n"
            printf "Remember to reload the terminal...\n  \$ exec zsh\n"
            ;;
        *)
            2>&1 printf "Usage: antibody-sync [-h | --help | -f | --force]\n\n"
            2>&1 printf "  Bundle file  --> ${_bundles}\n"
            2>&1 printf "  Plugins file --> ${_plugins_file}\n"
            ;;
    esac
}

# youtube-dl, download mp3
ymp3(){
    youtube-dl --extract-audio --audio-format mp3 \
        --audio-quality 5 --ignore-errors "${@}"
}

# cheat.sh - get command line cheatsheets
cheat(){
    curl -s "https://cheat.sh/${1}?style=perldoc"
}


# SSH over socks5 proxy (using dynamic port forwarding)
# e.g.
#  - using default port(4000):
#    $ ssh-socks5 <host> -l <user>
#
#  - using a custom port:
#    $ SOCKS_PORT=4444 ssh-socks5 <host> -l <user>
#
ssh-socks5(){
    [ $# -lt 1 ] && {
        >&2 printf "usage: ssh-socks5 <host> [ssh options]...\n\n"
        >&2 printf "  Note: Default socks port <4000>. If you want to change it then change the 'SOCKS_PORT' environment variable"
        return 1
    }
    SOCKS_PORT=${SOCKS_PORT:-4000}
    ssh -o ProxyCommand='nc -X 5 -x 127.0.0.1:'${SOCKS_PORT}' %h %p' -q  $*
}

scp-socks5(){
    [ $# -lt 2 ] && {
        >&2 printf "usage: scp-socks5 <file> <host> [ssh options]...\n\n"
        >&2 printf "  Note: Default socks port <4000>. If you want to change it then change the 'SOCKS_PORT' environment variable\n\n"
        return 1
    }
    SOCKS_PORT=${SOCKS_PORT:-4000}
    scp -o ProxyCommand='nc -X 5 -x 127.0.0.1:'${SOCKS_PORT}' %h %p' $*
}
