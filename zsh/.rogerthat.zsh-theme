#!/usr/bin/env zsh
#local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

setopt promptsubst
autoload -U add-zsh-hook

#
PROMPT='$(__prompt_time) $(__prompt_userinfo) $(__promtp_path)$(git_prompt_info) $(__promtp_symbol) '

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[yellow]%}<git: "
ZSH_THEME_GIT_PROMPT_SUFFIX=">%{$reset_color%}"

function __prompt_color(){
    __PREFIX="%{$fg[$1]%}"
    __SUFIX="%{$reset_color%}"
    echo "${__PREFIX}$2${__SUFIX}"
}

function __prompt_time(){
    echo $(__prompt_color "cyan" "%T");
}

function __prompt_userinfo(){
    echo $(__prompt_color "cyan" "@%n");
}

function __promtp_path(){
    echo $(__prompt_color "cyan" "[%~]");
}

function __promtp_symbol(){
    if [ $EUID -ne 0 ]; then
        echo $(__prompt_color "cyan" "ϸ");
    else
        echo $(__prompt_color "cyan" "#");
    fi
}

function __prompt_proxy(){
    # if there is a proxy
    if [ ! -z "$http_proxy" ]; then
        # to avoid duplicate entries
        if ! echo "${RPS1}" | grep proxy >/dev/null 2>&1; then
            RPS1="$RPS1 <proxy: $http_proxy>"
        fi
    else
        # sed for osx / gnu linux
        RPS1="$( echo $RPS1 2>&1 | sed -E 's|<proxy.+>||g' 2>/dev/null || echo $RPS1 2>&1 | sed -r 's|<proxy.+>||g' 2>/dev/null  )"
    fi
}

add-zsh-hook precmd __prompt_proxy
