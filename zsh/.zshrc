# vi mode
set -o vi

# ZSH_UNAME=$(uname)

autoload -Uz compinit 
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24)  ]]
then
    compinit;
else
    compinit -C;
fi

#############################################
#               Key bindings
#############################################

# UP and DOWN arrow keys
zmodload zsh/terminfo
if [ "${ZSH_UNAME}" = "Darwin" ]; then
    #bindkey "$terminfo[cuu1]" history-substring-search-up
    #bindkey "$terminfo[cud1]" history-substring-search-down
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
fi
# search on history with ctrl+r
bindkey '^R' history-incremental-search-backward
# vi-mode, search on hisotry with '?'
bindkey -M vicmd '?' history-incremental-search-backward

# source ~/.zsh/thzshrcemes/rho.zsh-theme

# antibody plugins
[ ! -f "${HOME}/.zsh_plugins.sh" ] || source "${HOME}/.zsh_plugins.sh"


#############################################
#               Options
#############################################

# {{{
setopt PROMPT_SUBST
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

# enable colors {{{
export CLICOLOR=1
if [ "${ZSH_UNAME}" = "Darwin" ]; then
    #export LSCOLORS="exfxcxdxbxegedabagacad"
    export LSCOLORS="ExGxFxDxCxDxDxhbhdacEc"
    LS_COLORS="${LSCOLORS}"
else
    export LS_COLORS="di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32"
fi
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# }}}

# history {{{
setopt inc_append_history
setopt share_history
# }}}

############################################
#               Source
#############################################

[ ! -f "${HOME}/.aliases.sh" ] || source "${HOME}/.aliases.sh"
[ ! -f "${HOME}/.exports.sh" ] || source "${HOME}/.exports.sh"
[ ! -f "${HOME}/.functions.sh" ] || source "${HOME}/.functions.sh"
[ ! -f "${HOME}/.completions.sh" ] || source "${HOME}/.completions.sh"

# local config
[ ! -f "${HOME}/.profile.local" ] || source "${HOME}/.profile.local"
