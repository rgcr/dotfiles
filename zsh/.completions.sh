############################################
#               compdefs
#############################################

# git {{{
compdef g=git
compdef _git gd=git-diff
compdef _git ga=git-add
# }}} -- end git

# tmux {{{

## stolen from _tmux completion function
__tmux-sessions() {
    local expl
    local -a sessions
    sessions=( ${${(f)"$(command tmux list-sessions 2>/dev/null)"}/:[ $'\t']##/:} )
    _describe -t sessions 'sessions' sessions "$@"
}

## completion for tmux aliases
compdef __tmux-sessions t
compdef __tmux-sessions tks

# }}} -- end tmux

# alias for conf function to edit config files
compdef '_arguments -C "1:Select a config file to edit:(slate i3 tmux nvim vim zsh)"' conf
