############################################
#               compdefs
#############################################

# git {{{
compdef g=git
compdef _git gd=git-diff
# }}}

compdef mux=tmuxinator 2>/dev/null

# tmux {{{
# stolen from completion function _tmux
__tmux-sessions() {
    local expl
    local -a sessions
    sessions=( ${${(f)"$(command tmux list-sessions)"}/:[ $'\t']##/:} )
    _describe -t sessions 'sessions' sessions "$@"
}
compdef __tmux-sessions t
compdef __tmux-sessions tks
# }}}

# edit config files {{{
compdef '_arguments -C "1:Select a config file to edit:(slate i3 tmux vim zsh)"' e
# }}}

#compdef '_arguments -C "1:List supported types:(list)"' gi

