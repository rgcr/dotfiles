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

__smug_projects() {
    local -a _projects
    if [[ -d ~/.config/smug/ ]]; then
        _projects=(~/.config/smug/*.yml(N:t:r))
        if (( ${#_projects} > 0 )); then
            _describe -t projects 'smug projects' _projects "$@"
        fi
    fi
}
compdef __smug_projects s

## completion for tmux aliases
compdef __tmux-sessions t
compdef __tmux-sessions tks

# completion for mux function (tmuxp wrapper)
# __mux-sessions() {
#     local -a sessions
#     local session_files
#
#     if [[ -d ~/.tmuxp ]]; then
#         session_files=(~/.tmuxp/*.yaml(N:t:r))
#         if (( ${#session_files} > 0 )); then
#             sessions=($session_files)
#             _describe -t sessions 'tmuxp sessions' sessions "$@"
#         fi
#     fi
# }

# compdef __mux-sessions mux

# }}} -- end tmux

# alias for conf function to edit config files
compdef '_arguments -C "1:Select a config file to edit:(slate i3 tmux nvim vim zsh)"' conf
