#############################################
#               Aliases
#############################################
#
# source zshrc
alias reload='source ~/.zshrc'

# General {{{
# I hate the 'whence' function
command which which>/dev/null 2>&1 && {
    alias which="$(command which which)"
}

alias ls='ls --color=always'
alias :q='exit'
alias ..='cd ..'
alias ...='cd ../../'
alias ll='ls -l'
alias lla='ls -la'
alias week='date +%V'
# }}}

# git {{{
alias g='git'
alias gst='git status'
# }}}

# tmux {{{
alias tks='tmux kill-session -t'
# }}}

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
alias agrep='alias 2>&1| grep '

# better jobs
alias j="jobs -l"

# better pgrep
alias pgrep='pgrep -fl'

alias b='brew'
