#############################################
#               Aliases
#############################################
#
# source zshrc
alias reload='source ~/.zshrc'

# General {{{
# I hate the 'whence' function
# command which which>/dev/null 2>&1 && {
    # alias which="$(command which which)"
# }

alias ls='ls --color=always'
# alias :q='exit'
alias ..='cd ..'
alias ...='cd ../../'
alias ll='ls -l'
alias lla='ls -la'
alias week='date +%V'
# }}}

# systemctl {{{
alias s='sudo systemctl'
# }}}

# vim / neovim {{{
alias v='vim'
alias n='nvim'
alias vo='vim $(fzf)'
# }}}

# git {{{
alias g='git'
alias gst='git status'
alias gb='git branch -v'
alias gd='git diff'
alias gp='git pull'
alias gpom='git pull origin master'
# }}}

# docker {{{
alias d='docker'
alias dps='docker ps -a'
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

# search alias quickly
alias agrep='alias 2>&1| grep '

# better jobs
alias j="jobs -l"

# better pgrep
alias pgrep='pgrep -fl'

alias b='brew'
