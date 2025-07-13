#############################################
#               Aliases
#############################################
# General {{{
# I hate the 'whence' function
# command which which>/dev/null 2>&1 && {
    # alias which="$(command which which)"
# }


# alias :q='exit'
alias ..='cd ..'
alias ...='cd ../../'
alias ls='ls --color=always'
alias ll='ls -l'
alias lla='ls -la'
alias week='date +%V'
# }}}

# 'man' formmat for bat to pipe '--help' outputs
alias batman='bat --language=man --style=plain --paging=always --theme=TwoDark'

# alias for my dotfiles
alias boostrap-config='stow --no-folding -d . -t ~ -vR'

# systemctl {{{
alias ctl='sudo systemctl'
# }}}

#
# vim / neovim {{{
alias v='vim'
alias vv='vim $(fzf)'
alias n='nvim'
alias nn='nvim $(fzf)'
# }}}

# git {{{
alias g='git'
alias gc='git commit -m'
alias gst='git status'
alias gb='git branch -v'
alias gd='git diff'
alias gp='git pull'
alias gpom='git pull origin master'
alias git-user='git config user.name'
alias git-name='git config user.name'
alias git-email='git config user.email'
alias git-mail='git config user.email'
# }}}

# docker {{{
alias d='docker'
alias dps='docker ps -a'
# }}}

# tmux {{{
alias tks='tmux kill-session -t'
# }}}

# python {{{
# alias py='python'
# alias lsvirtualenv='lsvirtualenv -b'
# alias pip='_pip'
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

# Arch yay package manager and fzf {{{
# search and install package
alias ys='fyay'
# search and remove package
alias yr='fyay_rm'
# }}}

# MacOS
alias b='brew'

# emacs
alias em='emacs -nw'

# sudo
# alias s='sudo'

# yazi
alias yz='yazi'

# copilot
alias gce='gh copilot explain'
alias gcs='gh copilot suggest'

