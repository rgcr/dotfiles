if [ ! -f $HOME/.antigen/antigen.zsh ]; then
    git clone https://github.com/zsh-users/antigen.git $HOME/.antigen
fi

# I hate which with whence function
if [ -x "/usr/bin/which"  ]; then 
    alias which='/usr/bin/which'; 
fi 

###############################################################
########################## ANTIGEN ############################
###############################################################
source ~/.antigen/antigen.zsh

# DETECT OS
case $(uname) in 
    Linux)
        IS_LINUX=1 ;;
    Darwin)
        IS_MAC=1 ;;
    *)
        ;;
esac

[ -x `which brew 2>/dev/null`  ] && HAS_BREW=1; 
[ -x `which pacman 2>/dev/null` ] && HAS_PACMAN=1; 

# Load the oh-my-zsh's library.
antigen use oh-my-zsh
# bundles
antigen bundles <<EOBUNDLES
    ## Bundles from the default repo (robbyrussell's oh-my-zsh).
    vi-mode
    git
    gitfast
    heroku
    vagrant
    tmux
    ## python
    virtualenvwrapper
    pip
    django
    ## ruby
    rvm
    rails
    ## nodejs
    node
    npm
    ## Syntax highlighting bundle.
    zsh-users/zsh-syntax-highlighting
    ## list files
    rimraf/k
    ## plugin
    rupa/z
EOBUNDLES

[ $IS_MAC -eq 1 ] && antigen bundle osx
[ $HAS_BREW -eq 1 ] && antigen bundle brew
[ $HAS_PACMAN -eq 1 ] && antigen bundle archlinux

# Load the theme.
antigen theme ~ .rogerthat

# Tell antigen that you're done.
antigen apply

###############################################################
#################### USER PERSONALIZATION  ####################
###############################################################
WORKPROFILE="$HOME/.zshrc-work_profile"
# vi mode
set -o vi
# search on history with ctrl+r
bindkey '^R' history-incremental-search-backward
setopt histignorespace
# silent no match 
unsetopt nomatch



###### VARIABLES #####
export EDITOR=vim
export WORKON_HOME=$HOME/.venvs

##### PYTHON / DJANGO  #####
pmrs() { PORT=${1:-8080}; python manage.py runserver 0.0.0.0:${PORT}    }
pmrsp() { PORT=${1:-8080}; python manage.py runserver_plus 0.0.0.0:${PORT}  }

alias pm='python manage.py'
alias pmmm='python manage.py makemigrations'
alias pms='python manage.py syncdb'
alias pmm='python manage.py migrate'
alias pmsh='python manage.py shell'
alias pmshp='python manage.py shell_plus'

##### NVM #####
export NVM_DIR=~/.nvm
if [[ $HAS_BREW -eq 1 && -e "$(brew --prefix nvm 2>/dev/null)/nvm.sh" ]]; then
    source $(brew --prefix nvm)/nvm.sh
elif [ -e "$HOME/.nvm/nvm.sh" ]; then
    source $HOME/.nvm/nvm.sh
fi

##### PROXY #####
setproxy() {
    p=${1}; np=${2:-localhost}
    no_proxy=$np; NO_PROXY=$np;
    http_proxy=$p; HTTP_PROXY=$p; https_proxy=$p; HTTPS_PROXY=$p; ftp_proxy=$p; FTP_PROXY=$p;
    export http_proxy https_proxy HTTP_PROXY HTTPS_PROXY ftp_proxy FTP_PROXY no_proxy NO_PROXY;
}

noproxy() {
    unset http_proxy https_proxy ftp_proxy no_proxy HTTP_PROXY HTTPS_PROXY FTP_PROXY NO_PROXY;
    echo "unset proxy!";
    RPS1="$_PREV_RPS1";
}

alias unsetproxy='noproxy'


##### TRANSLATION #####
alias etos='trans -brief en:es'
alias stoe='trans -brief es:en'

[ -f "$WORKPROFILE" ] && source "$WORKPROFILE";

##### SHOW INFO ABOUT PROXY ######
if [ $(env 2>&1 | grep -i proxy 2>&1 | grep -v rvm 2>&1 >/dev/null; echo $?) -eq 0 ]; then
    export _PREV_RPS1=$RPS1
    RPS1="$RPS1 <proxy: $http_proxy>";
else
    RPS1="$_PREV_RPS1";
fi
