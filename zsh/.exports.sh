#############################################
#               Envs
#############################################

# HISTORY {{{
HISTFILE=$HOME/.sh_history
HISTSIZE=10000
SAVEHIST=50000
# }}}

[ "${ZSH_UNAME}" = "Darwin" ] && {
    export LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8
}

export EDITOR=vim

# virtualenvwrapper {{{
unset  VIRTUAL_ENV_DISABLE_PROMPT
export WORKON_HOME=$HOME/.venvs
# }}}

# NVM
export NVM_DIR=~/.nvm

# FZF {{{
# if which "fzf" >/dev/null 2>&1; then
# export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_DEFAULT_COMMAND='rg --files --hidden'
# fi
# }}}

export PYTHONDONTWRITEBYTECODE=1
