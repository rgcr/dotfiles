#############################################
#               Envs
#############################################

# HISTORY {{{
HISTFILE=$HOME/.sh_history
HISTSIZE=10000
SAVEHIST=50000
# }}}

[ "$(uname)" = "Darwin" ] && {
    export LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8
}

export EDITOR=nvim

# virtualenvwrapper {{{
unset  VIRTUAL_ENV_DISABLE_PROMPT
export WORKON_HOME=$HOME/.venvs
# }}}

# NVM
export NVM_DIR=~/.nvm

# FZF
export FZF_DEFAULT_COMMAND="rg --files --hidden --ignore-vcs -L"

# PYTHON
export PYTHONDONTWRITEBYTECODE=1

# GO
export GOPATH=${HOME}/.go
