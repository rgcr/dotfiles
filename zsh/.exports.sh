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

export EDITOR=nvim

# virtualenvwrapper {{{
unset  VIRTUAL_ENV_DISABLE_PROMPT
export WORKON_HOME=$HOME/.venvs
# }}}

# NVM
export NVM_DIR=~/.nvm

# FZF
export FZF_DEFAULT_COMMAND="rg --files --hidden --ignore-vcs -L"

export PYTHONDONTWRITEBYTECODE=1
