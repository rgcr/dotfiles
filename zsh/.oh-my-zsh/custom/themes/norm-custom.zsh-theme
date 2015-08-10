#PROMPT='%{$fg[yellow]%}λ %m %{$fg[green]%}%c %{$fg[yellow]%}→ $(git_prompt_info)%{$reset_color%}'
PROMPT='%{$fg[green]%}%B%T%b%{$fg[cyan]%} @%n(%~)%{$fg[yellow]%}$(git_prompt_info) %{$fg[green]%}%%%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX=" "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
