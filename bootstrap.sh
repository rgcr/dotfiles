#!/usr/bin/env bash


_has(){
    command type "${1}" > /dev/null 2>&1
}



BACKUPDIR=${HOME}/.dotfiles.bak

if ! _has "stow" then
    echo '"stow" not found, you need to install stow'
    exit 1
fi

mkdir -p ${BACKUPDIR}
for f in ${HOME}/.zshrc* ${HOME}/.vim* ${HOME}/.i3* ${HOME}/.tmux* ${HOME}/.zplug*; do
    mv -vf ${f} ${BACKUPDIR}/ 2>/dev/null
done

rm -fv ${HOME}/.zcompdump*

# deploy dotfiles with stow
printf "\nRestow dotfiles\n"
for d in $(find . -mindepth 1 -maxdepth 1 ! -path ./.git -type d -printf "%f\n"); do
    stow -R ${d}
done

cat <<_EOL_

Run next command to install all vim plugins:
  vim +PlugInstall +qall

_EOL_
