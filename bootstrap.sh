#!/bin/sh

BACKUPDIR=$HOME/.dotfiles.bak
ZGENDIR=$HOME/.zgen/

if ! which stow >/dev/null 2>&1; then
    echo '"stow" not found, you need to install stow'
    exit 1
fi

mkdir -p $BACKUPDIR
echo "backup in $HOME/..."
for f in $HOME/.zshrc* $HOME/.vim* $HOME/.oh-my-zsh $HOME/.i3 $HOME/.tmux* $HOME/.zgen*; do
    mv -vf $f $BACKUPDIR/ 2>/dev/null
done

#install vundle
echo ""
echo "vundle..."
git clone https://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle

#install zgen
git clone https://github.com/tarjoilija/zgen.git $ZGENDIR

echo ""
echo "Restow dotfiles..."
for d in $(find . -maxdepth 1 -path ./.git -prune -o -type d -printf "%f " | sed 's|\.||g'); do
    stow -R $d
done

rm -fv $HOME/.zcompdump*
echo "installing vim plugins..."
vim +PluginInstall +qall
