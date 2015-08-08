#!/bin/sh 

if ! which stow >/dev/null 2>&1; then
    echo '"stow" not found, you need to install stow'
    exit 1
fi

echo ""
echo "delete previous symlinks..."
for f in $(find $HOME/.zshrc* $HOME/.vim* $HOME/.i3 $HOME/.tmux* -type l); do
    rm -rvf $f
done

# backup 
echo ""
echo "backup..."
for f in $HOME/.zshrc* $HOME/.vim* $HOME/.oh-my-zsh $HOME/.i3 $HOME/.tmux*; do
    mv -vf $f ${f}.bak 2>/dev/null
done

# install oh-my-zsh
echo ""
echo "oh-my-zsh..."
curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

#install vundle
echo ""
echo "vundle..."
git clone https://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle

mv -vf $HOME/.zshrc $HOME/.zshrc.orig

echo ""
echo "dotfiles..."
for d in $(find . -maxdepth 1 -path ./.git -prune -o -type d -printf "%f " | sed 's|\.||g'); do
    stow $d
done

rm -fv $HOME/.zcompdump*
echo "installing vim plugins..."
vim +PluginInstall +qall
