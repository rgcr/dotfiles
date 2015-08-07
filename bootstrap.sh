#!/bin/sh 

if ! which stow >/dev/null 2>&1; then
    echo '"stow" not found, you need to install stow'
    exit 1
fi

# backup 
echo "backup..."
for f in $HOME/.zshrc* $HOME/.vimrc $HOME/.oh-my-zsh $HOME/.vim $HOME/.i3 $HOME/.tmux.conf; do
    cp -rf $f ${f}.bak 2>/dev/null
done

# install oh-my-zsh
echo "oh-my-zsh..."
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

#install vundle
echo "vundle..."
git clone https://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle

echo "dotfiles..."
for d in $(find . -maxdepth 1 -path ./.git -prune -o -type d -printf "%f " | sed 's|\.||g'); do
    stow $d
done
