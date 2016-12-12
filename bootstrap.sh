#!/bin/sh

BACKUPDIR=$HOME/.dotfiles.bak
ZGENDIR=$HOME/.zgen/
TPMDIR=$HOME/.tmux/plugins/tpm

if ! which stow >/dev/null 2>&1; then
    echo '"stow" not found, you need to install stow'
    exit 1
fi

mkdir -p $BACKUPDIR
echo "backup in $HOME/..."
for f in $HOME/.zshrc* $HOME/.vim* $HOME/.oh-my-zsh $HOME/.i3 $HOME/.tmux* $HOME/.zgen*; do
    mv -vf $f $BACKUPDIR/ 2>/dev/null
done

#install vim-plug
printf "\nInstalling vim-plug"

# vim
mkdir -p ~/.vim/autoload
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# neovim
mkdir -p ~/.config/nvim/
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#install zgen
git clone https://github.com/tarjoilija/zgen.git $ZGENDIR

#install tpm
printf "\nInstalling tpm"
git clone https://github.com/tmux-plugins/tpm $TPMDIR

# deploy dotfiles with stow
printf "\nRestow dotfiles"
for d in $(find . -maxdepth 1 -path ./.git -prune -o -type d -printf "%f " | sed 's|\.||g'); do
    stow -R $d
done

rm -fv $HOME/.zcompdump*

printf "Installing vim plugins"
vim +PlugInstall +qall
