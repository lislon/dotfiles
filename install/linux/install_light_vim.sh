#!/usr/bin/env bash

SOURCE=~/dotfiles/vim/light_vimrc
TARGET=~/.vimrc

if [[ -f $TARGET && ! -L $TARGET ]] ; then
    mv $TARGET "${TARGET}.bak"
    echo "Backup file done $TARGET -> ${TARGET}.bak"
fi

ln -sf $SOURCE $TARGET
echo "Installation done ($TARGET)"
