#!/usr/bin/env bash

SOURCE=~/OneDrive/dotfiles/git/gitconfig
TARGET=~/.gitconfig

if [[ -f $TARGET && ! -L $TARGET ]] ; then
    mv $TARGET "${TARGET}.bak"
    echo "Backup file done $TARGET -> ${TARGET}.bak"
fi

ln -sf $SOURCE $TARGET
echo "Installation done ($TARGET)"
