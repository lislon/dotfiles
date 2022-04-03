#!/usr/bin/env bash

SOURCE=~/Dropbox/dotfiles/tmux/tmux.conf
TARGET=~/.tmux.conf

if [[ -f $TARGET && ! -L $TARGET ]] ; then
    mv $TARGET "${TARGET}.bak"
    echo "Backup file done $TARGET -> ${TARGET}.bak"
fi

ln -sf $SOURCE $TARGET
echo "Installation done ($TARGET)"
