#!/usr/bin/env bash

SOURCE=~/dotfiles/zsh/zshrc
TARGET=~/.zshrc

[[ -d ~/.oh-my-zsh ]] || sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)" || exit -1

if [[ -f $TARGET && ! -L $TARGET ]] ; then
    mv $TARGET "${TARGET}.bak"
    echo "Backup file done $TARGET -> ${TARGET}.bak"
fi

ln -sf $SOURCE $TARGET
echo "Installation done ($TARGET)"
