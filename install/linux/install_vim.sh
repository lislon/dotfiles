#!/usr/bin/env bash

SRC_FILES=(~/dotfiles/vim/vimrc ~/dotfiles/vim/dotvim/*)

for src_file in ${SRC_FILES[*]}; do
    if [[ $src_file =~ "dotvim" ]] ; then
        dst_file="~/.vim/`basename $src_file`"
    else
        dst_file="~/.`basename $src_file`"
    fi
    if [[ -f "$TARGET" && ! -L $TARGET ]] ; then
        mv $TARGET "${TARGET}.bak"
        echo "Backup src_file done $TARGET -> ${TARGET}.bak"
    fi
done

echo "Installation done"
