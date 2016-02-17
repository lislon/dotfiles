#!/usr/bin/env bash

SRC_FILES=(~/dotfiles/vim/vimrc ~/dotfiles/vim/dotvim/*)

mkdir -p ~/.vim

for src_file in ${SRC_FILES[*]}; do
    if [[ $src_file =~ "dotvim" ]] ; then
        dst_file="$HOME/.vim/`basename $src_file`"
    else
        dst_file="$HOME/.`basename $src_file`"
    fi
    if [[ -f "$TARGET" && ! -L $TARGET ]] ; then
        mv $TARGET "${TARGET}.bak"
        echo "Backup src_file done $TARGET -> ${TARGET}.bak"
    fi
    ln -sf $src_file $dst_file
done

if [[ ! -d ~/.vimtmp/undo ]]; then
    mkdir -p ~/.vimtmp/undo
fi
if [[ ! -d ~/.vimtmp/swp ]]; then
    mkdir -p ~/.vimtmp/swp
fi

echo "Installation done"
