#!/usr/bin/env bash

function install_spacemacs {
    [[ -d ~/.emacs.d ]] && mv ~/.emacs.d{,.bak}
    git clone https://github.com/syl20bnr/spacemacs.git ~/.emacs.d || exit -1
}

function is_spacemacs_intalled {
    cd ~/.emacs.d && git remote -v | grep spacemacs > /dev/null 2>/dev/null
}

# install spacemacs if needed
is_spacemacs_intalled || install_spacemacs

ln -sf ~/dotfiles/spacemacs ~/.spacemacs.d

echo "done"
