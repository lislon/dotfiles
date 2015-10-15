#!/usr/bin/env bash

function install_spacemacs {
    git clone https://github.com/syl20bnr/spacemacs.git ~/.emacs.d || exit -1
}

function is_spacemacs_intalled {
    cd ~/.emacs.d && git remote -v | grep spacemacs > /dev/null
}

# install spacemacs if needed
[[ is_spacemacs_intalled ]] || install_spacemacs

# backup old spacemacs if it's regular file
[[ -f ~/.spacemacs ]] && mv ~/.spacemacs{,.bak}
[[ -d ~/.emacs.d/private ]] && mv ~/.emacs.d/private{,.bak}

ln -sf ~/dotfiles/spacemacs/spacemacs.el ~/.spacemacs
ln -sf ~/dotfiles/spacemacs/private ~/.emacs.d/

echo "done"
