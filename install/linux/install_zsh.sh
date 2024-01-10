#!/usr/bin/env bash

SOURCE=~/OneDrive/dotfiles/zsh/zshrc
TARGET=~/.zshrc

if ! which curl >/dev/null 2>&1; then
    echo "Please install curl to use $0"
    exit 1
fi

install-zsh-autosuggestions
git clone https://github.com/marzocchi/zsh-notify.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/notify


[[ -d ~/.oh-my-zsh ]] || sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

if [[ -f $TARGET && ! -L $TARGET ]] ; then
    mv $TARGET "${TARGET}.bak"
    echo "Backup file done $TARGET -> ${TARGET}.bak"
fi

ln -sf $SOURCE $TARGET
echo "Installation done ($TARGET)"


function install-zsh-autosuggestions() {
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

}

