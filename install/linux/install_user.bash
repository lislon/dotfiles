#!/bin/bash

set -x

install_apt_get() {
	  apt-get update
	  apt-get install git zsh tmux wine
	  dpkg --add-architecture i386 && sudo apt-get update && sudo apt-get-install wine32
}

change_default_shell() {
	  # Make bash default shell
	  sed -i "s@SHELL=/bin/sh@SHELL=/bin/bash@"  /etc/default/useradd
}

timezone() {
	  ln -fs /usr/share/zoneinfo/Europe/Moscow /etc/localtime1
}

ele_acc() {
    useadd -m ele
    mkdir ~ele/.ssh
    chmod 600 ~/ele/.ssh
    chsh -s /bin/zsh ele
}

install_apt_get
change_default_shell
timezone
ele_acc
