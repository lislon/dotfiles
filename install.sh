#!/bin/bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables
dir=~/dotfiles # dotfiles directory
olddir=~/dotfiles_old # old dotfiles backup directory
# list of files/folders to symlink in homedir
global_files=".bashrc .minttyrc .tmux.conf .Xkbmap .gitignore_global .gitconfig .githelpers "

usage()
{
    cat << EOF
        usage: $0 options

        Install dotfiles

        OPTIONS:
        -h      Show this message
        -b      Basic files git/bashrc/zshrc/tmux
        -v      Vim files
        -e      Emacs files
EOF
}

symlink_with_backup() {
    dir=$1
    file=$2
    if [[ -h ~/$file ]]; then
        echo "File ~/$file already installed"
    else
        if [[ -e ~/$file ]] ; then
            rm -rf ~/dotfiles_old/$file
            mv ~/$file ~/dotfiles_old/
        fi
        echo "Creating symlink to ~/$file in home directory."
        ln -s $dir/$file ~/$file
    fi
}

install_files () {
    files=$1
    # create dotfiles_old in homedir
    mkdir -p $olddir
    # change to the dotfiles directory
    cd $dir
    # move any existing dotfiles in homedir to dotfiles_old directory,
    # then create symlinks from the homedir to any files in the ~/dotfiles directory
    # specified in $files
    for file in $files; do
        symlink_with_backup $dir $file
    done

    # Git 1.x.x complain on 'default = simple'
    if [[ `git --version` =~ (1\.[0-9]+\.[0-9]+) && ! -f $dir/.git/info/exclude ]]; then
        echo "Warning! You are using old version if git, .gitconfig file will be modified and ignored"
        sed -i '/default = simple/d' ~/.gitconfig
        echo '.gitconfig' > $dir/.git/info/exclude
    fi
}

install_zsh () {
  # If zsh isn't installed, get the platform of the current machine
  if `which zsh >/dev/null 2>&1`; then
        install_files ".zshrc .zshenv"
    # Clone my oh-my-zsh repository from GitHub only if it isn't already present
    if [[ ! -d $dir/.oh-my-zsh/ ]]; then
      git clone http://github.com/robbyrussell/oh-my-zsh.git $dir/.oh-my-zsh
    fi
  fi
}

install_vim () {
    if `which vim >/dev/null 2>&1`; then
        install_files ".vim .vimrc"
        bundledir=$dir/.vim/bundle

        if [[ ! -d ~/.vimtmp/undo ]]; then
            mkdir -p ~/.vimtmp/undo
        fi
        if [[ ! -d ~/.vimtmp/swp ]]; then
            mkdir -p ~/.vimtmp/swp
        fi

        if [[ ! -d $bundledir/Vundle.vim ]]; then
            git clone https://github.com/gmarik/Vundle.vim.git $bundledir/Vundle.vim
        fi
        if [[ ! -d $bundledir/nerdtree ]]; then
            git clone https://github.com/scrooloose/nerdtree.git $bundledir/nerdtree
        fi
        if [[ ! -d $bundledir/nerdcommenter ]]; then
            git clone https://github.com/scrooloose/nerdcommenter.git $bundledir/nerdcommenter
        fi
        echo "Plugin installation..."
        vim +PluginInstall +qall
        echo "Make vimproc..."
        cd ~/dotfiles/.vim/bundle/vimproc.vim/autoload/
        make >/dev/null
    fi

}

install_emacs () {
    if `which emacs >/dev/null 2>&1`; then
        if [[ ! -d ~/.emacs.d/personal ]] ; then
            curl -L https://git.io/epre | sh
        fi
    fi
    ln -s ~/dotfiles/.emacs/prelude-modules.el ~/.emacs.d/prelude-modules.el
    ln -s ~/dotfiles/.emacs/personal/custom.el ~/.emacs.d/personal/custom.el
}

########################################
# SCRIPT START
########################################

set -e

INSTALL_BASIC=
INSTALL_VIM=
INSTALL_EMACS=

# you put the “:” right after the argument when one's need value
while getopts “hbve” OPTION
do
    case $OPTION in
        h)
            usage
            exit 1
            ;;
        b)
            INSTALL_BASIC=1
            ;;
        v)
            INSTALL_VIM=1
            ;;
        e)
            INSTALL_EMACS=1
            ;;
        ?)
            usage
            exit
            ;;
    esac
done

if [[ -z $INSTALL_BASIC ]] && [[ -z $INSTALL_VIM ]] && [[ -z $INSTALL_EMACS ]] ; then
    usage
    exit 1
fi

if [[ $# == 0 ]] ; then
    help
fi

if [[ -n $INSTALL_BASIC ]] ; then
    install_files "$global_files"
    install_zsh
fi
if [[ -n $INSTALL_VIM ]]; then
    install_vim
fi
if [[ -n $INSTALL_EMACS ]]; then
    install_emacs
fi

echo "Installation done."
