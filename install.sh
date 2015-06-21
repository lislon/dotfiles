#!/bin/bash
############################
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables
dir=~/dotfiles # dotfiles directory
olddir=~/dotfiles_old # old dotfiles backup directory
# list of files/folders to symlink in homedir
global_files=".bashrc .gitignore_global .gitconfig .minttyrc .githelpers .tmux.conf .Xkbmap"
#####

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
    done
}

install_zsh () {
  # If zsh isn't installed, get the platform of the current machine
  if `which zsh >/dev/null`; then
        install_files ".zshrc .zshenv"
    # Clone my oh-my-zsh repository from GitHub only if it isn't already present
    if [[ ! -d $dir/.oh-my-zsh/ ]]; then
      git clone http://github.com/robbyrussell/oh-my-zsh.git $dir/.oh-my-zsh
    fi
  fi
}

install_vim () {
    if `which vim >/dev/null`; then
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
    fi

}

install_emacs () {
    if `which emacs >/dev/null`; then
        if [[ ! -d ~/.emacs.d/personal ]] ; then
            curl -L https://git.io/epre | sh
        fi
    fi
    ln -s ~/dotfiles/.emacs/prelude-modules.el ~/.emacs.d/prelude-modules.el
    ln -s ~/dotfiles/.emacs/personal/custom.el ~/.emacs.d/personal/custom.el
}

install_files "$global_files"
install_zsh
install_vim
install_emacs
echo "Installation done."
