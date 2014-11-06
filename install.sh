#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################
########## Variables
dir=~/dotfiles # dotfiles directory
olddir=~/dotfiles_old # old dotfiles backup directory
files=".zshrc .vimrc .bashrc .gitignore_global .gitconfig .minttyrc .vim .githelpers .tmux.conf" # list of files/folders to symlink in homedir
##########
# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
echo "done"
# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"
# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
echo "Moving any existing dotfiles from ~ to $olddir"
for file in $files; do
	if [[ -e ~/$file ]] ; then
		rm -rf ~/dotfiles_old/$file
		mv ~/$file ~/dotfiles_old/
	fi
	echo "Creating symlink to $file in home directory."
	ln -s $dir/$file ~/$file
done
install_zsh () {
	# If zsh isn't installed, get the platform of the current machine
	platform=$(uname);
	# Test to see if zshell is installed. If it is:
	if [ -f /bin/zsh -o -f /usr/bin/zsh ]; then
		# Clone my oh-my-zsh repository from GitHub only if it isn't already present
		if [[ ! -d $dir/.oh-my-zsh/ ]]; then
			git clone http://github.com/robbyrussell/oh-my-zsh.git $dir/.oh-my-zsh
		fi
	else
		# If the platform is Linux, try an apt-get to install zsh and then recurse
		if [[ $platform == 'Linux' ]]; then
			sudo apt-get install zsh
			if [ ! "$?" -eq 0 ] ; then
			  echo "Please install zsh, then re-run this script!"
			  exit -1
			fi
			install_zsh
			# If the platform is OS X, tell the user to install zsh :)
		elif [[ $platform == 'Darwin' ]]; then
			echo "Please install zsh, then re-run this script!"
			exit -1
		fi
	fi
}
install_vim () {
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

}

install_zsh
install_vim
echo "Installation done."
