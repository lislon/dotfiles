Installation
------------

``` bash
cd ~
git clone https://github.com/lislon/dotfiles.git
~/dotfiles/install.sh
```

Windows + Cygwin +ideavim
-------------------------

Prerequites: 

 - Python 2.7 https://www.python.org/downloads/
 - mingw


`mklink /H <path_to_git_version_of_ideavimrc> <path c:/Users/Elephant/.ideavimrc>`

Additional plugin installation
------------------------------

 - For ack https://github.com/chocolatey/chocolatey/wiki/Installation 
   Then ``choco install ag``
 - For Jira
       pip install requests


Command-t installation
----------------------

  cd ~/.vim/bundle/command-t/ruby/command-t
  ruby extconf.rb
  make

Nodejs related installation
---------------------------

      npm -g i node-vim-inspector

      Syntastic jsl - require compilation
          * The Linux download is here: http://www.javascriptlint.com/download.htm
          * After decompressing it you’ll notice there is only a single horribly wrong README.html file. To get things working compile it by going to the src directory and typing:
              make -f Makefile.ref
          A jsl will now be found in jsl-0.3.0/src/Linux_All_DBG.OBJ/jsl. To make it generally accessible do something like:
          * ln -s /whatever/jsl-0.3.0/src/Linux_All_DBG.OBJ /my/bin/jsl 

Cheatsheet for new stuff
-----------------------

 - ``]m [m``  - Move to next JS NOPENOPENOPE/block {}
 - <leader>lf Lusty File explorer
 - <leader>lb Lusty Buffer
 - <leader>lg Lusty bufferGrep
 - <leader>lm MRU
 - <leader>lt Commant-T
 - <leader>a  Ack

 Visual mode:
 - oO - move start/end selection
 - aB - select outer {
 - X - delete full lines

Todo
----

 - Search in tree
 - Highlight current file in NERDTree
 - In insert mode trim string if it's one-liner
 - In insert mode if line is empty, replace current line with insert

