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

 - Follow instructions https://github.com/JazzCore/ctrlp-cmatcher/
 - For ack https://github.com/chocolatey/chocolatey/wiki/Installation 
   Then ``choco install ag``

Cheatsheet for new stuff
-----------------------

 - ``]m [m``  - Move to next JS NOPENOPENOPE/block {}
 - <leader>lf Lusty File explorer
 - <leader>lb Lusty Buffer
 - <leader>lg Lusty bufferGrep
 - <leader>lm MRU
 - <leader>lt Commant-T
 - <leader>a  Ack
