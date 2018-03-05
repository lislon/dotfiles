cd %USERPROFILE%
IF NOT EXIST %USERPROFILE%\.emacs.d git clone https://github.com/syl20bnr/spacemacs.git .emacs.d
cd .emacs.d
git checkout develop
IF NOT EXIST %USERPROFILE%\.spacemacs.d mklink /D %USERPROFILE%\.spacemacs.d %USERPROFILE%\Dropbox\dotfiles\spacemacs
