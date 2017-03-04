cd %USERPROFILE%
IF NOT EXISTS %USERPROFILE%\.emacs.d git clone https://github.com/syl20bnr/spacemacs.git .emacs.d
IF NOT EXISTS %USERPROFILE%\.spacemacs.d mklink /D %USERPROFILE%\.spacemacs.d %USERPROFILE%\Dropbox\dotfiles\spacemacs
