cd %USERPROFILE%
IF NOT EXISTS %USERPROFILE%\.emacs.d git clone https://github.com/syl20bnr/spacemacs.git .emacs.d
mklink %USERPROFILE%\.spacemacs %USERPROFILE%\Dropbox\dotfiles\spacemacs\spacemacs.el
