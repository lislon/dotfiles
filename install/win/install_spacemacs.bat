;Put into C:\Users\ele\AppData\Roaming\.emacs 
;(setq user-emacs-directory "c:/Users/ele/.emacs.d")
;(setenv "HOME" "c:/Users/ele")
; set HOME  env


cd %USERPROFILE%
IF NOT EXIST %USERPROFILE%\.emacs.d git clone https://github.com/syl20bnr/spacemacs.git .emacs.d
cd .emacs.d
git checkout develop
IF NOT EXIST %USERPROFILE%\.spacemacs.d New-Item -Path %USERPROFILE%\.spacemacs.d -ItemType SymbolicLink -Value %USERPROFILE%\Dropbox\dotfiles\spacemacs

