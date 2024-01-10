;Put into C:\Users\ele\AppData\Roaming\.emacs 
;(setq user-emacs-directory "c:/Users/ele/.emacs.d")
;(setenv "HOME" "c:/Users/ele")
; set HOME  env


cd %USERPROFILE%
IF NOT EXIST $HOME\.emacs.d git clone https://github.com/syl20bnr/spacemacs.git .emacs.d
cd .emacs.d
git checkout develop
IF NOT EXIST $HOME\.spacemacs.d New-Item -Path $HOME\.spacemacs.d -ItemType SymbolicLink -Value $HOME\OneDrive\dotfiles\spacemacs

