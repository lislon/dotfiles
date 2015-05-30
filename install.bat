@echo off

REM mkdir -p by defaults
setlocal enableextensions

set "vimfiles=%HOMEPATH%\vimfiles"


IF NOT EXIST "%vimfiles%" (
  mklink /J %vimfiles% %HOMEPATH%\dotfiles\.vim
)

IF NOT EXIST "%HOMEPATH%\.vim" (
  mklink /J %HOMEPATH%\.vim %HOMEPATH%\dotfiles\.vim
)

mklink /H %HOMEPATH%\.gitconfig %HOMEPATH%\dotfiles\.gitconfig


IF EXIST "%HOMEPATH%\.emacs.d" (
mklink /H %HOMEPATH%\.emacs.d\personal\my.el %HOMEPATH%\dotfiles\.emacs\personal\my.el 
mklink /H %HOMEPATH%\.emacs.d\prelude-modules.el %HOMEPATH%\dotfiles\.emacs\prelude-modules.el
)


GOTO:Run

------------------------------
:install_vim

set "bundledir=%vimfiles%\bundle"

IF NOT EXIST %HOMEPATH%\.vimtmp\undo (
  md %HOMEPATH%\.vimtmp\undo
)

IF NOT EXIST %HOMEPATH%\.vimtmp\swp (
  md %HOMEPATH%\.vimtmp\swp
)

IF NOT EXIST %bundledir%\Vundle.vim (
  git clone https://github.com/gmarik/Vundle.vim.git %bundledir%\Vundle.vim
)

vim +:PluginInstall

GOTO:EOF

REM ------------------------------
:install_cmathcer

cd %HOMEPATH%\dotfiles\.vim\bundle\ctrlp-cmatcher\
install_windows.bat
GOTO:EOF

REM Powerline fonts
REM git clone https://github.com/runsisi/consolas-font-for-powerline 

:Run

call:install_vim
call:install_cmathcer
