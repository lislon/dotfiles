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

GOTO:EOF



call:install_vim