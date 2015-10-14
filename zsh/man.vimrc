source /etc/vimrc
runtime ftplugin/man.vim

augroup Man
    au!
    " this one is which you're most likely to use?
    autocmd FileType man :nnoremap <buffer> d <C-d>
    autocmd FileType man :nnoremap <buffer> <Space> <C-d>
    autocmd FileType man :nnoremap <buffer> u <C-u>
augroup end

set nocompatible
set nomod nolist nonumber cpoptions-=<
set scrolloff=300 hlsearch ignorecase smartcase
set clipboard=unnamed,unnamedplus nomodifiable
set tabstop=8
" syntax on
nnoremap K :!trans <C-R><C-W><CR>
nnoremap M :Man <C-R>=expand("<cword>")<CR><CR>
" set nomodified
nnoremap q :q!<CR>

set ft=man
