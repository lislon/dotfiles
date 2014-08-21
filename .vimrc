source ~/.vim/.vundle_init
" PHP {{{1
"Bundle 'joonty/vim-phpqa.git'
"let g:phpqa_messdetector_ruleset = '/opt/www/.utils/build/phpmd.xml'
"let g:phpqa_codesniffer_args = "--standard=Sotmarket"
" }}}1

" Nerd Tree {{{2
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
let g:NERDTreeDirArrows=0
" }}}2

:set nu
:set shiftwidth=4
:set tabstop=4
:set autoindent
:set incsearch 
:set wildmenu
:set wildmode=full
:colorscheme skittles_dark
:set gdefault
:set keymap=russian-jcukenwin
:set iminsert=0
:set imsearch=0
:highlight lCursor guifg=None guibg=Cyan

if &diff
	color skittles_dark
	map Q :cquit<CR>
endif

:syntax on
filetype plugin indent on
noremap <left> <nop>
noremap <right> <nop>
noremap <up> <nop>
noremap <down> <nop>

nmap <Space>= yyp<c-v>$r=
nmap <Space>- yyp<c-v>$r-
" Tell vim to remember certain things when we exit
" "  '10  :  marks will be remembered for up to 10 previously edited files
" "  "100 :  will save up to 100 lines for each register
" "  :20  :  up to 20 lines of command-line history will be remembered
" "  %    :  saves and restores the buffer list
" "  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo

function! ResCur()
  if line("'\"") <= line("$")
	  normal! g`"
	  return 1
  endif
endfunction

augroup resCur
	autocmd!
	autocmd BufWinEnter * call ResCur()
augroup END

"copy
:vmap <C-Insert> "+y
"paste (Insert like = p, Shift+Insrt like P)
:nmap <Insert> "+p
:imap <Insert> <c-o>"+p
:nmap <S-Insert> "+P
:imap <S-Insert> <c-o>"+P

" Very left & Very Right
:noremap H ^
:noremap L $

:map <Tab> %

" Delete till end"
:noremap D d$

" Split line
:noremap S i<cr><Esc>

:inoremap <C-a> <Esc>I
:inoremap <C-e> <Esc>A

set foldcolumn=3
set undofile
let mapleader="\\"
set undodir=~/.vimtmp/undo
" Move swap files
set backupdir=~/.vimtmp/swp
set directory=~/.vimtmp/swp

" Turn on/off highlight search by F3
:nnoremap <F3> :let @/ = ""<CR>
:nnoremap <F5> :tabe $MYVIMRC<CR>

" Alt + 1 - NERD Tree
:map 1 <Esc>:NERDTree<CR>

" Keep 3 lines below and above the cursor
:set scrolloff=3
" Ctrl+N switch options
:imap <Tab> <C-P>
" Disable annoying match braces behavious highlighting
highlight MatchParen cterm=NONE ctermbg=black ctermfg=white

" Support color highlight in putty
if has('unix')
	set t_Co=256
endif
