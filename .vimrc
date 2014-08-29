
source ~/.vim/.vundle_init


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

" Window switching
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

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
let mapleader="\\"
if has('persistent_undo')
	set undofile
	set undodir=~/.vimtmp/undo
endif
" Move swap files
set backupdir=~/.vimtmp/swp
set directory=~/.vimtmp/swp

" Turn on/off highlight search by F3
:nnoremap <F3> :let @/ = ""<CR>
:nnoremap <F12> :tabe $MYVIMRC<CR>

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

" Insert new line without insert mode
nmap <S-Enter> O<Esc>
" Auto update vimrc
augroup auto_reload
	au!
	autocmd BufWritePost ~/.vimrc,~/dotfiles/.vimrc source ~/.vimrc
augroup END

" CamelCaseMovements
nmap [b :call search('\<\<Bar>\u', 'bW')<CR>
nmap [w :call search('\<\<Bar>\u', 'W')<CR>
set autochdir

function! CheckFileType()
	if exists('b:countCheck') == 0
		let b:countCheck = 0
	endif
	let b:countCheck += 1 

	if &filetype == "" && b:countCheck > 20
		filetype detect
	elseif b:countCheck > 200 || &filetype != ""
		autocmd! newFileDetection
	endif
endfunction

augroup newFileDetection
	autocmd CursorMovedI * call CheckFileType()
augroup END

function! RunCmd(cmd)
	let fn=expand("%:p")
	let ft = &l:filetype
	botright copen
	setlocal modifiable
	%d _
	if a:cmd == "" && ft != ""
		silent execute "read !".fn
	else
		silent execute "read !".a:cmd." ".fn
	endif
	1d _
	normal! 0
	if ft != ""
		execute "setf ".ft
	else
		setlocal filetype=
	endif
	setlocal nomodifiable nomodified
endfunction

"command! RunBash call RunCmd("") 
nnoremap <F5> :<C-u>up\|call RunCmd("")<CR>
inoremap <F5> <Esc>:up\|call RunCmd("")<CR>
