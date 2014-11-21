source ~/.vim/.vundle_init


:set shiftwidth=4
:set tabstop=4
:set expandtab
:set autoindent
:set incsearch 
:set wildmenu
:set wildmode=full
:colorscheme skittles_dark
:set gdefault
:set keymap=russian-jcukenwin
:set iminsert=0
:set imsearch=0
:set ignorecase
:set nu
" Allow backspace after append
:set backspace=indent,eol,start
:highlight lCursor guifg=None guibg=Cyan

if &diff
	color skittles_dark
	map Q :cquit<CR>
else
	nmap <silent>Q :q<CR>
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
:nnoremap <F11> :tabe ~/.vim/.vundle_init<CR>

" Alt + 1 - NERD Tree
:map 1 <Esc>:NERDTree<CR>

" Keep 3 lines below and above the cursor
:set scrolloff=3
" Ctrl+N switch options !! Confclits with TextMate
":imap <Tab> <C-P>

" Support color highlight in putty
if has('unix')
	set t_Co=256
endif

" Insert new line without insert mode
nmap <S-Enter> O<Esc>
" Auto update vimrc
augroup auto_reload
	au!
	autocmd BufWritePost ~/.vimrc,~/dotfiles/.vimrc,~/dotfiles/.vim/.vundle_init source ~/.vimrc
    " Custom extensions sytnax highlighting
    autocmd BufNewFIle,BufRead *.vundle_init set filetype=vim
augroup END

" CamelCaseMovements
nmap [b :call search('\<\<Bar>\u', 'bW')<CR>
nmap [w :call search('\<\<Bar>\u', 'W')<CR>
set autochdir

" Automatically detect filetype for new files
function! CheckFileType()
	if exists('b:countCheck') == 0
		let b:countCheck = 0
	endif
	let b:countCheck += 1 

	if &filetype == "" && b:countCheck > 30
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
	if ft == "ruby"
		silent execute "read !ruby ".fn
	elseif ft != ""
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

"File navigations
set wildmenu
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,
			\*.jpg,*.gif,*.png
set wildmode=list:longest " turn on wild mode huge list

" fugitive git bindings
nnoremap <space>ga :Git add %:p<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gd :Gdiff<CR>
nnoremap <space>ge :Gedit<CR>
nnoremap <space>gr :Gread<CR>
nnoremap <space>gw :Gwrite<CR><CR>
nnoremap <space>gl :silent! Glog<CR>:bot copen<CR>
nnoremap <space>gp :Ggrep<Space>
nnoremap <space>gm :Gmove<Space>
nnoremap <space>gb :Git branch<Space>
nnoremap <space>go :Git checkout<Space>
nnoremap <space>gps :Dispatch! git push<CR>
nnoremap <space>gpl :Dispatch! git pull<CR>

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" unicode symbols

"let g:laststatus=2
"let g:Powerline_dividers_override = ['>>', '>', '<<', '<']
"let g:Powerline_mode_n = 'NORMA'

"let g:phpqa_messdetector_ruleset = '/opt/www/.utils/build/phpmd.xml'
"let g:phpqa_codesniffer_args = "--standard=Sotmarket"
let g:NERDTreeDirArrows=0

set rtp+=~/.local/lib/python2.7/site-packages/powerline/bindings/vim/
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_MultipleCompileFormats = "dvi"
let g:Tex_DefaultTargetFormat = "pdf"
let g:Tex_FormatDependency_ps = "dvi,ps"
let g:Tex_FormatDependency_pdf = "dvi,ps,pdf"



let g:Tex_CompileRule_dvi = 'latex --interaction=nonstopmode $*'
let g:Tex_CompileRule_ps = 'dvips   -o $*.ps $*.dvi'
let g:Tex_CompileRule_pdf = 'ps2pdf $*.ps'

"let g:Tex_ViewRule_pdf = "~/SumatraPDF/SumatraPDF.exe"
let g:Tex_ViewRule_pdf = "xpdf"

let g:Tex_BibtexFlavor = 'bibtex'
let g:Tex_GotoError=0

au BufWritePost *.tex silent call Tex_RunLaTeX()
au BufWritePost *.tex silent !pkill -USR1 xdvi.bin
