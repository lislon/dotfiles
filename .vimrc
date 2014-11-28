source ~/.vim/.vundle_init

" Basic stuff --------------------------- {{{

:set shiftwidth=4
:set tabstop=4
:set expandtab
:set autoindent
:set incsearch 
:set wildmenu
:set wildmode=full
:set gdefault
:set iminsert=0
:set imsearch=0
:set ignorecase
:set nu
" Allow backspace after append
:set backspace=indent,eol,start

:syntax on
filetype plugin indent on
" Keep 3 lines below and above the cursor
:set scrolloff=3

set foldcolumn=3
set autochdir
let mapleader=","
let maplocalleader="\\"
iabbrev lenght length
iabbrev lenth length

" Man page
nnoremap M K
"inoremap <Esc> <nop>
inoremap jk <Esc>

"Uppercase current word
inoremap <C-u> <Esc>mdgUiw`da
" }}} End of basic stuff 

" System stuyff {{{
if has("win32unix")
    :set keymap=russian-jcukenwin
endif
" Tell vim to remember certain things when we exit
" "  '10  :  marks will be remembered for up to 10 previously edited files
" "  "100 :  will save up to 100 lines for each register
" "  :20  :  up to 20 lines of command-line history will be remembered
" "  %    :  saves and restores the buffer list
" "  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo
if has('persistent_undo')
	set undofile
	set undodir=~/.vimtmp/undo
endif
" Move swap files
set backupdir=~/.vimtmp/swp
set directory=~/.vimtmp/swp
:highlight lCursor guifg=NONE guibg=Cyan

" Support color highlight in putty
if has('unix')
	set t_Co=256
endif

if has('unix')
	language messages C
else
	language messages en
endif
" Prevent gvim to resize by itself
" default is egmrLtT
:set guioptions=gmrtT
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar
" }}} End of system stuff

" Quit commands {{{

if &diff
	color skittles_dark
	noremap Q :cquit<CR>
else
	nnoremap <silent>K :q<CR>
    nnoremap Q <nop>
endif

" }}}

" Force to learning new commands {{{

noremap <left> <nop>
noremap <right> <nop>
noremap <up> <nop>
noremap <down> <nop>

" }}}

" Window switching {{{
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" }}}

" Reset cursor position when loading old file {{{
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

" }}}

" General bindigs {{{
"copy
:vnoremap <C-Insert> "+y
"paste (Insert like = p, Shift+Insrt like P)
:nnoremap <Insert> "+p
:inoremap <Insert> <c-o>"+p
:nnoremap <S-Insert> "+P
:inoremap <S-Insert> <c-o>"+P

" Wrap to vim fold
:vnoremap <Leader>f <Esc>'>o" }}}<Esc>'<O"  {{{<Left><Left><Left><Left>

" Very left & Very Right
:noremap H ^
:noremap L $

:noremap <Tab> %

" Delete till end"
:noremap D d$

" Split line
:noremap S i<cr><Esc>

:inoremap <C-a> <Esc>I
:inoremap <C-e> <Esc>A

" Turn on/off highlight search by F3
:nnoremap <F3> :let @/ = ""<CR>
:nnoremap <F12> :tabe $MYVIMRC<CR>
:nnoremap <F11> :tabe ~/.vim/.vundle_init<CR>
":nnoremap <leader>ev :vsplit ~/.vim/.vundle_init<CR>
:nnoremap <F2> :w<CR>
:inoremap <F2> <Esc>:w<CR>

" Alt + 1 - NERD Tree
:nnoremap <A-1> :NERDTreeFocusToggle<CR>
:nnoremap <A-2> :NERDTreeFind<CR>
let g:NERDTreeDirArrows=0

" Ctrl+N switch options !! Confclits with TextMate
":imap <Tab> <C-P>


" Insert new line without insert mode
nnoremap <S-Enter> O<Esc>
nnoremap <C-p> :CtrlPMRU<CR>
" }}}

" Misc stuff {{{1
" Auto update vimrc
augroup auto_reload
	autocmd!
	autocmd BufWritePost ~/.vimrc,~/dotfiles/.vimrc,~/dotfiles/.vim/.vundle_init,.vundle_init source $MYVIMRC
    " Custom extensions sytnax highlighting
    autocmd BufNewFIle,BufRead *.vundle_init set filetype=vim
    autocmd FileType vim setlocal foldmethod=marker
    autocmd FileType vim iabbrev <buffer> "} " }}<C-R>=string(})<CR>
augroup END

" File navigations
set wildmenu
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,
			\*.jpg,*.gif,*.png
set wildmode=list:longest " turn on wild mode huge list

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Disable annoying match braces behavious highlighting
highlight! MatchParen cterm=NONE ctermbg=white ctermfg=white
highlight! link MatchParen StatusLine

:colorscheme solarized

" Deletes
function! DeleteLine()
    :echom "we here 1:".v:count
    if v:count < 1 && getline(line('.')) == ""
        normal! "_dd
    else
        execute "normal! ".v:count."dd"
    endif
endfunction

nnoremap dd :<C-u>call DeleteLine()<Esc>

:let g:session_autoload = 'yes'
:let g:session_autosave = 'yes'
" }}}1

" Automatically detect filetype for new files {{{
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
" }}}

" F5 for running current file {{{
function! RunCmd(cmd)
	let fn=expand("%:p")
    let fns=expand("%")
	let ft = &l:filetype
	botright copen
	setlocal modifiable
	%d _
	if ft == "ruby"
		silent execute "read !ruby ".fn
    elseif ft == "javascript"
		silent execute "read !node ".fns
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
" }}}

" Figutive git bindings {{{
nnoremap <leader>ga :Git add %:p<CR><CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit -v -q<CR>
nnoremap <leader>gt :Gcommit -v -q %:p<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>ge :Gedit<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gw :Gwrite<CR><CR>
nnoremap <leader>gl :silent! Glog<CR>:bot copen<CR>
nnoremap <leader>gp :Ggrep<Space>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gb :Git branch<Space>
nnoremap <leader>go :Git checkout<Space>
nnoremap <leader>gps :Dispatch! git push<CR>
nnoremap <leader>gpl :Dispatch! git pull<CR>
" }}}

" LaTeX settings {{{
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
" }}}

"  Navigate in camelCase words {{{
" Use one of the following to define the camel characters.
" Stop on capital letters.
let g:camelchar = "A-Z"
" Also stop on numbers.
let g:camelchar = "A-Z0-9"
" Include '.' for class member, ',' for separator, ';' end-statement,
" and <[< bracket starts and "'` quotes.
let g:camelchar = "A-Z0-9.,;:{([`'\""
nnoremap <silent><C-Left> :<C-u>call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>
nnoremap <silent><C-Right> :<C-u>call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>
inoremap <silent><C-Left> <C-o>:call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>
inoremap <silent><C-Right> <C-o>:call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>
vnoremap <silent><C-Left> :<C-U>call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>v`>o
vnoremap <silent><C-Right> <Esc>`>:<C-U>call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>v`<o

" }}}

" Autocmnds for FileTypes {{{
augroup mygroup
    autocmd!
    autocmd BufWritePost *.tex silent call Tex_RunLaTeX()
    autocmd BufWritePost *.tex silent !pkill -USR1 xdvi.bin
    autocmd FileType javascript let b:delimitMate_expand_cr = 1
    autocmd FileType jade,css setlocal sw=2

    " Forget about this
    autocmd FileType javascript :iabbrev return NOPENOPENOPE
    autocmd FileType javascript :iabbrev function NOPENOPENOPE
augroup END
" }}} 

" Opeator pending maps {{{
onoremap p i(
" }}}

" Block change next/prev parenthesis {{{

" print foo(bar)
"  ^
" cin( print foo(|)

:onoremap in( :<c-u>normal! f)vi(<cr>
:onoremap il( :<c-u>normal! F)vi(<cr>
" }}}

" redir_messages.vim {{{
"
" Inspired by the TabMessage function/command combo found
" at <http://www.jukie.net/~bart/conf/vimrc>.
"

function! RedirMessages(msgcmd, destcmd)
"
" Captures the output generated by executing a:msgcmd, then places this
" output in the current buffer.
"
" If the a:destcmd parameter is not empty, a:destcmd is executed
" before the output is put into the buffer. This can be used to open a
" new window, new tab, etc., before :put'ing the output into the
" destination buffer.
"
" Examples:
"
"   " Insert the output of :registers into the current buffer.
"   call RedirMessages('registers', '')
"
"   " Output :registers into the buffer of a new window.
"   call RedirMessages('registers', 'new')
"
"   " Output :registers into a new vertically-split window.
"   call RedirMessages('registers', 'vnew')
"
"   " Output :registers to a new tab.
"   call RedirMessages('registers', 'tabnew')
"
" Commands for common cases are defined immediately after the
" function; see below.
"
    " Redirect messages to a variable.
    "
    redir => message

    " Execute the specified Ex command, capturing any messages
    " that it generates into the message variable.
    "
    silent execute a:msgcmd

    " Turn off redirection.
    "
    redir END

    " If a destination-generating command was specified, execute it to
    " open the destination. (This is usually something like :tabnew or
    " :new, but can be any Ex command.)
    "
    " If no command is provided, output will be placed in the current
    " buffer.
    "
    if strlen(a:destcmd) " destcmd is not an empty string
        silent execute a:destcmd
        nnoremap <buffer> K :q!<cr>
    endif

    " Place the messages in the destination buffer.
    "
    silent put=message

endfunction

" Create commands to make RedirMessages() easier to use interactively.
" Here are some examples of their use:
"
"   :BufMessage registers
"   :WinMessage ls
"   :TabMessage echo "Key mappings for Control+A:" | map <C-A>
"
command! -nargs=+ -complete=command BufMessage call RedirMessages(<q-args>, ''       )
command! -nargs=+ -complete=command WinMessage call RedirMessages(<q-args>, 'new'    )
command! -nargs=+ -complete=command TabMessage call RedirMessages(<q-args>, 'tabnew' )

" end redir_messages.vim" }}}

