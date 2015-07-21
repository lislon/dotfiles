source ~/.vim/.vundle_init
" Basic stuff {{{

set shiftwidth=4
set tabstop=4
set expandtab
set autoindent
set incsearch
"set hlsearch
set wildmode=full
set noshowmode
set gdefault
set ignorecase
" For /seARCH
set smartcase
set nu
set laststatus=2 " Vim airline even when 1 file opened
set encoding=utf-8
set infercase
set autoread
set autowrite
set splitright
set splitbelow
set ff=unix
set hidden
if has('gui_running')
    set spell spelllang=ru_ru,en_us
endif
set dictionary+=/usr/share/dict/words
set clipboard=unnamed,unnamedplus " Use + and * registers when deleting
" > flag for inserting new line when appening in register ("Ayy)
set cpoptions=ABceFs>
set isfname=@,48-57,/,.,-,_,+,$,%,~
" Turn off sound
"set vb
"set t_vb=


" Allow backspace after append
set backspace=indent,eol,start
" Prevent open same files twice
set switchbuf=useopen

:syntax on
filetype plugin indent on
" Keep 10 lines below and above the cursor
set scrolloff=10

" Lower the delay of escaping out of other modes
" set timeout timeoutlen=1000 ttimeoutlen=1
set timeout timeoutlen=500 ttimeoutlen=1

set foldcolumn=0
"set autochdir
let mapleader=","
let maplocalleader="\\"
iabbrev lenght length
iabbrev lenth length
set isfname-=, " Allow to gf work in rtp

" Man page
nnoremap M K
nnoremap K <nop>
"inoremap <Esc> <nop>
inoremap kj <Esc>
inoremap jk <Esc>
" Emacs style
inoremap fd <Esc>

"Uppercase current word
inoremap <C-u> <Esc>mdgUiw`da

" Script names
nnoremap <leader>wsn :WinMessage scriptnames<CR>

" rtp
nnoremap <leader>wrt :WinMessage set rtp?<CR>

" Linux buffers
"if has('unix')
    "noremap  Y "+Y
    "noremap  p "+p
    "noremap  P "+P
    "vnoremap y "+y
    "vnoremap Y "+Y
    "vnoremap p "+p
    "vnoremap P "+P
"endif

" Suck-in line below to newborn if body
" if () {
"   |
" }
" line goes to if body
inoremap <A-u> <Esc>ddjddkP>>
nnoremap <A-u> jjddkP>>

nnoremap <S-F2> :w !sudo tee %

" Unfuck my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

" }}} End of basic stuff

" System stuff {{{
if has("win32unix")
    " This stuff is reset iminsert mode
    set keymap=russian-jcukenwin
endif

" Dont use mapping when going to insert mode
set iminsert=0
set imsearch=0

let g:solarized_termcolors=256

if filereadable('../.vimrc_local')
    source ../.vimrc_local
end

if has('gui_running') || has('unix')
    set background=dark
    if stridx(&rtp, 'solarized') != -1
        :colorscheme Tomorrow-Night-Bright
    endif
else
    set background=dark
    :colorscheme skittles_dark
endif

if has ('win32')
    set guifont=Powerline_Consolas:h11:cRUSSIAN
elseif has('gui_running')
    set guifont=Liberation\ Mono\ for\ Powerline\ 11
else
    set guifont=Powerline\ Consolas\ 10
endif
" Tell vim to remember certain things when we exit
" "  '10  :  marks will be remembered for up to 10 previously edited files
" "  "100 :  will save up to 100 lines for each register
" "  :100  :  up to 100 lines of command-line history will be remembered
" "  %    :  saves and restores the buffer list
" "  n... :  where to save the viminfo files
set viminfo='10,\"100,:100,%,n~/.viminfo
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
set guioptions=gmrtT
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar
" }}} End of system stuff

" Quit commands {{{
fun! QuitPrompt()
   " Close nerd tree
   let nr = s:GetNerdTreeWinNr()
   if nr != -1 && winnr('$') == 2
       :exe nr . 'wincmd w'
       :q
   endif

   if has("gui_running") && tabpagenr("$") == 1 && winnr("$") == 1
      let choice = confirm("Close?", "&yes\n&no", 1)
      if choice == 1 | q | endif
   else | q | endif
endfun

fun! s:GetNerdTreeWinNr()
    let i = 0
    while i < winnr('$')
        let ft = getwinvar(i, '&ft')
        if ft == 'nerdtree'
            return i
        endif
        let i += 1
    endwhile
    return -1
endfun

if &diff
    color skittles_dark
    noremap Q :cquit<CR>
else
    nnoremap <silent><c-K> :call QuitPrompt()<CR>
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
"map <C-h> <C-w>h
"map <C-j> <C-w>j
"map <C-k> <C-w>k
"map <C-l> <C-w>l
" }}}

" Reset cursor position when loading old file {{{
function! ResCur()
  if line("'\"") <= line("$")
      silent! normal! g`"
      return 1
  endif
endfunction

if has("folding")
    function! UnfoldCur()
        if !&foldenable
            return
        endif
        let cl = line(".")
        if cl <= 1
            return
        endif
        let cf  = foldlevel(cl)
        let uf  = foldlevel(cl - 1)
        let min = (cf > uf ? uf : cf)
        if min
            execute "normal!" min . "zo"
            return 1
        endif
    endfunction
endif

augroup resCur
    autocmd!
    if has("folding")
        autocmd BufWinEnter * if ResCur() | call UnfoldCur() | endif
    else
        autocmd BufWinEnter * call ResCur()
    endif
augroup END

" }}}

" General bindigs {{{

" Built-in and custom snippets
nnoremap <localleader>s :exe "tab sview ~/.vim/bundle/vim-snippets/UltiSnips/" . &ft . ".snippets"<CR>
nnoremap <localleader>S :exe "tabe ~/.vim/UltiSnips/" . &ft . ".snippets"<CR>

" <Leader>``: Force quit all
nnoremap <Leader>`` :qa!<cr>

" :PI For plugin installation
command! PI :update | PluginInstall
"copy
":vnoremap <C-Insert> "+y
""paste (Insert like = p, Shift+Insrt like P)

" nomap Insert
:nnoremap <Insert> "+p
:cnoremap <S-Insert> <c-r>+
:inoremap <Insert> <c-o>"+]P<Esc>==
:inoremap <S-Insert> <c-o>"+]P<Esc>==
":nmap <S-Insert> yo"+P
":inoremap <S-Insert> <c-o>"+P


 "Wrap to vim fold
:vnoremap <Leader>f <Esc>'>o" }}}<Esc>'<O" {{{ <Right>

" Very left & Very Right
:noremap H ^
:noremap L $

" Ctrl+I not works with that :(
":noremap <Tab> %

" Delete till end"
:noremap D d$

" Split line
:noremap S i<cr><Esc>

:inoremap <C-a> <Esc>I
:inoremap <C-e> <Esc>A

:nnoremap <F1> :help
:nnoremap <F12> :tabe ~/dotfiles/.vimrc<CR>
:nnoremap <F11> :tabe ~/.vim/.vundle_init<CR>
" Irritations
:nnoremap <F10> :vs ~/dotfiles/README.md<CR>G
":nnoremap <leader>ev :vsplit ~/.vim/.vundle_init<CR>
:nnoremap <silent> <F2> :w<CR>
:inoremap <silent> <F2> <Esc>:w<CR>
:vnoremap <silent> <F2> <C-C>:w<CR>
:cnoremap <silent> <F2> <C-C>:w<CR>
:nnoremap <F6> :set paste!<CR>


" Ctrl+N switch options !! Confclits with TextMate
":imap <Tab> <C-P>

" Insert new line without insert mode
nnoremap <Enter> o<Esc>
" Remove line above
nnoremap <C-S-Enter> mZkdd`Z

" Allow regex without escape
nnoremap / /\v
nnoremap ? ?\v

nnoremap <silent> <leader><space> :let @/ = ''<cr>

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" j = Move by screenline, gj move by real line
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" <Alt_D> Digraph insertion :h digraph
inoremap <a-d> <c-k>

"Space to toggle folds
nnoremap <space> zazz
vnoremap <space> zazz

inoremap <c-j> <c-o>j
inoremap <c-k> <c-o>k

" <Leader> + increase window width by 1/3
nnoremap <silent><leader>+ :execute "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <silent><leader>= :execute "vertical resize " . (winwidth(0) * 3/2)<CR>
nnoremap <silent><leader>- :execute "vertical resize " . (winwidth(0) * 2/3)<CR>

" CTRL-Tab is Next window
noremap <C-Tab> <C-W>w
inoremap <C-Tab> <C-O><C-W>w
cnoremap <C-Tab> <C-C><C-W>w
onoremap <C-Tab> <C-C><C-W>w

"noremap 1 gT
"noremap 2 gt
inoremap <C-Tab> <C-O><C-W>w
cnoremap <C-Tab> <C-C><C-W>w
onoremap <C-Tab> <C-C><C-W>w


"nnoremap <c-k> :wq<CR>

" Select all
nnoremap vaa ggVGg_

" relative number lines
nnoremap <leader>N :set relativenumber!<CR>

" Toggle line numbers
nnoremap <leader>n :setlocal number!<cr>

" Bind this shit to something else to prevent conflict with NerdCommneter
map <localLeader>rd <Plug>RooterChangeToRootDirectory
"map <leader>cs <plug>NERDCommenterSexy

" Remove trailing spaces
nnoremap <silent> <leader>ts :let _oldts = @/<CR>:%s/\v\s+$//<CR>:let @/=_oldts<CR>

" Auto reindent when paste
"nnoremap p ]p
"nnoremap P ]P

nmap <C-Enter> o<Esc>

" Search for term in cwd
nnoremap <leader>a :tab split<CR>:Ack ""<Left>

" Search word under cursor in cwd
nnoremap <leader>A :tab split<CR>:Ack <C-r><C-w><CR>

" Copy line from above word-by-word
inoremap <c-^> @<Esc>kyWjPA<BS>

" Ctrl+g copy path to clipboard
nnoremap <c-g> :echo "Path '".expand('%:p')."' copied to clipboard" \| :call EasyClip#Yank(expand('%:p'))<CR>

" <Leader>cd: Switch to the directory of the open buffer
nnoremap <Leader>cd :cd %:p:h<cr>:pwd<cr>

" Panic Button
"nnoremap <f9> mzggg?G`z


" Ctrl-[hl]: Move left/right by word
cnoremap <c-h> <s-left>
cnoremap <c-l> <s-right>

" Ctrl-Space: Show history
cnoremap <c-@> <c-f>
cnoremap <c-j> <down>
cnoremap <c-k> <up>

" Ctrl-v: Paste
cnoremap <c-v> <c-r>"

" _ : Quick horizontal splits
nnoremap _ :sp<cr>

" | : Quick vertical splits
nnoremap <bar> :vsp<cr>

" Jump to function end
nnoremap ]f ]}k
nnoremap [f [{j
vnoremap ]f ]}k
vnoremap [f [{j

" }}}

" Misc stuff {{{1

" {{{ Extra whitespace
" listchar=trail is not as flexible, use the below to highlight trailing
" whitespace. Don't do it for unite windows or readonly files
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
    augroup MyAutoCmd
    autocmd BufWinEnter * if &modifiable && &ft!='unite' | match ExtraWhitespace /\s\+$/ | endif
    autocmd InsertEnter * if &modifiable && &ft!='unite' | match ExtraWhitespace /\s\+\%#\@<!$/ | endif
    autocmd InsertLeave * if &modifiable && &ft!='unite' | match ExtraWhitespace /\s\+$/ | endif
    autocmd BufWinLeave * if &modifiable && &ft!='unite' | call clearmatches() | endif
augroup END
" }}}
" Indent block {{{
function! SelectIndent()
  let cur_line = line(".")
  let cur_ind = indent(cur_line)
  let line = cur_line
  while indent(line - 1) >= cur_ind
    let line = line - 1
  endw
  exe "normal " . line . "G"
  exe "normal V"
  let line = cur_line
  while indent(line + 1) >= cur_ind
    let line = line + 1
  endw
  exe "normal " . line . "G"
endfunction

nnoremap vip :call SelectIndent()<CR>


" }}}

fun! English(num)
    " English
    above sview ~/Documents/english.txt
    set scrollbind
    execute "/exercise " . a:num
    execute "/\\v^1\\\."
    execute "/\\v^1\\\."
    " Russian
    above sview ~/Documents/english.txt
    execute "/exercise " . a:num
    execute "/\\v^1\\\."

    " Work
    above execute "split ~/Documents/exercice_".a:num.".txt"
    set noscrollbind
    resize 3
    wincmd k
    resize 3
    wincmd k
    resize 3

endf


" Auto update vimrc
augroup auto_reload
    autocmd!
    autocmd BufWritePost ~/.vimrc,~/dotfiles/.vimrc,~/dotfiles/.vim/.vundle_init,.vundle_init source $MYVIMRC
    " Custom extensions sytnax highlighting
    autocmd BufNewFIle,BufRead *.vundle_init set filetype=vim
augroup END

" File navigations
set wildmenu
set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,
            \*.jpg,*.gif,*.png
set wildmode=list:longest " turn on wild mode huge list

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Disable annoying match braces behavious highlighting
highlight! MatchParen cterm=NONE ctermbg=gray ctermfg=white
" I commented this out, because highlighting of both () is very same and it's
" hard to distinguish between them. (Tested in Unix, Ubuntu, CLI)
"highlight! link MatchParen StatusLine


" Delete line but not copy blank {{{
"function! DeleteLine()
    "if v:count < 1 && match(getline(line('.')), '^\s*$') >= 0
        "normal! "_dd
    "else
        "execute "normal! ".v:count."dd"
    "endif
"endfunction

"nnoremap <silent>dd :<C-u>call DeleteLine()<Esc>
:
" Prevent corrupting of delete buffer by using single deletion
nnoremap x "_x
" }}}


" Highlight cursor only in current window, but not in insert mode
augroup CursorLine
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter,InsertLeave * setlocal cursorline
augroup end CursorLine

" Highlight traling spaces
highlight TrailingSpace guibg=Green
" \v = very magic, \a = any alpha-character, @<= = look behind
nnoremap <leader>w :match TrailingSpace /\v\a@<=\s+$/<CR>
" Clear match
nnoremap <leader>W :match clear<CR>
" List navigation {{{

nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz
" }}}
"nnoremap <leader>g mC:silent execute "grep -r ".shellescape(expand("<cWORD>"))." ."<cr>:copen 20<cr>


command! ErrorsToggle call ErrorsToggle()
function! ErrorsToggle() " {{{
  if exists("w:is_error_window")
    unlet w:is_error_window
    exec "close"
  else
    exec "Errors"
    lopen
    let w:is_error_window = 1
  endif
endfunction " }}}

command! -bang -nargs=? QFixToggle call QFixToggle(<bang>0)

function! QFixToggle(forced) " {{{
  if exists("g:qfix_win") && a:forced == 0
    cclose
    unlet g:qfix_win
  else
    copen 10
    let g:qfix_win = bufnr("$")
  endif
endfunction " }}}

fun! FoldColumnToggle()
    if &foldcolumn
        setlocal foldcolumn=0
    else
        setlocal foldcolumn=3
    endif
endf

nmap <silent> <f3> :ErrorsToggle<cr>
nmap <silent> <f4> :QFixToggle<cr>
nnoremap <silent> <leader>f :call FoldColumnToggle()<cr>

" Status line {{{

"hi User1 guifg=#eea040 guibg=#222222
"hi User2 guifg=#dd3333 guibg=#222222
"hi User3 guifg=#ff66ff guibg=#222222
"hi User4 guifg=#a0ee40 guibg=#222222
"hi User5 guifg=#eeee40 guibg=#222222

"set statusline=
"set statusline +=%1*\ %n\ %*            "buffer number
"set statusline +=%5*%{&ff}%*            "file format
"set statusline +=%3*%y%*                "file type
"set statusline +=%4*\ %<%F%*            "full path
"set statusline +=%2*%m%*                "modified flag
"set statusline +=%1*%=%5l%*             "current line
"set statusline +=%2*/%L%*               "total lines
"set statusline +=%1*%4v\ %*             "virtual column number
"set statusline +=%2*0x%04B\ %*          "character under cursor

" }}}

" Replace word under cursor
nnoremap gr yiw:.,$s/<C-r>"//Ic<left><Left><Left><Backspace>/

" For global replace
nnoremap gR gD:%s/<C-R>///Ic<left><left><left><left>

" Any selection
vnoremap gr :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gv"ky:%s/<C-R>k//<left><left>

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

"let g:last_f5_run_cmd = ''

" F5 for running current file {{{
function! RunCmd(cmd, bufCommand)
    let chunks = split(a:cmd, " ")
    " Expand %
    let chunks[-1] = expand(chunks[-1])
    let cmd = join(chunks, ' ')

    let ft = &l:filetype
    botright copen
    setlocal modifiable
    %d _
    silent execute "read !".cmd
    1d _
    normal! 0
    " Replace ^M
    silent! execute "%s/\r//e"
    if a:bufCommand != ""
        silent! execute a:bufCommand
    endif
    "if ft != ""
        "execute "setf ".ft
    "else
        "setlocal filetype=
    "endif
    setlocal nomodifiable nomodified
endfunction

fun! BindRunCommand(key, command, bufCallback)
    let bufCmd = substitute(a:bufCallback, '|', '\\|', 'g')
    let cmd = "nnoremap <buffer> <silent> <".a:key."> :<C-u>up\\|call ".
        \ "RunCmd(\"".a:command."\", '".bufCmd."')<CR>"
    silent execute cmd
endf

fun! RunMake()
    if &makeprg != 'make'
        silent make
        "copen
    endif
endf

"command! RunBash call RunCmd("")
nnoremap <silent><F5> :call RunMake()<CR>
nnoremap <silent><S-F5> :call RunMake()<CR>
" }}}

" Figutive git bindings {{{
nnoremap <leader>gas :Git add . \| Gstatus<CR><CR>
nnoremap <leader>gs :update \| Gstatus<CR>
nnoremap <leader>gc :Gcommit -v -q<CR>
nnoremap <leader>gt :Gcommit -v -q %:p<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>ge :Gedit<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gw :Gwrite<CR><CR>
nnoremap <leader>gl :silent! Glog<CR>:bot copen<CR>
nnoremap <leader>gp :Ggrep<Space>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gb :Gblame<Space>
nnoremap <leader>go :Git checkout<Space>
nnoremap <leader>gps :Dispatch! git pull && git push<CR>
nnoremap <leader>gpl :Dispatch! git pull<CR>
nnoremap <leader>grh :!git reset --hard FETCH_HEAD
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
    autocmd FileType jade,css setlocal sw=2

    " Forget about this
    autocmd FileType javascript :iabbrev re return
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
        nnoremap <buffer> <c-K> :q!<cr>
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


fun! s:ShowArgs(line1, line2, args)
    echo "lines ".a:line1." - ".a:line2
    echo "args:".a:args
endf

command! -range=% -bar -nargs=1 Test2 call s:ShowArgs(<line1>, <line2>, <q-args>)



" CoffeeScript {{{

fun! CoffeeRange() range
    echo a:firstline
    echo a:lastline
endf
vnoremap <buffer> <localleader>c :call CoffeeRange()<CR>


fun! InitFtCoffee()
    nnoremap <buffer> <localleader>c :CoffeeWatch vert
    vnoremap <buffer> <localleader>c :CoffeeRange
    " Remove parenthesis
    nnoremap <buffer> <localleader>r :s/(/ /<CR>:s/);//<CR>

    if match(expand("%:p"), "[\\/]test[\\/]") >= 0
        call BindRunCommand("F5",
                    \ "ENV=testing mocha --compilers coffee:coffee-script/register %:p",
                    \ '/error')
    else
        call BindRunCommand("F5", "coffee %:p", "")
    endif
endf

augroup CoffeeScript
    au!
    " this one is which you're most likely to use?
    autocmd FileType coffee call InitFtCoffee()
augroup end
    " }}}

" Prevent to modify compiled files {{{
augroup JavascriptBoywer
   au!
   " this one is which you're most likely to use?
   " TODO: All except !rdpromo-*
   "autocmd BufRead */public/**.js setlocal ro | nnoremap <buffer> <c-K> :q!<CR>
   autocmd BufRead */dist/** setlocal ro | nnoremap <buffer> <c-K> :q!<CR>
augroup end
" }}}

" Zip Right
"
" Moves the character under the cursor to the end of the line.  Handy when you
" have something like:
"
"     foo
"
" And you want to wrap it in a method call, so you type:
"
"     println()foo
"
" Once you hit escape your cursor is on the closing paren, so you can 'zip' it
" over to the right with this mapping.
"
" This should preserve your last yank/delete as well.
nnoremap zl :let @z=@"<cr>x$p:let @"=@z<cr>

" Keep the cursor in place while joining lines
"nnoremap J mzJ`z
nnoremap <silent> <Plug>JoinLines mzJ`z:call repeat#set("\<Plug>JoinLines", v:count)<CR>
nmap J <Plug>JoinLines


nnoremap <lt>> V`]<
nnoremap ><lt> V`]>
nnoremap =- V`]=
" }}}

" FileTypes {{{

" FileType: php {{{
augroup php
    au!
    autocmd FileType php nnoremap <buffer> <localleader>d :call pdv#DocumentCurrentLine()<CR>
    autocmd FileType php noremap <buffer> <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>
    autocmd FileType php inoremap <buffer> <silent> <Leader>; <c-o>:call cosco#commaOrSemiColon()<CR>
augroup end
" }}}
" {{{ FileType: nginx
augroup nginx
    au!
    autocmd BufReadPost /etc/nginx/*/*.conf set ft=nginx
augroup end
" }}}
" FileType: gitcommit {{{
augroup gitcommit
    au!
    autocmd FileType gitcommit :nnoremap <silent><buffer> <c-l>
                \ :silent let b:last_commit = system("git --no-pager show HEAD --format=%s -s")<CR>
                \ :echo "Last commit: ".b:last_commit[:-2]<CR>
    autocmd FileType gitcommit :inoremap <silent><buffer> <c-l>
                \ <esc>
                \ :silent let b:last_commit = system("git --no-pager show HEAD --format=%s -s")<CR>
                \ :echo "Last commit: ".b:last_commit[:-2]<CR>
    autocmd BufReadPost,FileReadPost .git/COMMIT_EDITMSG 1,2s/Please/No Please/
" }}}
" FileType: quickfix {{{
augroup quickfix
    au!
    " Exit from grep
    autocmd FileType qf :nnoremap <silent> <buffer> <c-K> :q!<CR><C-w><C-l>
        \ :unlet! g:qfix_win<CR>
    autocmd FileType qf :nnoremap <silent> <buffer> <F5> <C-w><C-p>
augroup end
" }}}
" FileType: nerdtree {{{
augroup nerdtree
    au!
    " Space to open/close folders
    autocmd FileType nerdtree nmap <buffer><special><silent> <Space> <CR>
    autocmd FileType nerdtree :hi NonText guifg=bg
    autocmd FileType nerdtree :nnoremap <buffer>H 20<C-w><
    autocmd FileType nerdtree :nnoremap <buffer>L 20<C-w>>
    autocmd FileType nerdtree :nnoremap <buffer><s-q> <C-w>p
augroup end
" }}}
" FileType: vim {{{
augroup vim
    au!
    autocmd FileType vim setlocal foldcolumn=3
    autocmd FileType vim setlocal foldmethod=marker
    " Vimscript debugger
    autocmd FileType vim nnoremap <buffer><silent> <F7> :BreakPts<CR>
    "autocmd FileType vim iabbrev <buffer> "} " }}<C-R>=string(})<CR>
augroup end
    " }}}
" FileType: zsh {{{
augroup zsh
    au!
    autocmd FileType zsh setlocal foldcolumn=3
    autocmd FileType zsh setlocal foldmethod=marker
augroup end
" }}}
" FileType: awesome rc.lua {{{
augroup awesomerclua
    au!
    autocmd FileType lua setlocal foldcolumn=3 | setlocal foldmethod=marker
augroup end
    " }}}
" FileType: html {{{
augroup html
    au!
    autocmd FileType html setlocal nowrap
    autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
augroup end
" }}}
" FileType: css {{{
augroup css
    au!
    autocmd FileType css noremap <buffer> <c-f> :call HtmlBeautify()<cr>
augroup end
" }}}
" FileType: sh {{{

fun! Chmodsh(file)
    if exists('b:sh_new_file') |
        echo "Saving " . a:file
        if getline(1) =~ "^#!"
            exe "silent !chmod ug+x ".a:file
        endif
    endif
endf

augroup sh
   au!
   autocmd BufNewFile * let b:sh_new_file = 1
   autocmd BufWritePost *  call Chmodsh(expand("<afile>"))
   autocmd FileType sh call BindRunCommand("F5", "sh %:p", "")
augroup end
" }}}
" FileType: help {{{
augroup helpfiletype
    au!
    " this one is which you're most likely to use?
    autocmd FileType help nnoremap M <c-]><CR>
augroup end
" }}}
" FileType: javascript {{{

" Splits [1, 2, 3, 4, 5] to multi lines
fun! ArrSp()
    normal! vi[
    :s/,\s*/,/
    :exe "normal! o\ei\<CR>\evi[=vi[\ea\<CR>\e"
endfun


fun! InitFtJavaScript()
    " ~/Sources/sometest/ok
    if match(expand("%:p"), 'Sources[\\/].\{-}[\\/]test[\\/]') >= 0
        call BindRunCommand("F5", "mocha %:p", '/error')
        call BindRunCommand("F9", "node-inspector & mocha --debug-brk %", '')
    else
        call BindRunCommand("F5", "node %:p", "")
        call BindRunCommand("F9", "node-inspector & node --debug-brk %", '')
    endif
    command! ArrSp call ArrSp()
endfun

augroup javascript
    au!
    autocmd FileType javascript noremap <buffer> <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>
    autocmd FileType javascript inoremap <buffer> <silent> <Leader>; <c-o>:call cosco#commaOrSemiColon()<CR>
    autocmd FileType javascript inoremap <buffer> <silent> <c-s> " +  + "<Esc><Left><Left><Left>i
    autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
    " Run debbugger on current file (to install npm -g i node-vim-inspector)
    autocmd FileType javascript nnoremap <buffer> <leader>d :silent nbclose<CR>:Start node-vim-inspector %
        \ --vim.keys.break="F9"
        \ --vim.keys.continue="F8"
        \ --vim.keys.down=""
        \ --vim.keys.in="F11"
        \ --vim.keys.next="F10"
        \ --vim.keys.out=""
        \ --vim.keys.up="S-F11" <CR>:nbstart<CR>

    autocmd FileType javascript call InitFtJavaScript()
augroup end


" }}} FileTypes
" FileType: python {{{
augroup python
    " this one is which you're most likely to use?
    autocmd FileType python call BindRunCommand("F5", "python %", '')
augroup end
" }}}
" FileType: git {{{
augroup git
    au!
    " this one is which you're most likely to use?
    autocmd FileType git set foldlevel=9
augroup end
" }}}
" FileType: unite {{{
augroup unite
    au!
    "autocmd FileType unite nnoremap <buffer> <silent> <c-k> :<C-u>UniteClose<CR>
    "autocmd FileType unite inoremap <buffer> <silent> <c-k> <Esc>:<C-u>UniteClose<CR>
augroup end
" }}}
" FileType: jade {{{
augroup jade
    au!
augroup end
    " }}}

" }}} FileTypes

" Plugin settings {{{
" {{{ Plugin:Surround vim
" use char2nr to obtain number
let g:surround_40 = "(\r)"
let g:surround_41 = "(\r)"
let g:surround_91 = "[\r]"
let g:surround_93 = "[\r]"
" }}}
" {{{ Plugin:indentLine
let g:indentLine_char = '┊'
let g:indentLine_fileType = ['jade']
" }}}
" {{{ Plugin:Misc
let g:brkptsDefStartMode = "functions"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"
let g:delimitMate_expand_cr = 1
" }}}
" {{{ Plugin:Ack
let g:ack_default_options =
            \ " -s -H --nocolor --nogroup --column --smart-case --follow"
" }}}
" Plugin:Syntastic {{{
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_debug_file = $HOME.'/syntastic.log'
" }}}
" {{{ Plugin:NerdTree

" Autofocus file if NERDTree is not visible
fun! NERDTreeFocusAndFind()
    if s:GetNerdTreeWinNr() != -1
        NERDTreeFocus
    else
        NERDTreeFind
    endif
endf

let g:NERDCreateDefaultMappings = 0
let g:NERDTreeMapOpenInTab='<c-t>'
let g:NERDTreeMapOpenSplit='<c-i>'
let g:NERDTreeMapOpenVSplit='<c-v>'


" Alt + 1 - NERD Tree
if has('win32')
    :nnoremap <A-2> :NERDTreeFind<CR>
    :nnoremap <A-3> :GundoToggle<CR>
    :nnoremap <silent><s-q> :NERDTreeFocus<CR>
    :nnoremap <leader>T :NERDTreeFind<CR>
else
    :nnoremap <silent><s-q> :call NERDTreeFocusAndFind()<CR>
    :nnoremap <leader>T :NERDTreeFind<CR>
    :nnoremap <A-3> :GundoToggle<CR>
endif

let g:NERDTreeCopyCmd='cp '
let g:NERDTreeMinimalUI=1
let g:NERDTreeDirArrows=1
let g:nerdtree_tabs_synchronize_view=0
" Conflicts with F5 (Stealing focus)
let g:nerdtree_tabs_autofind=0
" }}}
" {{{ Plugin:NerdCommnter
map <leader>c <plug>NERDCommenterToggle
" I am always hit <leader>s to comment
map <leader>s <plug>NERDCommenterToggle
" }}}
" {{{ Plugin:Syntastic
let g:syntastic_mode_map = {
            \ "mode": "active",
            \ "active_filetypes": [],
            \ "passive_filetypes": ['java', 'html', 'rst']
            \ }
let g:syntastic_auto_jump = 0
let g:syntastic_enable_signs = 1
" }}}
" {{{ Plugin:YouCompleteMe

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" }}}
" {{{ Plugin:UltiSnips

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

let g:snips_author = 'Boris Avdeev <elephant@lislon.ru>'

" }}}
" {{{ Plugin:Pdv PHPDoc
let g:pdv_template_dir = $HOME ."/.vim/bundle/pdv/templates_snip"
" }}}
" {{{ Plugin:EasyClip

let g:EasyClipAutoFormat = 1

let g:EasyClipUseCutDefaults = 0
nmap s <Plug>MoveMotionPlug
xmap s <Plug>MoveMotionXPlug
nmap ss <Plug>MoveMotionLinePlug
nmap Y yy
let g:EasyClipUsePasteToggleDefaults = 0

"nmap <c-n> <plug>EasyClipSwapPasteForward

" Substitue
nmap <silent> gs <plug>SubstituteOverMotionMap
nmap gss <plug>SubstituteLine
xmap gs <plug>XEasyClipPaste
" }}}
" {{{ Plugin:Repeat.vim
silent! call repeat#set("\<Plug>NERDCommenterToggle", v:count)
" }}}
" {{{ Plugin:vim-smooth-scroll
"nnoremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
"nnoremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
"nnoremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
"nnoremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>
" }}}
" {{{ Plugin:vim-airline
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
      let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

" }}}
" {{{ Plugin:vim-multiply-cursors
" Ctrl+N, Ctrl-P - next, Ctrl-X - shift
let g:lsmulti_cursor_exit_from_visual_mode = 0
let g:lsmulti_cursor_exit_from_insert_mode = 0
" For example, setting it to {'\':1} will make insert-mode mappings beginning with the default leader key work in multi-cursor mode.
let g:lsmulti_cursor_insert_maps = { 'j': 1 }
" }}}
" {{{ Plugin:XkbSwitch
if has('win32')
    let g:XkbSwitchLib = expand('~/dotfiles/bin/libxkbswitch32').'.dll'
else
    let g:XkbSwitchIMappings = ['ru']
endif
let g:XkbSwitchEnabled = 1

" Fix irritating behavior when I press / on russian layout
fun! XkbRepeat()
    if g:XkbSwitchEnabled == 1
        let lang = libcall(g:XkbSwitchLib, 'Xkb_Switch_getXkbLayout', '')
        if lang == "ru"
            call feedkeys('/')
            return
        endif
    endif
    if g:loaded_repeat == 1
        call repeat#run(1)
    else
        normal! .
    endif
endf
"nnoremap <silent> . :call XkbRepeat()<CR>

fun! SearchInRus(key)
    if exists('b:xkb_ilayout') && b:xkb_ilayout == "ru"
        call libcall(g:XkbSwitchLib, 'Xkb_Switch_setXkbLayout', 'ru')
        cnoremap <buffer> <silent> <Enter> <CR>:call libcall(g:XkbSwitchLib, 'Xkb_Switch_setXkbLayout', 'us')<CR>
        cnoremap <buffer> <silent> <Esc> <C-C>:call libcall(g:XkbSwitchLib, 'Xkb_Switch_setXkbLayout', 'us')<CR>
    endif
    call feedkeys(a:key, 'n')
endf
nnoremap <silent> / :call SearchInRus('/')<CR>
nnoremap <silent> ? :call SearchInRus('?')<CR>

" }}}
" {{{ Plugin:Rooter
" Disabled because i can't run mocha from qf window
let g:rooter_change_directory_for_non_project_files = 0
" }}}
" {{{ Plugin:jsdoc
let g:jsdoc_default_mapping = 0
nnoremap <silent> gc <Plug>jsdoc
" }}}
" {{{ Plugin:Sql-workbench
let g:sw_tmp = "C:\\temp-sql"
let g:sw_exe = "c:\\opt\\sql-workbench\\sqlwbconsole.exe"
" }}}
" {{{ Plugin:unite

" {{{ grep

if executable('pt')
    let g:unite_source_grep_command = 'pt'
    let g:unite_source_grep_default_opts = '--nogroup --nocolor -S'
    let g:unite_source_grep_recursive_opt = ''
    let g:unite_source_grep_encoding = 'utf-8'
elseif executable('ag')
    let g:unite_source_grep_command = 'ag'
    let g:unite_source_grep_default_opts = '--nogroup --nocolor --column --hidden --ignore .git'
    let g:unite_source_grep_recursive_opt = ''
endif

" }}}
" GREP in project (,ff)
nnoremap <silent> <Leader>ff :<C-u>Unite grep:.
 \ -buffer-name=search-buffer -auto-preview<CR>
" Grep in system Anything
nnoremap <silent> <Leader>fg :<C-u>Unite everything/async -start-insert<CR>
" GREP word under cursor (,fw)
nnoremap <silent> <Leader>fw :<C-u>UniteWithCursorWord grep:.
            \ -buffer-name=search-buffer -auto-preview<CR>
" Find neary-by file - ,fn
nnoremap <silent> <Leader>fn :<C-u>UniteWithBufferDir file -start-insert<CR>
" MRU files (,m)
nnoremap <leader>m :<c-u>Unite file_mru -start-insert<CR>
" Line Search (,l)
nnoremap <silent> <leader>l :<C-u>Unite line -start-insert -default-action=persist_open<CR>
" New file in same buffer (use -tab for new buffer)
nnoremap <silent> <leader>n :<C-u>UniteWithBufferDir file/new
            \ -start-insert -winheight=1 -here<CR>
" List project files
nnoremap <silent> <c-p> :<C-u>UniteWithProjectDir file_rec/async -start-insert -here -winheight=10<CR>
nnoremap <silent> <leader>p :<C-u>UniteWithProjectDir file_rec/async -start-insert<CR>

if has('g:loaded_unite')

    call unite#custom#source('file_rec/async', 'matchers', ['converter_relative_word', 'matcher_default'])

    "call unite#filters#matcher_default#use(['matcher_glob'])
    call unite#custom#source('file_rec/async', 'ignore_pattern',
                \ '\v(node_modules|public)')
    call unite#custom#source('everything,everything/async', 'ignore_pattern',
                \ '\v(\.vimtmp)')
    "call unite#custom#source('file_rec', 'ignore_globs', split(&wildignore, ','))

    "let g:unite_source_alias_aliases = {
                "\   'test' : {
                "\     'source': 'file_rec',
                "\     'args': '~/',
                "\   },
                "\   'b' : 'buffer',
                "\ }

    "nnoremap <silent> <c-p> :<C-u>Unite -start-insert file_rec/async:~/Sources<CR>
    "nnoremap <silent> <leader>b :<C-u>Unite -start-insert -complete file:~/.vim/bundle<CR>
    "nnoremap <silent> <leader>cs :<C-u>Unite codesearch<CR>

    ""call unite#filters#matcher_default#use(['matcher_default'])
    ""call unite#filters#matcher_default#use(['matcher_fuzzy'])
    "if executable(g:unite_source_everything_cmd_path)
        "nnoremap <c-p> :<C-u>Unite -start-insert file buffer file_mru everything<CR>
    "else
        "nnoremap <c-p> :<C-u>Unite -start-insert file buffer file_mru<CR>
    "endif
    "nnoremap <leader>r :<C-u>Unite -start-insert file_rec/async:!<CR>
    "nnoremap <c-l> :<C-u>UniteWithBufferDir -start-insert file<CR>

    "let g:unite_source_history_yank_enable = 1
    "nnoremap <leader>y :<C-u>Unite history/yank<CR>

    ""nnoremap <silent> <c-l> :<C-u>Unite bookmark:* buffer file:!<CR>
    "call unite#custom#profile('default', 'context', {
    "\   'start_insert': 1,
    "\   'winheight': 10,
    "\   'direction': 'botright',
    "\ })
    "call unite#custom#profile('codesearch', 'context', {
    "\   'start_insert': 1,
    "\   'filters': [
    "\       'matcher_exclude_node_modules'
    "\   ]
    "\ })


    "call unite#custom#profile('line', 'context', {
    "\   'winheight': 50,
    "\ })



    "call unite#custom#source(
                "\ 'file', 'matchers',
                "\ ['converter_tail', 'matcher_default'])


    autocmd FileType unite call s:unite_my_settings()
    function! s:unite_my_settings()
        " Overwrite settings.
        "imap <buffer> kj      <Plug>(unite_insert_leave)
        imap <buffer> <c-k>   <Plug>(unite_exit)
        nmap <buffer> <c-k>   <Plug>(unite_exit)
        nmap <buffer> <Esc>   <Plug>(unite_exit)

        " Empty cache
        nmap <buffer> <F5>   <Plug>(unite_redraw)
        imap <buffer> <F5>   <Plug>(unite_redraw)

        " Pressing backspace on empty input results to closing of unite
        silent! iunmap <buffer> <Backspace>

        " Fast selection
        imap <buffer> '     <Plug>(unite_quick_match_default_action)
        nmap <buffer> '     <Plug>(unite_quick_match_default_action)

        "imap <buffer><expr> k unite#smart_map('k', '')
        "imap <buffer> <TAB>   <Plug>(unite_select_next_line)
        " How i will switch window?
        nmap <buffer> <C-S-w>     <Plug>(unite_delete_backward_path)
        imap <buffer> <C-S-w>     <Plug>(unite_delete_backward_path)
        "imap <buffer><expr> x
                    "\ unite#smart_map('x', "\<Plug>(unite_quick_match_choose_action)")
        "nmap <buffer> x     <Plug>(unite_quick_match_choose_action)
        "nmap <buffer> <C-z>     <Plug>(unite_toggle_transpose_window)
        "imap <buffer> <C-z>     <Plug>(unite_toggle_transpose_window)
        "imap <buffer> <C-y>     <Esc><Plug>(unite_narrowing_path)
        "nmap <buffer> <C-y>     <Plug>(unite_narrowing_path)
        nmap <buffer> <C-j>     <Plug>(unite_toggle_auto_preview)
        "nmap <buffer> <C-r>     <Plug>(unite_narrowing_input_history)
        "nmap <buffer> <C-d>     <Plug>(unite_input_directory)
        "imap <buffer> <C-r>     <Plug>(unite_narrowing_input_history)
        "nnoremap <silent><buffer><expr> l
                    "\ unite#smart_map('l', unite#do_action('default'))

        "let unite = unite#get_current_unite()
        "if unite.profile_name ==# 'search'
            "nnoremap <silent><buffer><expr> r     unite#do_action('replace')
        "else
            "nnoremap <silent><buffer><expr> r     unite#do_action('rename')
        "endif

        "nnoremap <silent><buffer><expr> cd     unite#do_action('lcd')
        "nnoremap <buffer><expr> S      unite#mappings#set_current_filters(
                    "\ empty(unite#mappings#get_current_filters()) ?
                    "\ ['sorter_reverse'] : [])

        " Runs "split" action by <C-s>.
        imap <silent><buffer><expr> <C-i>     unite#do_action('split')
        imap <silent><buffer><expr> <C-v>     unite#do_action('vsplit')
        nmap <silent><buffer><expr> <c-i>     unite#do_action('split')
        nmap <silent><buffer><expr> <C-v>     unite#do_action('vsplit')
        " nmap <c-i> resets <tab>
        nmap <buffer> <Tab>                  <Plug>(unite_choose_action)
    endfunction
endif

" }}}
" {{{ Plugin:unite-everything
let g:unite_source_everything_cmd_path = substitute($HOME, "\\", "/", "g")."/dotfiles/bin/es.exe"
" }}}
" {{{ Plugin:unite-codesearch
let g:unite_source_codesearch_command = $HOME."/.go/bin/csearch"
" }}}
" Plugin:Browserify {{{
augroup css
    au!
    autocmd InsertLeave *.css :BLReloadCSS
augroup end
let g:bl_pagefiletypes = ['html', 'javascript', 'php', 'jade']
" }}}
" Plugin:vim-translate {{{
" Shift +K https://github.com/soimort/translate-shell/wiki/Text-Editor-Integration
nnoremap K :!trans <C-R><C-W><CR>
" }}}
" }}}

" {{{ Test

fun! Sorder(list)
    let new_list = deepcopy(a:list)
    call sort(new_list)
    return new_list
endf
" }}}

" {{{ Project navigation

command! SwCommon NERDTree rdpromo-common
command! SwSite   NERDTree rdpromo-site
command! SwMarkup NERDTree rdpromo-markup
" }}}

