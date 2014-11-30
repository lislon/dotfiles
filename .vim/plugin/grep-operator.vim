nnoremap <leader>g :set operatorfunc=GrepOperator<cr>g@
vnoremap <leader>g :<c-u>call GrepOperator(visualmode())<cr>

fun! GrepOperator(type)
    let saved_unnamed_register = @@

    "normal! mC
    if a:type ==# 'v'
        normal! `<v`>y
    elseif a:type ==# 'char'
        normal! `[v`]y`
    else
        return
    endif

    silent execute "grep -r " . shellescape(@@) . " ."
    copen 20

    let @@ = saved_unnamed_register
endf

