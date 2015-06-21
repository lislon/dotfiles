SetTitleMatchMode, 2
#f::
IfWinExist, - Mozilla Firefox
    WinActivate
return

#s::
IfWinExist, Skype
WinActivate
return

#p::
IfWinExist, PhpStorm
WinActivate
return
