# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

shopt -s extglob

complete -f -X '!*.php' php

# User specific aliases and functions

set history-size 10000

# Template for coloring string
#cyan=$(tput setaf 6)
#bold=$(tput bold)
#reset=$(tput sgr0)
#PS1='\[$cyan$bold\][\u@\h \W] $ \[$reset\]'
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
