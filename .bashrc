# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

shopt -s extglob

complete -f -X '!*.php' php
