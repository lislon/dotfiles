# {{{ Oh My Zsh
# Path to your oh-my-zsh installation.
export ZSH=$HOME/dotfiles/.oh-my-zsh
export LC_ALL='en_US.UTF-8'

export SYSTEMD_EDITOR="emacsclient -c"
export EDITOR="emacsclient -c"
export SUDO_EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"


# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="bira"


# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(git composer fasd gitfast autojump bower npm common-aliases wd)
if [ -f "/etc/arch-release" ]; then
    plugins+=(archlinux systemd)
    export SSH_AUTH_SOCK=/run/user/1000/ssh-agent.socket
elif `which ssh-agent &>/dev/null`; then
    plugins+=(ssh-agent)
fi
# Use j <dirspec>
#plugins=(git composer fasd gitfast)

source $ZSH/oh-my-zsh.sh

# }}}
# {{{ Aliases
alias myzsh="$EDITOR ~/dotfiles/.zshrc && reload"
alias view=vim -R
alias reload=". ~/.zshrc"
alias v='f -e vim' # quick opening files with vim
alias gl='git --no-pager log --oneline --graph -5'
alias gpush='git push'
alias tmux='tmux -2'
alias s="sudo "
alias -g G="| grep -i"
alias -g L="| less -R"
alias -g F="| ack --passthru "
alias ack="ack --pager='less -RFSX'"
alias luarc="$EDITOR ~/.config/awesome/rc.lua"
#alias fx='setxkbmap -layout "us,ru" -option "grp:rctrl_toggle"'
alias fx='setxkbmap -layout "us,ru" -option "grp:ctrl_shift_toggle" -option ctrl:nocaps'
alias help='man'
alias cal='cal -m'
alias ll='ls -hla'
alias svim='sudo -E vim'
alias -s pdf='xdg-open'
alias recent='find . -maxdepth 1 -type f -atime -1'             # show recent files
alias emax="emacsclient -nw"                      # used to be "emacs -nw"
alias e="emacsclient -c"
#alias semac="sudo emacsclient -t"                # used to be "sudo emacs -nw"
#alias emacs="emacsclient -c -a emacs"           # new - opens the GUI with alternate non-daemon
alias sc-cat='systemctl cat'

# OS-depend aliases
if `which pacman &>/dev/null`; then
    alias i="sudo pacman -S"
    alias is="pacman -Ss"
elif `which apt-get &>/dev/null`; then
    alias i="sudo apt-get install"
    alias is="sudo apt-get search"
elif `which yum &>/dev/null`; then
    alias i="sudo yum install"
    alias is="sudo yum search"
fi
# }}}
# {{{ Settings

declare -U path
path=( ~/bin ~/.local/bin $path )

fpath=(~/dotfiles/.zshfunc $fpath)

export ENV='development'
export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
#export EDITOR='vim'
# locate / | fzf
#    c-v open in vim
#    c-V sudiedit
export FZF_DEFAULT_OPTS='--extended --cycle '
setopt extended_glob
setopt rc_expand_param
setopt correct
setopt interactivecomments
setopt share_history

# Terminal in emacs do not show nice glyphs :( --workaround
if [[ -n ${INSIDE_EMACS} ]]; then
    # This shell runs inside an Emacs *shell*/*term* buffer.
    #prompt walters
    unsetopt zle
    PS1='%{$terminfo[bold]$fg[green]%}%n@%m%{$reset_color%} %{$terminfo[bold]$fg[blue]%}%~%{$reset_color%} $ '
else
    #Fix backspace issue when using rxvt + ssh
    export TERM='xterm'
fi


# Show stats if commands takes longer then 10 sec
REPORTTIME=10
# }}}
# {{{ Autocompletion
# zstyle :completion:function:completer:command:argument:tag
zstyle ':completion:::::' completer _expand _complete _approximate _ignored
zstyle ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) )'
# autocomplete one way readme -> README, but not README -> readme
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# Ignore same arguments twice
zstyle ':completion:*:(rm|vi|vim|mv|cp):*' ignore-line true
zstyle ':completion::*' ignore-parents parent pwd
zstyle ':completion:*:(vi|vim):*' ignored-patterns '*.(pdf|ps|dvi|aux)'
zstyle ':completion:*:(node):*' file-patterns '*.js'
zstyle ':completion:*:(coffee):*' file-patterns '*.coffee'
zstyle ':completion:*:(node):*' ignored-patterns 'Gruntfile.js'

# -g = glob pattern, (-.) glob modifier to allow only files or symlinks
compdef '_files -g "*.jpg(-.)"' gimp
# }}}
# {{{ Man in vim
export MANPAGER="/bin/sh -c \"col -b | vim -u ~/dotfiles/man.vimrc -c 'set nomodified' -\""
# }}}
# {{{ Misc

bak () {
    mv $1 $1.bak
    echo "Make backup $1.bak"
}

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
    local file
    file=$(fzf --query="$1" --select-1 --exit-0)
    [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# Modified version where you can press
#   - CTRL-O to open with `open` command,
#   - CTRL-E or Enter key to open with the $EDITOR
fo() {
    local out file key
    out=$(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e)
    key=$(head -1 <<< "$out")
    file=$(head -2 <<< "$out" | tail -1)
    if [ -n "$file" ]; then
        [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-vim} "$file"
    fi
}

# # fd - cd to selected directory
# fd() {
#     local dir
#     dir=$(find ${1:-*} -path '*/\.*' -prune \
#                -o -type d -print 2> /dev/null | fzf +m) && cd "$dir"
# }

# fda - including hidden directories
fda() {
    local dir
    dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# cdf - cd into the directory of the selected file
cdf() {
    local file
    local dir
    file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

fag() {
   ag --hidden --nobreak --nonumbers --noheading --depth 5 . | fzf
}

mkdircd() {
    mkdir -p $1 && cd $1
}

writecmd() {
    perl -e '$TIOCSTI = 0x5412; $l = <STDIN>; $lc = $ARGV[0] eq "-run" ? "\n" : ""; $l =~ s/\s*$/$lc/; map { ioctl STDOUT, $TIOCSTI, $_; } split "", $l;' -- $1
}

# Allow multi-selections from menu using Ctrl+o
bindkey -M menuselect '\C-o' accept-and-menu-complete
bindkey '\C-i' complete-word
bindkey '\e#' pound-insert

# autoload -U -- ~/dotfiles/.zshfunc/[^_]*(:t)

# Ctrl-X, U - and you can enter any name of widget to execute
bindkey '\C-xu' universal-argument
#bindkey '^R' history-incremental-search-backward


# C-S inserts sudo before line
insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo

#export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

#export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
#export XDG_CONFIG_HOME=~/.config

if [[ -f /etc/profile.d/fzf.zsh ]] ; then
    source /etc/profile.d/fzf.zsh
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS=--extended-exact

if [[ -f ~/.zshrc_local ]] ; then
    source ~/.zshrc_local
fi


# }}}
