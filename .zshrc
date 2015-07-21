# {{{ Oh My Zsh
# Path to your oh-my-zsh installation.
export ZSH=$HOME/dotfiles/.oh-my-zsh
export LC_ALL='en_US.UTF-8'

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
alias myzsh="vim ~/dotfiles/.zshrc && reload"
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
#alias emax="emacsclient -t"                      # used to be "emacs -nw"
#alias semac="sudo emacsclient -t"                # used to be "sudo emacs -nw"
#alias emacs="emacsclient -c -a emacs"           # new - opens the GUI with alternate non-daemon
alias sc-cat='systemctl cat'

# OS-depend aliases
if `which pacman &>/dev/null`; then
    alias i="sudo pacman -S"
    alias is="pacman -Ss"
elif `which apt-get &>/dev/null`; then
    alias i="sudo apt-get install "
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
export EDITOR='vim'
# Fix backspace issue when using rxvt + ssh
export TERM='xterm'
setopt extended_glob
setopt rc_expand_param
setopt correct
setopt interactivecomments


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
man () {
    tempo=`/usr/bin/man $*`
    if [[ $? == 0 ]]; then
        vim -R \
            -u NONE \
            -c 'normal! M' \
            -S =(echo '
        set ft=man nomod nolist nonumber cpoptions-=<
        set scrolloff=300 hlsearch ignorecase smartcase
        set clipboard=unnamed,unnamedplus
        syntax on
        nnoremap q :q!<CR>
        nnoremap <Space> <C-d>
        nnoremap d <C-d>
        nnoremap u <C-u>
        nnoremap a <NOP>
        nnoremap A <NOP>
        nnoremap i <NOP>
        nnoremap I <NOP>
        nnoremap K :!trans <C-R><C-W><CR>'
        ) \
            <(echo $tempo)
    fi
}
# }}}
# {{{ Misc


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

if [[ -f ~/.zshrc_local ]] ; then
    source ~/.zshrc_local
fi

# }}}
