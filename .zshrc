# Path to your oh-my-zsh installation.
export ZSH=$HOME/dotfiles/.oh-my-zsh
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="agnoster"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git composer fasd gitfast ssh-agent coffe dirhistory npm vi-mode)
#plugins=(git composer fasd gitfast)

source $ZSH/oh-my-zsh.sh

# User configuration

#export PATH="/usr/local/bin:/usr/bin:/cygdrive/c/opt/git/bin:/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0:/cygdrive/c/Program Files (x86)/ATI Technologies/ATI.ACE/Core-Static:/cygdrive/c/Program Files (x86)/Common Files/Acronis/SnapAPI:/cygdrive/c/Program Files (x86)/Acronis/TrueImageHome:/cygdrive/c/Program Files/Windows Imaging:/cygdrive/c/Program Files (x86)/Calibre2:/cygdrive/c/opt/php:/cygdrive/c/opt/pear:/cygdrive/c/opt/git/bin:/cygdrive/c/opt/mysql5.6/bin:/cygdrive/c/opt/composer:/cygdrive/c/opt/putty:/cygdrive/c/opt/python/App:/cygdrive/c/opt/python/App/Scripts:/cygdrive/c/Program Files/TortoiseGit/bin"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias myzsh="vim ~/dotfiles/.zshrc"
alias z=myzsh
alias vimrc="vim ~/dotfiles/.vimrc"
alias reload=". ~/.zshrc"

if [[ -f ~/.zshrc_local ]] ; then 
	source ~/.zshrc_local
fi

export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
alias v='f -e vim' # quick opening files with vim
setopt extended_glob
# Allow multi-selections from menu using Ctrl+o
bindkey -M menuselect '\C-o' accept-and-menu-complete
bindkey '\C-i' complete-word
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

# -g = glob pattern, (-.) glob modifier to allow only files or symlinks
compdef '_files -g "*.jpg(-.)"' gimp

#export PATH=~/bin/:$PATH
	
declare -U path
path=( ~/bin ~/.local/bin $path )

fpath=(~/dotfiles/.zshfunc $fpath)
# autoload -U -- ~/dotfiles/.zshfunc/[^_]*(:t)

# Ctrl-X, U - and you can enter any name of widget to execute
bindkey '\C-xu' universal-argument

# showares "BEFORE{$arr[@]}AFTER" behaves like 
# % showargs BEFORE{one, two\ three,four}AFTER
# >> BEFOREoneAFTER<<
# >> BEFOREtwo threeAFTER<<
# >> BEFOREfourAFTER<<
setopt rc_expand_param
alias gl='git --no-pager log --oneline --graph -5'
alias gpush='git push'

