export HOME="/home/paper"
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="dst"
plugins=(git, autojump, tmux)

# User configuration
export PATH="$PATH"
export MANPATH="/usr/local/man:$MANPATH"
export GOROOT=/usr/local/go
export GOPATH=$HOME/golang
export PATH=$HOME/.bin:$GOPATH/bin:$PATH

# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1
# Set colors to match iTerm2 Terminal Colors
export TERM=xterm-256color
export LSCOLORS=Gxfxcxdxbxegedabagacad

source $ZSH/oh-my-zsh.sh


alias ls='ls -h --color=auto'
alias ll='ls -lh --color=auto'
alias la='ls -ah --color=auto'
alias rm='rm -i'
alias grep="grep --color=auto"
alias -s gz='tar -xzvf'
alias -s tgz='tar -xzvf'
alias -s zip='unzip'
alias -s bz2='tar -xjvf'

alias e='emacs'

# Git
alias gcl='git clone'
alias gsh="ssh-agent bash -c 'ssh-add ~/.ssh/paper; git push '"
alias gst='git status'
alias gch='git checkout'
alias gbr='git branch'
alias gif='git diff'

alias gad='git add'
alias grm='git rm'
alias glo='git log'
alias gcm='git commit -m'

alias gre='git remote'
alias grv='git remote -v'

# Function alias
cdmk() { mkdir $@ && cd $_ ;}
