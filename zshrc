# OS Specific
case `uname` in
  Darwin)
    export HOME="/Users/paper"
    export PATH="/usr/local/opt/ncurses/bin:$PATH"
  ;;
  Linux)
    export HOME="/root"
  ;;
  FreeBSD)
    # commands for FreeBSD go here
  ;;
esac

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="dst"
plugins=(git z zsh-autosuggestions)
ENABLE_CORRECTION="true"
ZSH_TMUX_AUTOSTART="true"

# User configuration
export LC_ALL="en_US.UTF-8"
export PATH="$PATH"
export MANPATH="/usr/local/man:$MANPATH"
export GOROOT=/usr/local/go
export GOPATH=$HOME/golang
export PATH=$HOME/.bin:$GOROOT/bin:$GOPATH/bin:$PATH

# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1
# Set colors to match iTerm2 Terminal Colors
export TERM=xterm-256color
export LSCOLORS=Gxfxcxdxbxegedabagacad

source $ZSH/oh-my-zsh.sh
export PROMPT="
%{$terminfo[bold]$fg[blue]%}#%{$reset_color%} \
%(#,%{$fg[magenta]%}%n%{$reset_color%},%{$fg[cyan]%}%n) \
%{$fg[white]%}@ \
%{$fg[green]%}%m \
%{$fg[white]%}in \
%{$terminfo[bold]$fg[yellow]%}%~%{$reset_color%}\
${hg_info}\
${git_info}\
 \
%{$fg[white]%}[%*] $exit_code
%{$terminfo[bold]$fg[red]%}$ %{$reset_color%}"

if [ "$(uname 2> /dev/null)" = "Linux" ]; then
   alias ls='ls -h --color=auto'
   alias ll='ls -lh --color=auto'
   alias la='ls -ah --color=auto'
fi
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
