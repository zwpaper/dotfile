# OS Specific
case `uname` in
    Darwin)
        export HOME="/Users/zhangwei"
        export PATH="/usr/local/opt/ncurses/bin:$PATH"
        export GOROOT="/usr/local/opt/go/libexec/"
        export GOPATH=$HOME"/code/golang"
        ;;
    Linux)
        export HOME="/home/paper"
        export GOROOT=/usr/local/go
        export GOPATH=$HOME/golang

        pacman() {
            case $1 in
                 -S | -D | -S[^sih]* | -R* | -U*)
                    /usr/bin/sudo /usr/bin/pacman "$@" ;;
                 *)
                    /usr/bin/pacman "$@" ;;
            esac
        }
        ;;
    FreeBSD)
        # commands for FreeBSD go here
        ;;
esac

export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="dst"
plugins=(git z zsh-autosuggestions zsh-syntax-highlighting tmux)
ENABLE_CORRECTION="true"
ZSH_TMUX_AUTOSTART="true"

# User configuration
export LC_ALL="en_US.UTF-8"
export PATH="$PATH"
export MANPATH="/usr/local/man:$MANPATH"
export PATH=$HOME/.bin:/usr/local/bin/:$GOROOT/bin:$GOPATH/bin:$PATH
export PATH="$HOME/Library/Haskell/bin:$PATH"

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

alias ls="lsd"
alias la="ls -a"
alias lla='ls -la'
alias lt='ls --tree'

alias cat="bat"

alias rm='rm -i'
alias grep="grep --color=auto"
alias -s gz='tar -xzvf'
alias -s tgz='tar -xzvf'
alias -s zip='unzip'
alias -s bz2='tar -xjvf'

# Emacs
alias e="emacsclient -t -a ''"
export EDITOR="emacsclient -t -a ''"

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
alias glg='git log --all --decorate --oneline --graph'
alias gcm='git commit -m'

alias gre='git remote'
alias grv='git remote -v'

# Function alias
cdmk() { mkdir $@ && cd $_ ;}

# work script
if [ -f ~/.zsh.work ]; then
    source ~/.zsh.work
fi

# env
HTTP_PROXY=http://127.0.0.1:7890
HTTPS_PROXY=http://127.0.0.1:7890
NO_PROXY=127.0.0.1,localhost,192.168.0.0/16,10.0.0.0/8

export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn
export PATH="$HOME/repo/flutter/bin:$PATH"

