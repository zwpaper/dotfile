# OS Specific
case `uname` in
    Darwin)
        export HOME="/Users/zhangwei"
        # gettest form emacs
        export PATH="/usr/local/opt/gettext/bin:$PATH"
        export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:/usr/local/opt/ncurses/bin:/usr/local/opt/texinfo/bin:$PATH"
        export GOROOT="$HOME/code/go/"
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
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
ENABLE_CORRECTION="true"
ZSH_TMUX_AUTOSTART="true"

# User configuration
export LC_ALL="en_US.UTF-8"
export MANPATH="/usr/local/man:$MANPATH"
export PATH=$HOME/.yarn/bin:$HOME/.cabal/bin:$HOME/Library/Haskell/bin:$HOME/.local/bin:$GOROOT/bin:$GOPATH/bin:$PATH
export PATH=$HOME/.bin:$PATH
export PATH=$PATH:/usr/local/bin

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


# Emacs
alias e="emacsclient -t -a ''"
export EDITOR="emacsclient -t -a ''"

## vterm
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'


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

# Lang
export LDFLAGS="-L/usr/local/opt/ncurses/lib"
export CPPFLAGS="-I/usr/local/opt/ncurses/include"

## Flutter
export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn
export PATH="$HOME/code/repo/flutter/bin:$PATH"
export PATH="$HOME/code/repo/flutter/.pub-cache/bin:$PATH"

## Golang
export GO111MODULE=on
export GOPROXY=http://goproxy.cn

## Python
export PATH=$HOME/Library/Python/3.7/bin:$PATH
alias pip='pip3 --user'
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/sensetime/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/sensetime/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/sensetime/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/sensetime/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

## Latex
export PATH=/Library/TeX/texbin/:$PATH
export TEXINPUTS=::~/OneDrive/app-sync/tex/texmf/

## LLVM
export PATH="/usr/local/opt/llvm/bin:$PATH"

# iTerm2
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
iterm2_print_user_vars() {
  it2git
}

# shell
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

eval "$(zoxide init zsh)"

eval "$(starship init zsh)"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/sbin:$PATH"
