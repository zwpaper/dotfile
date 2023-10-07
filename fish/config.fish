fish_config theme choose nord

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /Users/zhangwei/opt/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

## Shell Configuration
set -gx EDITOR emacs -q -nw
set -x PATH ~/.bin ~/.cargo/bin/ $PATH

function proxy
  set -U HTTP_PROXY http://localhost:7890
  set -U HTTPS_PROXY $HTTP_PROXY
  set -U http_proxy $HTTP_PROXY
  set -U https_proxy $HTTP_PROXY
  set -U all_proxy $HTTP_PROXY
  set -U NO_PROXY sensetime.com localhost
  echo Proxy set to $HTTP_PROXY
  echo No Proxy $NO_PROXY
end


# pure fish prompt
# https://github.com/pure-fish/pure
set -g async_prompt_functions _pure_prompt_git
set -U pure_enable_k8s true
set --universal pure_show_system_time true


alias ls="lsd "
alias ll="lsd -lh "
alias la="lsd -lha "

alias e='emacs -q -nw'

zoxide init fish | source

## Kubernetes
alias k="kubectl"
alias ks="kubectl -n kube-system "
kubectl completion fish | source

### KCL
set -x PATH $PATH ~/.bin/kclvm/bin

## Golang
set -U GOPATH ~/code/golang
set -U GOROOT ~/.gobrew/current
set -x PATH $GOPATH/bin $PATH
set -x PATH $GOROOT/bin $PATH

source ~/.config/fish/work.fish
