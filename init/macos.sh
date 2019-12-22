os=`uname`
case $os in
    Darwin)
        echo "starting init"
        ;;
    *)
        echo support macOS only
        exit 1
        ;;
esac

# Manually
echo "* touchpad touch as click"
echo "* touchpad disable three key functions"
echo "* add ssh key to github"

# Env
repoPath=$HOME"/code/repo"
dotRepo="dotfile"
dotPath=$repoPath/$dotRepo
dotRepoGit="git@github.com:zwpaper/dotfile.git"
dotRepoHTTPS="https://github.com/zwpaper/dotfile.git"

emacsRepoHTTPS="https://github.com/zwpaper/paper-emacs.git"
emacsPath=$HOME/.emacs.d/

GOROOT_PARENT=$HOME/code/
GOROOT=$GOROOT_PARENT/go/
GOPATH=$HOME/code/golang/
GO_VERSION=1.13.5
GO_URL="https://dl.google.com/go/go$GO_VERSION.darwin-amd64.tar.gz"

## Application list
## Mac App Store
masApps="
reeder
spark
xnip
"
## Homebrew Cask
caskApps="
alfred
omnifocus
karabiner-elements
hammerspoon
istat-menus
quitter
dropbox
google-chrome
1password
iterm2
visual-studio-code
postman
docker
slack
wechat
neteasemusic
font-hack-nerd-font
squirrel
bettertouchtool
"
## Homebrew Apps
brewApps="
zsh
bat
lsd
cmake
gnu-sed
gcc
socat
aspell"
### end macOS software

function install_mac_apps() {
    echo "Installing apps in macOS"
    # Mac tools
    which brew
    result=`echo $?`
    if [ X$result == X1 ]; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    brew tap homebrew/cask-fonts

    export HOMEBREW_NO_AUTO_UPDATE=1
    for i in $brewApps; do
        echo ""
        echo "Installing "$i
        brew install $i
    done
    for i in $caskApps; do
        echo ""
        echo "Installing "$i
        brew cask install $i
    done
    # alfred 3
    if [ ! -e /Applications/Alfred\ 3.app ]; then
        curl -SLo /tmp/alfred3.dmg https://cachefly.alfredapp.com/Alfred_3.8.6_972.dmg
        hdiutil mount /tmp/alfred3.dmg
        sudo cp -R /Volumes/Alfred/Alfred\ 3.app /Applications
        hdiutil unmount /Volumes/Alfred
    fi

    # Emacs 27
    if [ ! -e /Applications/Emacs.app ]; then
        brew tap d12frosted/emacs-plus
        brew install --fetch-HEAD emacs-plus --HEAD --without-spacemacs-icon --with-modern-icon --with-jansson
    fi
    if [ ! -L $HOME/.emacs.d ]; then
        git clone $emacsRepoHTTPS $repoPath
        ln -s $repoPath/paper-emacs $emacsPath
    fi
}

# dev software
# dev env
## oh my zsh
function init_oh_my_zsh() {
    if [ ! -d ~/.oh-my-zsh ]; then
        echo "setting zsh..."
        sh -c \
           "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" \
           "" --unattended # do not delete the space
        rm -f ~/.zshrc
        ln -s $dotPath/zshrc ~/.zshrc
        echo "zsh done..."
    fi
}

# karabiner-elements
function init_karabiner() {
    if [ ! -f ~/.config/karabiner/karabiner.json ]; then
        echo "setting karabiner..."
        if [ -f ~/.config/karabiner ]; then
            mv ~/.config/karabiner ~/.config/karabiner.bak
        fi
        mkdir -p ~/.config
        ln -s $dotPath/karabiner ~/.config/karabiner
        echo "karabiner done"
    fi
}

## language
function init_golang() {
    if [ ! -d $GOROOT ]; then
        curl -SLo /tmp/go.tar.gz $GO_URL
        mkdir -p $GOROOT_PARENT
        mkdir -p $GOPATH
        tar xf /tmp/go.tar.gz -C $GOROOT_PARENT
    fi
    command -v gopls >/dev/null 2>&1 || {
        GOROOT=$GOROOT GOPATH=$GOPATH PATH=$GOROOT/bin:$PATH go get golang.org/x/tools/gopls@latest
    }
}

function init_rust() {
    command -v rustup >/dev/null 2>&1 || {
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    }
    command -v rls >/dev/null 2>&1 || {
        source $HOME/.cargo/env && rustup component add rls rust-analysis rust-src
    }
}
## End language

function init_rime() {
    LIBRIME_PATH="$HOME/code/repo/librime"
    if [ ! -d $LIBRIME_PATH ]; then
    echo "download librime..."
    git clone --recursive https://github.com/rime/librime.git --depth=1 $LIBRIME_PATH
    mkdir -p $LIBRIME_PATH/xbuild/lib/Release/
    cp /Library/Input\ Methods/Squirrel.app/Contents/Frameworks/librime.1.dylib $LIBRIME_PATH/xbuild/lib/Release/librime.dylib
    fi
    LIBERIME_PATH="$HOME/code/repo/liberime"
    if [ ! -d $LIBERIME_PATH ]; then
        echo "download liberime..."
        git clone https://gitlab.com/liberime/liberime.git --depth=1 $LIBERIME_PATH
        echo "build liberime..."
        cd $LIBERIME_PATH
        export RIME_PATH=$LIBRIME_PATH
        make liberime
        cp $LIBERIME_PATH/build/liberime.so $emacsPath/plugin/
        cd -
    fi
    sudo cp /Library/Input\ Methods/Squirrel.app/Contents/Frameworks/librime.1.dylib /usr/local/lib/
}


# Sync
## Spelling
function sync_spelling() {
    if [ ! -h ~/Library/Spelling ]; then
        echo "sync spelling..."
        sudo mv ~/Library/Spelling/ ~/Library/Spelling.bak
        ln -s ~/Dropbox/AppSync/Spelling ~/Library/Spelling
        echo "spelling done"
    fi
}

init_golang
init_rust
