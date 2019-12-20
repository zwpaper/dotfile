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

GOROOT_PARENT=$HOME/code/
GOPATH=$HOME/code/golang
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
font-source-code-pro
"
## Homebrew Apps
brewApps="
zsh
bat
lsd
"
## GNU Apps, should install with default name
gnuApps="
gnu-sed
"
### end macOS software

### Start installing
echo "Installing apps in macOS"
# Mac tools
which brew
result=`echo $?`
if [ X$result == X1 ]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew tap homebrew/cask-fonts
fi

export HOMEBREW_NO_AUTO_UPDATE=1
for i in $brewApps; do
    echo ""
    echo "Installing "$i
    brew install $i
done
for i in $gnuApps; do
    echo ""
    echo "Installing "$i
    brew install $i --with-default-names
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
    brew install --fetch-HEAD emacs-plus --HEAD --without-spacemacs-icon --with-modern-icon --with-jansson --with-xwidgets
fi
if [ ! -L $HOME/.emacs.d ]; then
    git clone $emacsRepoHTTPS $repoPath
    ln -s $repoPath/paper-emacs $HOME/.emacs.d
fi
### End installing

# dev software
# dev env
if [ ! -d $repoPath ]; then
    # this init.sh is saved in repo, should not go here
    echo "no repo path found"
    exit 1
fi
## oh my zsh
if [ ! -d ~/.oh-my-zsh ]; then
    echo "setting zsh..."
    sh -c \
       "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" \
       "" --unattended # do not delete the space
    rm -f ~/.zshrc
    ln -s $dotPath/zshrc ~/.zshrc
    echo "zsh done..."
fi

# karabiner-elements
if [ ! -f ~/.config/karabiner/karabiner.json ]; then
    echo "setting karabiner..."
    if [ -f ~/.config/karabiner ]; then
        mv ~/.config/karabiner ~/.config/karabiner.bak
    fi
    mkdir -p ~/.config
    ln -s $dotPath/karabiner ~/.config/karabiner
    echo "karabiner done"
fi

# Golang
if [ ! -d $GOROOT_PARENT ]; then
    curl -SLo /tmp/ $GO_URL
    mkdir -p $GOROOT_PARENT
    tar -C $GOROOT_PARENT -xzf go$GO_VERSION.darwin-amd64.tar.gz
fi

# Sync
## Spelling
if [ ! -h ~/Library/Spelling ]; then
    echo "sync spelling..."
    sudo mv ~/Library/Spelling/ ~/Library/Spelling.bak
    ln -s ~/Dropbox/AppSync/Spelling ~/Library/Spelling
    echo "spelling done"
fi
