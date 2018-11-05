# Manually
echo "* touchpad touch as click"
echo "* touchpad disable three key functions"

# Env
repoPath=$HOME"/repo"
dotRepo="dotfile"
dotPath=$repoPath"/"$dotRepo
dotRepoGit="git@github.com:zwpaper/dotfile.git"

## Application list
## Mac App Store
masApps="
bear
reeder
spark
mweb
xnip
airmail
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
qingg
iterm2
visual-studio-code
postman
docker
slack
wechat
neteasemusic
"
## Homebrew Apps
brewApps="
zsh
emacs
golang
global
"

# Mac tools
which brew
result=`echo $?`
if [ X$result == X1 ]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

export HOMEBREW_NO_AUTO_UPDATE=1
for i in $brewApps; do
    brew install $i
done
for i in $caskApps; do
    brew cask install $i
done

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
    brew install zsh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    rm -f ~/.zshrc
    ln -s ~/repo/dotfile/zshrc ~/.zshrc
    echo "zsh done..."
fi
## Emacs
if [ ! -h ~/.emacs.d ]; then
    echo "setting emacs..."
    mv ~/.emacs.d ~/emacs.d.bak
    ln -s $dotPath/emacs.d ~/.emacs.d
    echo "emacs done"
fi

# karabiner-elements
if [ ! -f ~/.config/karabiner/karabiner.json ]; then
    echo "setting karabiner..."
    mv ~/.config/karabiner ~/.config/karabiner.bak
    ln -s $dotPath/karabiner ~/.config/karabiner
    echo "karabiner done"
fi

# Sync
## Spelling
if [ ! -h ~/Library/Spelling ]; then
    echo "sync spelling..."
    sudo mv ~/Library/Spelling/ ~/Library/Spelling.bak
    ln -s ~/Dropbox/AppSync/Spelling ~/Library/Spelling
    echo "spelling done"
fi