# Manually
echo "* touchpad touch as click"
echo "* touchpad disable three key functions"

# Env
repoPath=$HOME"/repo"
dotRepo="dotfile"
dotPath=$repoPath"/"$dotRepo
dotRepoGit="git@github.com:zwpaper/dotfile.git"

## Application list
caskApps="
omnifocus
iterm2
karabiner-elements
"

# Mac tools
which brew
result=`echo $?`
if [ X$result == X1 ]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

for i in $caskApps; do
    brew cask install $i
done

# karabiner-elements
if [ ! -f ~/.config/karabiner/karabiner.json ]; then
    echo "setting karabiner..."
    mv ~/.config/karabiner ~/.config/karabiner.bak
    ln -s $dotPath/karabiner ~/.config/karabiner
    echo "karabiner done"
fi


# dev software
# brew install emacs

# dev env
if [ ! -d $repoPath ]; then
    mkdir -p $repoPath
fi
## clone dotfile
if [ ! -d $dotPath ]; then
    cd $repoPath
    git clone $dotRepoGit
fi
## oh my zsh
if [ ! -d ~/.oh-my-zsh ]; then
    brew install zsh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    rm -f ~/.zshrc
    ln -s ~/repo/dotfile/zshrc ~/.zshrc
fi
