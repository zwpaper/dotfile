#!/usr/bin/env zsh

source basic.sh

function software::ensure_pip3() {
    alias pip3="pip3 --user "
}

function software::ensure_homebrew() {
    if ! exists_command brew; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    export HOMEBREW_NO_AUTO_UPDATE=1
}

function software::ensure_npm() {
    if ! exists_command npm; then
        software::ensure_homebrew
        brew install npm
    fi
}

function software::ensure_by_brew() {
    if [[ $# == 0 ]]; then
        return 0
    fi

    software::ensure_homebrew

    not_installed=()
    for i in $@; do
        if ! exists_command $i; then
            not_installed+=(${i})
        fi
    done
    if [[ ${#not_installed} != 0 ]]; then
        brew install ${not_installed[@]}
    fi
}

function software::beancount() {
    software::ensure_pip3

    pip3 install fava
}

function programming::bash_lsp() {
    software::ensure_npm
    if ! exists_command bash-language-server; then
        npm i -g bash-language-server
    fi
}

function software::ensure() {
    os=`uname`
    case $os in
        Darwin)
            software::ensure_by_brew $@
            ;;
        *)
            echo support macOS only
            exit 1
            ;;
    esac
}
