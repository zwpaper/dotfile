#!/usr/bin/env zsh

set -exuo pipefail

function log() {
    echo "$(date +'%Y-%m-%d %H:%M:%S%z') [INFO] - $@"
    if [[ -n "${LOG_FILE}" ]]; then
        echo "$(date +'%Y-%m-%d %H:%M:%S%z') [INFO] - $@" >> ${LOG_FILE}
    fi
}

function err() {
    echo "$(date +'%Y-%m-%d %H:%M:%S%z') [ERRO] - $@" >&2
    if [[ -n "${LOG_FILE}" ]]; then
        echo "$(date +'%Y-%m-%d %H:%M:%S%z') [ERRO] - $@" >> ${LOG_FILE}
    fi
}


function exists_command() {
    if type "$1" >./type.log 2>&1; then
        return 0
    else
        return 1
    fi
}

function exists_dir() {
    if [ -d $1 ]; then
        return 0
    else
        return 1
    fi
}
