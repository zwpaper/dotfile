#! /bin/bash

set -eu
set -o pipefail

go version

go get -u github.com/nsf/gocode
go get -u github.com/rogpeppe/godef
go get -u golang.org/x/tools/cmd/goimports
