#!/usr/bin/env bash

set -e

# TODO cd here

for t in user system; do
    for s in `ls $t/*.service`; do
        ln -sf `pwd`/$s /lib/systemd/$s
	echo `pwd`/$s /lib/systemd/$s
    done
done

echo Manual:
echo "dnsmasq using default service"
echo "ssss.config in Dropbox/dotfile"
