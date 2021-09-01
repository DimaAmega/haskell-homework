#!/bin/bash

set -e

if [ -f $1 ]; then
    cat $1 | ghci $2 -fno-show-loaded-modules | sed \
    -e '/Ok, [A-Za-z0-9]* modules loaded./,$!d' \
    -e '/Ok, [A-Za-z0-9]* modules loaded./d' \
    -e 's/*[A-Z][a-z0-9]*> *//' \
    -e '$ d'
    exit 0
fi

echo "no such file $1"
exit 1
