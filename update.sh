#!/bin/sh

set -e

git pull
git submodule update --init

if [ ! -e modules/helm/helm-config.el ]; then
    (cd modules/helm/helm-config.el && make)
fi

(cd modules/clang-complete-async && make)
