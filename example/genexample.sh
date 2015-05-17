#!/bin/bash

set -e

test -e repository || ln -sf ../.git repository
test -e gipeda || ln -sf ../gipeda gipeda
test -e src || ln -sf ../src src

./genlogs.sh

mkdir -p site/js
../install-jslibs.sh

cp ../site/index.html site/index.html
cp ../site/js/gipeda.js site/js/gipeda.js

./gipeda

lftp sftp://ghc-perf:xxx@www-origin.haskell.org -e 'mirror -c -e -R site ghc-perf/gipeda; exit'
