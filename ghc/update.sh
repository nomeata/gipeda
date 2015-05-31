#!/bin/bash

cd $(dirname $0)

export PATH=~/.bin:"$PATH"

git -C repository fetch origin "+refs/heads/*:refs/heads/*" --prune
git -C logs fetch origin "+refs/heads/*:refs/heads/*" --prune
./gipeda -j4
lftp sftp://ghc-perf:xxx@www-origin.haskell.org -e 'mirror -c -e -R site/ ghc-perf/ghc; exit'
