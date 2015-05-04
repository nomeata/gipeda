#!/bin/bash

cd $(dirname $0)

git -C repository fetch origin "+refs/heads/*:refs/heads/*" --prune
./gipeda -j4
rsync -rvaiz --delete site/ deb.haskell.org:deb.haskell.org/speed/
