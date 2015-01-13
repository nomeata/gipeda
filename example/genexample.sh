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

cd site

rm -rf .git
git init 
git remote add origin ../..
git fetch 
git branch master origin/gh-pages
git add .
git commit -m 'New gh-pages checkout'
git push origin HEAD:gh-pages
