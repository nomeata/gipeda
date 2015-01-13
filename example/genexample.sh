#!/bin/bash

set -e

ln -sf ../.git repository
ln -sf ../gipeda gipeda
ln -sf ../src src

./genlogs.sh

mkdir -p site/js
../install-jslibs.sh

cp ../site/index.html site/index.html
cp ../site/js/gipeda.js site/js/gipeda.js

./gipeda
