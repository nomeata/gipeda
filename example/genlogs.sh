#!/bin/bash

if [ -e tmp ]
then
	echo "Please remove tmp/ first"
	exit 1
fi

mkdir -p logs

for hash in $(git -C repository log --format=%H)
do
	mkdir tmp/
	git -C repository archive $hash | tar -x -C tmp/
	ohcount tmp/ > logs/$hash.log
	rm -rf tmp
done
