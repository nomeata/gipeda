#!/bin/bash

scripts="$(realpath "$(dirname $0)")"
cd ~/logs/

# first=57ed4101687651ba3de59fb75355f4b83ffdca75
# first=853cdaea7f8724cd071f4fa7ad6c5377a2a8a6e4
first=052ec24412e285aa34911d6187cc2227fc7d86d9

while true
do
	git -C ghc-master fetch --prune
	git -C ghc-master pull
	for branchtip in $(git -C ghc-master for-each-ref --format='%(refname)')
	do
		if ! git -C ghc-master merge-base --is-ancestor $first $branchtip
		then
			continue
		fi

		if [ "$branchtip" = "refs/remotes/origin/wip/gadtpm" ]; then continue; fi
		if [ "$branchtip" = "refs/remotes/origin/wip/perf-testsuite" ]; then continue; fi
		if [ "$branchtip" = "refs/remotes/origin/wip/nfs-locking" ]; then continue; fi

		for rev in $(git -C ghc-master log --format=%H --first-parent $first..$branchtip | tac)
		do
			if git cat-file -e HEAD:$rev.log 2>/dev/null; then continue; fi
			if git cat-file -e HEAD:$rev.log.broken 2>/dev/null; then continue; fi
			if test -d "ghc-tmp-$rev"; then continue; fi

			date -R
			echo "Benchmarking $rev... (reachable from $branchtip)"
			$scripts/run-speed.sh "$rev" >/dev/null

			if [ -e $rev.log -a "$(wc -c < $rev.log)" -gt 10000000 ]
			then
				mv $rev.log $rev.log.broken
			fi
			if [ -e $rev.log.broken -a "$(wc -c < $rev.log.broken)" -gt 10000000 ]
			then
				truncate --size=<1M $rev.log.broken
				echo "Truncated too large log file" >> $rev.log.broken
			fi

			if [ -e $rev.log ]
			then
				git add $rev.log
				git commit -m "Log for $rev from $branchtip"
			else
				git add $rev.log.broken
				git commit -m "Log for $rev from $branchtip (failed)"
			fi
			git push
			git checkout --
			break 2
		done
	done
	echo "Sleeping..."
	sleep 60 || break
done
