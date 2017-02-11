#!/bin/bash

function say {
	echo
	echo "$@"
	echo
}

function run {
	echo "$@"
	"$@"
}

function runt {
	echo "$@"
	timeout -k 4h 3h "$@"
}

rev="$1"
if [ -z "$rev" ]
then
  echo "$0 <rev>"
fi

set -e


cd ~/logs/

echo Updating ~/all-repo-cache
git -C ~/all-repo-cache/ fetch --all --quiet

if [ -e "ghc-tmp-$rev" ]
then
	echo "ghc-tmp-$rev already exists"
	exit 1
fi

#logfile="$rev-$(date --iso=minutes).log"
logfile="$rev.log"
exec > >(sed -e "s/ghc-tmp-$rev/ghc-tmp-REV/g" | tee "$logfile".tmp)
exec 2>&1

set -o errtrace

function failure {
	test -f "$logfile".tmp || cd ..
	say "Failure..."
	run mv "$logfile".tmp "$logfile".broken
	run rm -rf "ghc-tmp-$rev"
}
trap failure ERR

say "Cloning"

run git clone --reference ~/all-repo-cache/ git://git.haskell.org/ghc "ghc-tmp-$rev"
cd "ghc-tmp-$rev"
run git checkout "$rev"

# Fixup 1
if git merge-base --is-ancestor  15b9bf4ba4ab47e6809bf2b3b36ec16e502aea72 $rev &&
   ! git merge-base --is-ancestor  d55a9b4fd5a3ce24b13311962bca66155b17a558 $rev
then
   echo "In range 15b9bf4..d55a9b4; applying patch d55a9b4"
   git cherry-pick d55a9b4fd5a3ce24b13311962bca66155b17a558
fi

git submodule update --reference ~/all-repo-cache/ --init

say "Identifying"

run git log -n 1

#say "Code stats"
#
#run ohcount compiler/
#
#run ohcount rts/
#
#run ohcount testsuite/

say "Booting"

runt perl boot

say "Configuring"

echo "Try to match validate settings"
echo 'GhcHcOpts  = ' >> mk/build.mk # no -Rghc-timing
echo 'GhcLibWays := $(filter v dyn,$(GhcLibWays))' >> mk/build.mk
echo 'GhcLibHcOpts += -O -dcore-lint'  >> mk/build.mk
echo 'GhcStage2HcOpts += -O -dcore-lint'  >> mk/build.mk

runt ./configure

say "Building"

runt /usr/bin/time -o buildtime make -j8 V=0
echo "Buildtime was:"
cat buildtime

say "Running the testsuite"

run make -C testsuite fast VERBOSE=4 THREADS=8

say "Running nofib"

runt make -C nofib boot
runt make -C nofib NoFibRuns=15

say "Total space used"

run du -sc .

say "Cleaning up"

run cd ..
run rm -rf "ghc-tmp-$rev"
run mv "$logfile".tmp "$logfile"
