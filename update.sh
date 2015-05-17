#!/bin/bash
lftp sftp://ghc-perf:xxx@www-origin.haskell.org -e 'mirror -c -e -R -x upload.sh -x ghc -x .git . ghc-perf/; exit'
