#!/bin/bash

set -e
set -x

mkdir -p site/js
cd site/js
test -e signals.min.js ||
	wget -c https://raw.githubusercontent.com/millermedeiros/js-signals/master/dist/signals.min.js
test -e hasher.min.js ||
	wget -c https://raw.githubusercontent.com/millermedeiros/Hasher/master/dist/js/hasher.min.js
test -e handlebars-v2.0.0.js || (
		wget -c http://builds.handlebarsjs.com.s3.amazonaws.com/handlebars-v2.0.0.js -O /tmp/handlebars-v2.0.0.js
		echo "082b97e40cd66456aec27431b9de35353de1fe71954f82fd32023f05cf11086e	/tmp/handlebars-v2.0.0.js" | sha256sum -c
		cp /tmp/handlebars-v2.0.0.js .
	)

test -e jquery-1.11.2.min.js ||
	wget -c https://code.jquery.com/jquery-1.11.2.min.js
test -d jquery-ui || {
        wget -c https://jqueryui.com/resources/download/jquery-ui-1.11.4.zip
	unzip jquery-ui-1.11.4.zip
	mv jquery-ui-1.11.4 jquery-ui
	rm -f jquery-ui-1.11.4.zip
	}
test -e jquery.timeago.js ||
	wget -c https://timeago.yarp.com/jquery.timeago.js
test -d flot || {
	wget -c http://www.flotcharts.org/downloads/flot-0.8.3.zip
	echo "ca33e0e707b0ac48f8962baaab321a006ef35cf73ea5bece95a30f23d49b9f31  flot-0.8.3.zip" | sha256sum -c
	unzip flot-0.8.3.zip
	rm -f flot-0.8.3.zip
}
test -d bootstrap || {
	mkdir bootstrap
	cd bootstrap
	wget -c https://github.com/twbs/bootstrap/releases/download/v3.3.1/bootstrap-3.3.1-dist.zip
	unzip bootstrap-3.3.1-dist.zip
	rm -f bootstrap-3.3.1-dist.zip
	cd ..
	}
test -e naturalSort.js ||
	wget -c https://raw.githubusercontent.com/overset/javascript-natural-sort/master/naturalSort.js
