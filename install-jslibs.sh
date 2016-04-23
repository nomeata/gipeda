#!/bin/bash

set -e
set -x

mkdir -p site/js
cd site/js
test -e signals.min.js ||
	wget -c https://raw.githubusercontent.com/millermedeiros/js-signals/master/dist/signals.min.js
test -e hasher.min.js ||
	wget -c https://raw.githubusercontent.com/millermedeiros/Hasher/master/dist/js/hasher.min.js
test -e handlebars-v2.0.0.js ||
	wget -c http://builds.handlebarsjs.com.s3.amazonaws.com/handlebars-v2.0.0.js
test -e jquery-1.11.2.min.js ||
	wget -c http://code.jquery.com/jquery-1.11.2.min.js
test -d jquery-ui || {
        wget -c http://jqueryui.com/resources/download/jquery-ui-1.11.4.zip
	unzip jquery-ui-1.11.4.zip
	mv jquery-ui-1.11.4 jquery-ui
	rm -f jquery-ui-1.11.4.zip
	}
test -e jquery.timeago.js ||
	wget -c http://timeago.yarp.com/jquery.timeago.js
test -d flot || {
	wget -c http://www.flotcharts.org/downloads/flot-0.8.3.zip
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
