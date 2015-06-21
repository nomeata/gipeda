// Main document
var data = {
    settings: {},
};
// Name of the current view
var view = 'index';
// Options of the current view
var viewData = {};

// Nonpersistent settings
var settings = {
    benchFilter: {
	improvements: true,
	boring: false,
	regressions: true,
    },
    collapsedGroups: [true, false, false, false],
    compare: {
	from: null,
	to: null,
    }
};

// Signals

viewChanged = new signals.Signal()
dataChanged = new signals.Signal()
settingsChanged = new signals.Signal()


// Routes

var routes = {
    index:
        { regex: /^$/,
          download: ['out/latest-summaries.json'],
          url: function () {return ""},
        },
    complete:
        { regex: /^all$/,
          download: ['out/all-summaries.json'],
          url: function () {return "all"},
        },
    graphIndex:
        { regex: /^graphs$/,
          download: ['out/benchNames.json', 'out/graph-summaries.json'],
          url: function () {return "graphs"},
        },
    revision:
        { regex: /^revision\/([a-f0-9]+)$/,
          viewData: function (match) { return { hash: match[1] }; },
          download: function () {
            return ['out/benchNames.json','out/reports/' + viewData.hash + '.json'];
          },
          url: function (hash) { return "revision/" + hash; },
        },
    compare:
        { regex: /^compare\/([a-f0-9]+)\/([a-f0-9]+)$/,
          viewData: function (match) { return { hash1: match[1], hash2: match[2] }; },
          download: function () {
            return ['out/benchNames.json',
	            'out/reports/' + viewData.hash1 + '.json',
	            'out/reports/' + viewData.hash2 + '.json'
		    ];
          },
          url: function (hash1, hash2) { return "compare/" + hash1 + "/" + hash2; },
        },
    graph:
        { regex: /^graph\/(.*)$/,
          viewData: function (match, options) {
            var highlights = [];
            options.forEach(function (opt) {
                var m;
                if (m = /hl=(.*)/.exec(opt)) {
                    highlights.push(m[1]);
                }
            });
            return {
                benchName: match[1],
                highlights: highlights,
            };
          },
          download: function () {
            return ['out/latest-summaries.json','out/graphs/' + viewData.benchName + '.json'];
          },
          url: function (benchName, hls) {
            var comps = [ "graph/" + benchName ];
            $.merge(comps, hls.map(function (hl) { return "hl=" + hl }));
            return comps.join(';');
          },
        },
};

function parseRoute (path) {
    var options = path.split(";");
    var routePath = options.shift();

    $.each(routes, function (v,r) {
        var match = r.regex.exec(routePath);
        if (match) {
            view = v;
            if (r.viewData) viewData = r.viewData(match, options);
            return false;
        } else {
            return true;
        }
    });
    viewChanged.dispatch();
}

viewChanged.add(function () {
    if (routes[view].download) {
        d = routes[view].download;
        if ($.isFunction(d)) {d = d()}
        d.forEach(function (url) { getJSON(url) });
    }
});

function handleHashChange(newHash) {
    parseRoute(newHash);
}

// Data

function commitsFrom(hash, count) {
  revs = data.revisions;

  // Can happen during loading
  if (!revs) {return []};

  if (count == 0) return [];

  var rev = revs[hash];
  if (rev) {
    if (rev.summary.parents.length > 0) {
      var later = commitsFrom(rev.summary.parents[0], count - 1);
      later.unshift(rev);
      return later;
    } else {
      return [ rev ];
    }
  } else {
    return []
  }
}


// Template handling
/*
var templates = {};

$(function ()  {
  var template_ids =  ["revision", "compare", "index", "complete", "graphIndex", "graph", "revTooltip"];
  template_ids.forEach(function(id) {
    var source = $("#" + id).html();
    templates[id] = Handlebars.compile(source);
  });

  var partials_ids =  ["nav", "summary-icons", "summary-list", "rev-id", "nothing", "tags"];
  partials_ids.forEach(function(id) {
    var source = $("#" + id).html();
    Handlebars.registerPartial(id, source);
  });

});
*/

// Template helpers

function revisionLink(hash) {
  if (!hash) { return "#"; }
  return "#" + routes.revision.url(hash);
}
function compareLink(hash1,hash2) {
  if (!hash1) { return "#"; }
  if (!hash2) { return "#"; }
  return "#" + routes.compare.url(hash1,hash2);
}
function graphLink(benchName, hl1, hl2) {
    hls = [];
    if (hl1 && typeof(hl1) == 'string') {hls.push(hl1)};
    if (hl2 && typeof(hl2) == 'string') {hls.push(hl2)};
    return "#" + routes.graph.url(benchName,hls);
}
function diffLink(rev1, rev2) {
    return "TODO"; /* Handlebars.compile(data.settings.diffLink)({base: rev1, rev: rev2}); */
}
function logLink(rev, options) {
  if (data.settings.logLink) {
    var link = Handlebars.compile(data.settings.logLink)({rev: rev});
    $.extend(this,{link:link});
    return options.fn(this);
  }
}
function indexLink() {
  return "#" + routes.index.url();
}
function allLink() {
  return "#" + routes.complete.url();
}
function graphIndexLink() {
  return "#" + routes.graphIndex.url();
}
function recentCommits() {
  return commitsFrom(data.latest, data.settings.limitRecent);
}
function allCommits() {
  return commitsFrom(data.latest, -1);
}
function shortRev(rev) {
  if (!rev) { return ''; }
  return rev.substr(0,7);
}
function iso8601(timestamp) {
  if (!timestamp) { return '' };
  return new Date(timestamp*1000).toISOString();
}
function humanDate(timestamp) {
  return new Date(timestamp*1000).toString();
}

// We cache everything
var jsonSeen = {};
var jsonFetching = {};
function getJSON(url, callback, options) {
    var opts = {
        block: true,
    };
    $.extend(opts, options);
    if (jsonSeen[url]) {
	console.log("Not fetching "+url+" again.");
	if (callback) callback();
    } else if (jsonFetching[url]) {
	console.log("Already fetching "+url+".");
	if (callback) jsonFetching[url].push(callback);
    } else {
	console.log("Fetching "+url+".");
	jsonFetching[url] = [];
	if (callback) jsonFetching[url].push(callback);
        $.ajax(url, {
            success: function (newdata) {
		console.log("Fetched "+url+".");
	        jsonSeen[url] = true;
	        $.extend(true, data, newdata);
	        dataChanged.dispatch();
		$.each(jsonFetching[url], function (i,c) {c()});
		delete jsonFetching[url];
            },
            error: function (e) {
                console.log("Failure fetching "+url,e);
            },
	    cache: false,
            dataType: 'json',
        });
    }
}


// Views

function groupStats (benchResults) {
  return {
    totalCount: benchResults.length,
    improvementCount: benchResults.filter(function (br) { return br.changeType == 'Improvement' }).length,
    regressionCount:  benchResults.filter(function (br) { return br.changeType == 'Regression' }).length,
  }
}

function benchmark_name_matches(pattern, name) {
    if (pattern[pattern.length-1] == "*") {
        return pattern.substr(0, pattern.length-1) == name.substr(0, pattern.length-1);
    } else {
        return pattern == name;
    }
}

// The following logic should be kept in sync with BenchmarkSettings.hs
function setting_for(name) {
    var benchSettings = {
        smallerIsBetter: true,
        unit: "",
        type: "integral",
        group: "",
        threshold: 3,
        important: true
    };

    data.settings.benchmarks.map(function (s) {
        if (benchmark_name_matches(s.match, name)) {
            $.extend(benchSettings, s);
        }
    })

    return benchSettings;
}


// The following logic should be kept in sync with toResult in ReportTypes.hs
function compareResults (res1, res2) {
    if (!res1 && !res2) { return };

    name = res1? res1.name : res2.name;
    s = setting_for(name);

    res = {
        name: name,
        previous:    res1 ? res1.value : null,
        value:       res2 ? res2.value : null,
        unit:        s.unit,
        important:   s.important,
        changeType: "Boring",
        change: "foobar",
    };

    if (res1 && res2){
        if (s.type == "integral" || s.type == "float"){
            if (res1.value == 0 && res2.value == 0) {
                res.change = "=";
            } else if (res1.value == 0) {
                res.change = "+  ∞";
                res.changeType = "Improvement";
            } else {
                var perc = 100.0 * (res2.value - res1.value) / res1.value;
                var percS = Math.round (perc * 100.0) / 100;
                if (Math.abs(perc) < 0.01) {
                    res.change = "=";
                } else if (perc >= 0) {
                    res.change = "+ " + percS + "%";
                } else {
                    res.change = "- " + (-percS) + "%";
                }

                if (Math.abs(perc) >= s.threshold) {
                    if (perc >= 0) {
                        s.changeType = "Improvement";
                    } else {
                        s.changeType = "Regression";
                    }
                }
            }
        } else if (s.type == "small integral") {
            if (res1.value == res2.value) {
                res.change = "=";
            } else if (res2.value > res1.value) {
                res.change = "+" + (res2.value - res1.value);
                res.changeType = "Improvement";
            } else if (res1.value > res2.value) {
                res.change = "-" + (res1.value - res2.value);
                res.changeType = "Regression";
            }
        }

        if (s.smallerIsBetter) {
            if (res.changeType == "Improvement") {
                res.changeType = "Regression";
            } else if (res.changeType == "Regression") {
                res.changeType = "Improvement";
            }
        }
    }

    return res;
}

// Some views require the data to be prepared in ways that are 
// too complicated for the template, so lets do it here.
dataViewPrepare = {
  'revision': function (data, viewData) {
    if (!data.benchGroups || !data.revisions) return {};
    var hash = viewData.hash;
    var rev = data.revisions[hash];
    if (!rev) return {};
    if (!rev.benchResults) return {};

    var groups = data.benchGroups.map(function (group) {
      var benchmarks = group.groupMembers.map(function (bn) {
	return rev.benchResults[bn]
      }).filter(function (br) {return br});
      return {
	groupName: group.groupName,
	benchResults: benchmarks,
	groupStats: groupStats(benchmarks),
      };
    });
    return {
      rev : rev,
      groups : groups,
    };
  },
  'compare': function (data, viewData) {
    if (!data.benchGroups || !data.revisions) return {};
    var hash1 = viewData.hash1;
    var hash2 = viewData.hash2;
    var rev1 = data.revisions[hash1];
    var rev2 = data.revisions[hash2];
    if (!rev1) return {};
    if (!rev1.benchResults) return {};
    if (!rev2) return {};
    if (!rev2.benchResults) return {};

    var groups = data.benchGroups.map(function (group) {
      var benchmarks = group.groupMembers.map(function (bn) {
	var r1 = rev1.benchResults[bn];
	var r2 = rev2.benchResults[bn];
	return compareResults(r1,r2);
      }).filter(function (br) {return br});
      return {
	groupName: group.groupName,
	benchResults: benchmarks,
	groupStats: groupStats(benchmarks),
      };
    });
    return {
      rev1 : rev1,
      rev2 : rev2,
      groups : groups,
    };
  },
}


function load_template () {
    console.log('Rebuilding page');
    var context = {};
    $.extend(context, data, viewData, settings);
    if (dataViewPrepare[view]){
      $.extend(context, dataViewPrepare[view](data, viewData));
    }
    $('#main').html(templates[view](context));

    $(".nav-loading").toggle(jQuery.active > 0);

    updateBenchFilter();
    updateCollapsedGroups();
    $('abbrv.timeago').timeago();


    if ($('#benchChart').length) {
	setupChart();
    }
}
// viewChanged.add(load_template);
// dataChanged.add(load_template);

function setupChart () {

    var commits = commitsFrom(data.latest, data.settings.limitRecent);
    var benchName = viewData.benchName;

    $("<div id='tooltip' className='panel alert-info'></div>").css({
		position: "absolute",
		display: "none",
		//border: "1px solid #fdd",
		padding: "2px",
		//"background-color": "#fee",
		// opacity: 0.80,
                width: '300px',
	}).appendTo("#main");

    var plot_series = {
	  lines: { show: true, fill: true, fillColor: "rgba(255, 255, 255, 0.8)" },
	  points: { show: true, fill: false },
	  label: benchName,
	  data: commits.map(function (x,i){
	    if (!x.benchResults) return;
	    if (!x.benchResults[benchName]) return;
	    return [commits.length - i, x.benchResults[benchName].value]
	  }),
	};

    var plot_options = {
	  legend: {
		position: 'nw',
	  },
	  grid: {
		hoverable: true,
		clickable: true,
	  },
          yaxis: {},
	  xaxis: {
		// ticks: values.map(function (x,i){return [i,x[0]]}),
		tickFormatter: function (i,axis) {
			if (i > 0 && i <= commits.length) {
				var rev = commits[commits.length - i];
                                if (!rev) return "";
                                var date = new Date(rev.summary.gitDate * 1000);
                                return $.timeago(date);
			} else {
				return '';
			}
		}
	  }
	};

    var numberType;
    if (data.benchmarkSettings && data.benchmarkSettings[benchName]) {
        numberType = data.benchmarkSettings[benchName].numberType;
    }
    if (numberType == "integral" || numberType == "small integral") {
        plot_options.yaxis.minTickSize = 1;
        plot_options.yaxis.tickDecimals = 0;
    }

    var plot = $.plot("#benchChart", [plot_series], plot_options);

    viewData.highlights.forEach(function (hash) {
        commits.forEach(function (rev,i) {
            if (rev.summary.hash == hash)  {
                plot.highlight(0, i);
            };
        });
    });


    $("#benchChart").bind("plothover", function (event, pos, item) {
	if (item) {
		var v = item.datapoint[1];
		var i = item.dataIndex;
                var rev = commits[i].summary.hash;
                var summary = commits[i].summary;

		var tooltipContext = $.extend({value: v}, summary);

                if ($("#tooltip").data('rev') != rev) {
                    $("#tooltip")
			.html(shortRev(rev))
			.data('rev',rev)
		        .html(templates.revTooltip(tooltipContext))
                        .fadeIn(200)
                        .css({top: item.pageY+10, left: item.pageX-100})
                        .show();
                    $("#benchChart").css('cursor','pointer');
                }
	} else {
		$("#tooltip").data('rev',null).hide();
		$("#benchChart").css('cursor','default');
	}
    });
    $("#benchChart").bind("plotclick", function (event, pos, item) {
	if (item) {
		var v = item.datapoint[1];
		var i = item.dataIndex;
                var hash = commits[i].summary.hash;
		
		goTo(routes.revision.url(hash));
	}
    });
}

function updateCollapsedGroups() {
    // Does not work yet...
    /*

    var list = settings.collapsedGroups;
    console.log(list);
    if ($(".panel-collapse").length) {
	$(".panel-collapse").each(function (i){
	    if (i < list.length) {
		console.log(i, list[i], this);
		$(this).toggleClass('in', !list[i]);

		$(this).collapse();
		if (list[i]) {
		    $(this).collapse('hide');
		} else {
		    $(this).collapse('show');
		}
	    }
	});
    }
    */
}

function updateBenchFilter() {
    var showRegressions  = settings.benchFilter.regressions;
    var showBoring       = settings.benchFilter.boring;
    var showImprovements = settings.benchFilter.improvements;

    $('#show-regressions').toggleClass('active', showRegressions);
    $('#show-boring').toggleClass('active', showBoring); 
    $('#show-improvements').toggleClass('active', showImprovements);

    $('tr.row-Regression').toggle(showRegressions);
    $('tr.row-Boring').toggle(showBoring);
    $('tr.row-Improvement').toggle(showImprovements);

    $('.bench-panel').show().each(function() {
        if ($(this).has('tr.row-result:visible').length == 0) {
            $(this).hide();
        }
    });

    $('tr.summary-row').addClass('summary-row-collapsed');
    if (showBoring) {
        $('tr.summary-row:not(.summary-improvement):not(.summary-regression)')
            .removeClass('summary-row-collapsed');
    }
    if (showRegressions) {
        $('tr.summary-row.summary-regression')
            .removeClass('summary-row-collapsed');
    }
    if (showImprovements) {
        $('tr.summary-row.summary-improvement')
            .removeClass('summary-row-collapsed');
    }
    // Always show first entry in the history
    $('.summary-table tr.summary-row').first()
            .removeClass('summary-row-collapsed');

    $('.graph-list-panel').show().each(function() {
        if ($(this).has('tr.summary-row:visible').length == 0) {
            $(this).hide();
        }
    });

    updateNothingToSee();
};

function updateNothingToSee() {
    $('.nothing-to-see')
	.toggle(jQuery.active == 0 && $('.bench-panel:visible, tr.summary-row:not(.summary-row-collapsed)').length == 0);
}

$(function (){
    $('body').on('click', '.benchSelector', function (event) {
        $(this).toggleClass('active');
	settings.benchFilter = {
	    regressions:  $('#show-regressions').hasClass('active'),
	    boring:       $('#show-boring').hasClass('active'),
	    improvements: $('#show-improvements').hasClass('active'),
	};
        //updateBenchFilter();
        settingsChanged.dispatch()
    });
});

// Redirection

function goTo(path) {
    console.log("goTo " + path);
    hasher.setHash(path);
}


// The gipeda react app

var SummaryIcons = React.createClass({
    render: function() {
	return (
	  <span title={this.props.summaryDesc}>
	  <span>
	   {this.props.totalCount}{' '}
	   <span className="glyphicon glyphicon-stats"></span>{' '}
	  </span>
	  <span>
	   {this.props.improvementCount}{' '}
	   <span className="glyphicon glyphicon-plus text-success"></span>{' '}
	  </span>
	  <span>
	   {this.props.regressionCount}{' '}
	   <span className="glyphicon glyphicon-minus text-warning"></span>{' '}
	  </span>
	</span>);
    },
});

var RevisionSlug = React.createClass({
    render: function() {
	return <code data-rev={this.props.hash} className="rev-draggable">
	    {shortRev(this.props.hash)}
	</code>;
    },
    componentDidMount: function() {
	var elem = React.findDOMNode(this);
	$(elem).draggable({
	    revert: true,
	    revertDuration: 0,
	});
    }
});

var RevisionLink = React.createClass({
    render: function() {
	return <a href={revisionLink(this.props.hash)}>
	    <RevisionSlug hash={this.props.hash}/>
	</a>;
    },
});

var TimeAgo = React.createClass({
    render: function() {
	var date = this.props.date;
	var ago = jQuery.timeago(iso8601(date));
	return <abbrv title={humanDate(date)}>{ago}</abbrv>;
    },
    /*
    componentDidMount: function() {
	this.intervalID = window.setInterval(this.forceUpdate.bind(this), 1000);
    },
    componentWillUnmount: function() {
	clearInterval(this.intervalID);
    },
    */
})

var SummaryRow = React.createClass({
    render: function() {
	var hash = this.props.hash;
	var rev = data.revisions[hash];
	var first = this.props.listIndex == 0;

	var improvement = rev.summary.stats.improvementCount > 0;
	var regression = rev.summary.stats.regressionCount > 0;
	var boring = ! improvement && ! regression;

	var show_it =
	    improvement && settings.benchFilter.improvements ||
	    regression  && settings.benchFilter.regressions ||
	    boring      && settings.benchFilter.boring ||
	    first;

	return (<tr className={classNames('summary-row',
		{ 'summary-improvement': improvement,
		  'summary-regression': regression,
		  'summary-row-collapsed': !show_it})}>
	     <td className="col-md-2 text-right">
	       <TimeAgo date={rev.summary.gitDate}/>
	     </td>
	     <td className="col-md-1">
	      <RevisionLink hash={hash}/>
	     </td>
	     <td className="col-md-7">
	       { rev.summary.gitSubject }
	     </td>
	     <td className="col-md-2 text-right">
	      <SummaryIcons {...(rev.summary.stats)}/>
	     </td>
	    </tr>);
    },
});

var SummaryList = React.createClass({
    render: function() {
	return <div>
	    {/* <Nothing/> */}
	    <table className="table summary-table">
	      <tbody>
		{this.props.hashes.map(function(h,i) {
		    return <SummaryRow key={h} hash={h} listIndex={i}/>;
		})}
	      </tbody>
	    </table>
	</div>
    },
});

var TagRow = React.createClass({
    render: function() {
	var tag = this.props.tag;
	var hash = this.props.hash;
	var rev = data.revisions[hash];

	if (rev) {
	    return (<tr className="tag-row">
		 <td className="col-md-2 text-right">
		   <TimeAgo date={rev.summary.gitDate}/>
		 </td>
		 <td className="col-md-1">
		  <RevisionLink hash={hash}/>
		 </td>
		 <td className="col-md-2">
		   <strong>{ tag }</strong>
		 </td>
		 <td className="col-md-7">
		   { rev.summary.gitSubject }
		 </td>
		</tr>);
	} else {
	    return (<tr className="tag-row" title="This tag has not been benchmarked yet">
		 <td className="col-md-2 text-right">
		 </td>
		 <td className="col-md-1">
		  <RevisionLink hash={hash}/>
		 </td>
		 <td className="col-md-2">
		   { tag }
		 </td>
		 <td className="col-md-7">
		 </td>
		</tr>);
	}
    },
});


var TagList = React.createClass({
    render: function() {
	var tags = data.tags;
	if (!tags) { tags = [] };
	return <div>
	    <h2>Tags</h2>
	    <table className="table tag-table">
	      <tbody>
		{jQuery.map(tags, function(h,t) {
		    return <TagRow key={t} tag={t} hash={h}/>;
		})}
	      </tbody>
	    </table>
	</div>
    },
});


// The components for each route
views = {
    'index': React.createClass({
	render: function() {
	    return (<div>
	     <div className="container">
	      <h1>Recent commits</h1>
	      <SummaryList hashes={recentCommits().map(function (c){return c.summary.hash;})}/>
	      <TagList/>
	     </div>
	     <div className="container">
	      <p className="text-center">
		<a href={allLink()}>view older commits...</a>
	      </p>
	     </div>
	    </div>);
	}
    }),
    'revision': React.createClass({
	render: function() {
	    if (!data.benchGroups || !data.revisions) return null;
	    var hash = viewData.hash;
	    var rev = data.revisions[hash];
	    if (!rev) return null;
	    if (!rev.benchResults) return null;
	    if (!rev.summary) return null;

	    var groups = data.benchGroups.map(function (group) {
	      var benchmarks = group.groupMembers.map(function (bn) {
		return rev.benchResults[bn]
	      }).filter(function (br) {return br});
	      return {
		groupName: group.groupName,
		benchResults: benchmarks,
		groupStats: groupStats(benchmarks),
	      };
	    });

	    var parentInfo;
	    if (rev.summary.parents) {
		parentInfo = (<span>
		  Displaying changes since: <RevisionLink hash={rev.summary.parents[0]}/>
		  &nbsp;– &nbsp;
		  <a href={diffLink(rev.summary.parents[0], hash)}>View diff</a>
	        </span>);
	    } else {
		parentInfo = 'No parent commit found';
	    }

	    return (
	     <div className="container">
	      <div className="row">
	       <div className="col-md-6 col-md-push-6">
		<h2>
		 Commit {shortRev(rev.summary.hash)}
		</h2>
		<p>{parentInfo}&nbsp;–&nbsp;
		{/*
		{{#logLink rev.summary.hash }}
		  <a href="{{link}}">View buildlog</a> –
		{{/logLink}}
	        */}
		<SummaryIcons {...rev.summary.stats} />
		</p>
		<pre>{rev.gitLog}</pre>
	       </div>

	       <div className="col-md-6 col-md-pull-6">
	        {/*
		{{> nothing }}
		*/}
		<div className="panel-group" role="tablist">
		 {groups.map(function (g, i) { return (
		  <div key={g.groupName} className="panel panel-default bench-panel">
		   <div className="panel-heading" role="tab" id={'heading-' + i}>
		    <h4 className="panel-title">
		     <a className="accordion-toggle" data-toggle="collapse" href={'#table-' + i}>
		     { g.groupName }
		     <span className="stats pull-right">
		      <SummaryIcons {...(g.groupStats)}/>
		      <span className="indicator-toggled glyphicon glyphicon-chevron-down text-grey"/>
		      <span className="indicator-untoggled glyphicon glyphicon-chevron-right text-grey"/>
		     </span>
		     </a>
		    </h4>
		   </div>

		   <div id={'table-' + i} className="panel-collapse collapse in" role="tabpanel">
		    <div className="panel-body">
		     <table className="table table-condensed benchmark-table">
		      <thead>
		      <tr>
		      <th className="col-md-5">Benchmark name</th>
		      <th className="col-md-2 text-right">previous</th>
		      <th className="col-md-2 text-right">change</th>
		      <th className="col-md-2 text-right">now</th>
		      <th className="col-md-1 text-left"></th>
		      </tr>
		      </thead>
		      <tbody>
		       {g.benchResults.map(function (r) {
		        return (
			<tr key={r.name}
			  className={classNames("row-result","row-"+ r.changeType)}>
			 <td className="benchmark-name">
			  {r.name}
			  <a className="graph-link" title="Graphs" href={graphLink(r.name, hash)}>
			   <span className="glyphicon glyphicon-signal"/>
			  </a>
			 </td>
			 <td className="text-right">{r.previous}</td>
			 <td className="text-right">{r.change}</td>
			 <td className="text-right">{r.value}</td>
			 <td className="text-left">{r.unit}</td>
			</tr>
		       )})}
		      </tbody>
		     </table>
		    </div>
		   </div>
		  </div>
		 )})}
		</div>
	       </div>
	      </div>
	     </div>);
	},
    }),
};

// The navigation component

var BenchSelector = React.createClass({
    render: function() {
	var btnClasses = "btn btn-default navbar-btn benchSelector";
	return (
	    <div className="btn-group" role="group">
	     <button type="button" className={classNames(btnClasses, {active: settings.benchFilter.improvements})} id="show-improvements" title="Show improvements">
	      <span className="text-success">+</span>
	     </button>
	     <button type="button" className={classNames(btnClasses, {active: settings.benchFilter.boring})} id="show-boring" title="Show unchanged">
	      <span>=</span>
	     </button>
	     <button type="button" className={classNames(btnClasses, {active: settings.benchFilter.regressions})} id="show-regressions" title="Show regressions">
	      <span className="text-warning">-</span>
	     </button>
	    </div>
	);
    },
});

var DiffSelector = React.createClass({
    render: function() {
	var rev1 = settings.compare.from;
	var rev2 = settings.compare.to;

	var txt1 = rev1 ? shortRev(rev1) : '???????';
	var txt2 = rev2 ? shortRev(rev2) : '???????';

	var href = (rev1 && rev2) ?
	    "#" + routes.compare.url(rev1, rev2) :
	    'javascript:alert("Please drag two revisions here to compare them")';

	return (
	   <a id="go-to-compare" href={href} className="navbar-link">
	    <code>
	     <span title="Drop a revision id to compare two revisions" id="compare-from">{txt1}</span>..<span title="Drop a revision id to compare two revisions" id="compare-to">{txt2}</span>
	     </code>
	   </a>);
    },
    componentDidMount() {
	$('#compare-from').droppable({
	    accept: ".rev-draggable",
	    activeClass: "ui-state-highlight",
	    hoverClass: "ui-state-active",
	    drop: function( event, ui ) {
		settings.compare.from = ui.draggable.data('rev');
		settingsChanged.dispatch()
	    },
	});
	$('#compare-to').droppable({
	    accept: ".rev-draggable",
	    activeClass: "ui-state-highlight",
	    hoverClass: "ui-state-active",
	    drop: function( event, ui ) {
		settings.compare.to = ui.draggable.data('rev');
		settingsChanged.dispatch()
	    },
	});
    }
});

var Navigation = React.createClass({
    render: function() {
	return (
     <nav className="navbar navbar-default">
       <div className="container">

	{/* Inspired by http://stackoverflow.com/a/22978968/946226 */}
	{/* Title */}
	<div className="navbar-header pull-left">
	  <a className="navbar-brand" href="#">{ data.settings.title }</a>
	</div>

	{/* 'Sticky' (non-collapsing) right-side menu item(s) */}
	<div className="navbar-header pull-right">
	  <p className="navbar-text nav-loading pull-left">Loading data...</p>

	  <div className="navbar-text" role="group">
	    <DiffSelector />
	  </div>

	  <ul className="nav pull-left">
	   <li className="pull-left">
	    <BenchSelector/>
	   </li>
	  </ul>

	  {/* Required bootstrap placeholder for the collapsed menu */}
	  <button type="button" data-toggle="collapse" data-target=".navbar-collapse" className="navbar-toggle"><span className="sr-only">Toggle navigation</span><span className="icon-bar"></span><span className="icon-bar"></span><span className="icon-bar"></span></button>
	</div>

	<div className="collapse navbar-collapse navbar-left" id="bs-example-navbar-collapse-1">
	  <ul className="nav navbar-nav pull-right">
	    <li><a href={indexLink()}>Revisions</a></li>
	    <li><a href={graphIndexLink()}>Graphs</a></li>
	  </ul>

	</div>
      </div>
     </nav>
    )},
})

// The main component
var Gipeda = React.createClass({
    getInitialState: function() {
      return {view: view};
    },
    componentWillMount: function () {
	var component = this;
        viewChanged.add(function () {
	    component.setState({view: view});
        });
        dataChanged.add(function () {
	    component.forceUpdate();
        });
        settingsChanged.add(function () {
	    component.forceUpdate();
        });
    },
    render: function() {
	var View = views[this.state.view];
	if (View) {
	    return <div><Navigation/><View/></div>;
	} else {
	    return <p>Sorry, view '{this.state.view}' is not found.</p>;
	}
    },
});

// Main setup

$(function() {
    hasher.prependHash = '';

    console.log('Gipeda starting up...');

    $('#loading').hide();

    React.render(<Gipeda />, document.getElementById('main')
      );


    $(document).ajaxStart(function () {
	$(".nav-loading").show();
	updateNothingToSee();
    });
    $(document).ajaxStop(function () {
	$(".nav-loading").hide();
	updateNothingToSee();
    });

    // Load settins, then figure out what view to use.
    $.get("out/latest.txt", function (latest) {
        data.latest = latest;

        getJSON("out/settings.json", function (settings) {
	    $('title').html(data.settings.title + " – Gipeda");
            hasher.changed.add(handleHashChange);
            hasher.initialized.add(handleHashChange);
            hasher.init();
        });
    }, 'text');


});
