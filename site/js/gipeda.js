// Main document
var data = {};
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
    collapsedGroups: [true, false, false, false]
};

// Signals

viewChanged = new signals.Signal()
dataChanged = new signals.Signal()


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
          download: ['out/benchNames.json'],
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

function commitsFrom(revs, hash, count) {
  // Can happen during loading
  if (!revs) {return []};

  if (count == 0) return [];

  var rev = revs[hash];
  if (rev) {
    if (rev.summary.parents.length > 0) {
      var later = commitsFrom(revs, rev.summary.parents[0], count - 1);
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
var templates = {};

$(function ()  {
  var template_ids =  ["revision", "index", "complete", "graphIndex", "graph", "revTooltip"];
  template_ids.forEach(function(id) {
    var source = $("#" + id).html();
    templates[id] = Handlebars.compile(source);
  });

  var partials_ids =  ["nav", "summary-icons", "summary-list", "nothing"];
  partials_ids.forEach(function(id) {
    var source = $("#" + id).html();
    Handlebars.registerPartial(id, source);
  });

});

Handlebars.registerHelper('revisionLink', function(hash) {
  if (!hash) { return "#"; }
  return "#" + routes.revision.url(hash);
});
Handlebars.registerHelper('graphLink', function(benchName, hl) {
    if (hl) {
        return "#" + routes.graph.url(benchName,[hl]);
    } else {
        return "#" + routes.graph.url(benchName);
    }
});
Handlebars.registerHelper('diffLink', function(rev1, rev2) {
  return data.settings.cgitLink + "/commitdiff/" + rev2
});
Handlebars.registerHelper('logLink', function(rev) {
  return Handlebars.compile(data.settings.logLink)({rev:rev});
});
Handlebars.registerHelper('indexLink', function() {
  return "#" + routes.index.url();
});
Handlebars.registerHelper('allLink', function() {
  return "#" + routes.complete.url();
});
Handlebars.registerHelper('graphIndexLink', function() {
  return "#" + routes.graphIndex.url();
});
Handlebars.registerHelper('recentCommits', function(revisions) {
  return commitsFrom(revisions, data.latest, data.settings.limitRecent);
});
Handlebars.registerHelper('allCommits', function(revisions) {
  return commitsFrom(revisions, data.latest, -1);
});
function shortRev(rev) {
  if (!rev) { return ''; }
  return rev.substr(0,7);
}
Handlebars.registerHelper('shortRev', shortRev);
Handlebars.registerHelper('iso8601', function(timestamp) {
  if (!timestamp) { return '' };
  return new Date(timestamp*1000).toISOString();	   
});
Handlebars.registerHelper('humanDate', function(timestamp) {
  return new Date(timestamp*1000).toString();
});

// We cache everything
var jsonSeen = {};
function getJSON(url, callback, options) {
    var opts = {
        block: true,
    };
    $.extend(opts, options);
    if (jsonSeen[url]) {
	console.log("Not fetching "+url+" again.");
	if (callback) callback();
    } else {
	console.log("Fetching "+url+".");
        $.ajax(url, {
            success: function (newdata) {
		console.log("Fetched "+url+".");
	        jsonSeen[url] = true;
	        $.extend(true, data, newdata);
	        dataChanged.dispatch();
	        if (callback) callback();
            },
            dataType: 'json'
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
viewChanged.add(load_template);
dataChanged.add(load_template);

function setupChart () {
    
    var commits = commitsFrom(data.revisions, data.latest, data.settings.limitRecent);
    var benchName = viewData.benchName;

    $("<div id='tooltip' class='panel alert-info'></div>").css({
		position: "absolute",
		display: "none",
		//border: "1px solid #fdd",
		padding: "2px",
		//"background-color": "#fee",
		// opacity: 0.80,
                width: '300px',
	}).appendTo("#main");

    var plot = $.plot("#benchChart",
	[{
	  lines: { show: true, fill: true, fillColor: "rgba(255, 255, 255, 0.8)" },
	  points: { show: true, fill: false },
	  label: benchName,
	  data: commits.map(function (x,i){
	    if (!x.benchResults) return;
	    if (!x.benchResults[benchName]) return;
	    return [commits.length - i, x.benchResults[benchName].value]
	  }),
	}],
	{	
	  legend: {
		position: 'nw',
	  },
	  grid: {
		hoverable: true,
		clickable: true,
	  },
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
	});

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
    $('tr.summary-row').first()
            .removeClass('summary-row-collapsed');

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
        updateBenchFilter();
    });
});

// Redirection

function goTo(path) {
    console.log("goTo " + path);
    hasher.setHash(path);
}


// Main setup

$(function() {
    hasher.prependHash = '';

    $('#loading').hide();

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
