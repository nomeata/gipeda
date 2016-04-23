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
    collapsedGroups: [true, false, false, false],
    compare: {
	from: null,
	to: null,
    }
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
  var template_ids =  ["revision", "compare", "index", "complete", "graphIndex", "graph", "revTooltip"];
  template_ids.forEach(function(id) {
    var source = $("#" + id).html();
    templates[id] = Handlebars.compile(source);
  });

  var partials_ids =  ["nav", "summary-icons", "summary-list", "rev-id", "nothing", "tags", "branches"];
  partials_ids.forEach(function(id) {
    var source = $("#" + id).html();
    Handlebars.registerPartial(id, source);
  });

});

Handlebars.registerHelper('revisionLink', function(hash) {
  if (!hash) { return "#"; }
  return "#" + routes.revision.url(hash);
});
Handlebars.registerHelper('compareLink', function(hash1,hash2) {
  if (!hash1) { return "#"; }
  if (!hash2) { return "#"; }
  return "#" + routes.compare.url(hash1,hash2);
});
Handlebars.registerHelper('graphLink', function(benchName, hl1, hl2) {
    hls = [];
    if (hl1 && typeof(hl1) == 'string') {hls.push(hl1)};
    if (hl2 && typeof(hl2) == 'string') {hls.push(hl2)};
    return "#" + routes.graph.url(benchName,hls);
});
Handlebars.registerHelper('diffLink', function(rev1, rev2) {
    if (data.settings.diffLink) {
	return Handlebars.compile(data.settings.diffLink)({base: rev1, rev: rev2});
    } else {
	return 'javascript:alert("No diffLink defined in settings")';
    }
});
Handlebars.registerHelper('revisionInfo', function(revSummary) {
    var ctxt = {};
    ctxt.rev = revSummary.hash;
    if (revSummary.parents) {
	ctxt.base=revSummary.parents[0];
    }
    var result = Handlebars.compile(data.settings.revisionInfo)(ctxt);
    return new Handlebars.SafeString(result);
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
Handlebars.registerHelper('id', function (text) {
    if (text) {
	lines = text.split(/\r?\n/);
	return new Handlebars.SafeString(lines.map(Handlebars.escapeExpression).join('&#10;'))
    }
});
Handlebars.registerHelper('iso8601', function(timestamp) {
  if (!timestamp) { return '' };
  return new Date(timestamp*1000).toISOString();
});
Handlebars.registerHelper('humanDate', function(timestamp) {
  return new Date(timestamp*1000).toString();
});
// inspired by http://stackoverflow.com/a/17935019/946226
Handlebars.registerHelper('each_naturally', function(context,options){
    var output = '';
    if (context) {
	var keys = jQuery.map(context, function(v,k) {return k});
	var sorted_keys = keys.sort(naturalSort);
	sorted_keys.map(function (k,i) {
	    output += options.fn(context[k], {data: {key: k, index: i}});
	});
    }
    return output;
});
Handlebars.registerHelper('each_unnaturally', function(context,options){
    var output = '';
    if (context) {
	var keys = jQuery.map(context, function(v,k) {return k});
	var sorted_keys = keys.sort(naturalSort).reverse();
	sorted_keys.map(function (k,i) {
	    output += options.fn(context[k], {data: {key: k, index: i}});
	});
    }
    return output;
});

// Sort by age, then by name
Handlebars.registerHelper('each_branch', function(context,options){
    var output = '';
    if (context) {
	jQuery.map(context, function (b,i) { return {branchData: b, branchName: i}; })
            .sort(function(a,b) {
                revA = data.revisions[a.branchData.branchHash];
                revB = data.revisions[b.branchData.branchHash];
	        if (revA && revB) {
                    return revB.summary.gitDate - revA.summary.gitDate;
                }
	        if (revA) {
                    return -1;
                }
	        if (revB) {
                    return 1;
                }
                return naturalSort(a.branchName, b.branchName);
            }).map(function (b,i) {
		var interesting = b.branchData.branchHash != b.branchData.mergeBaseHash;
                output += options.fn(b.branchData,
			{data: {key: b.branchName, index: i, interesting: interesting}});
            });
    }
    return output;
});

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

                var th_up = s.threshold;
                var th_down = (1-(1/(1+s.threshold/100)))*100;

                if (perc >= 0 && perc > th_up) {
                        res.changeType = "Improvement";
                }
                if (perc < 0 && -perc > th_down) {
                        res.changeType = "Regression";
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

function remember_from_to() {
    settings.compare.from = $('#compare-from').data('rev');
    settings.compare.to = $('#compare-to').data('rev');
}

function recall_from_to() {
    if (settings.compare.from) {
	$('#compare-from').data('rev', settings.compare.from);
	$('#compare-from').text(shortRev(settings.compare.from));
    }
    if (settings.compare.to) {
	$('#compare-to').data('rev', settings.compare.to);
	$('#compare-to').text(shortRev(settings.compare.to));
    }
    $('#go-to-compare').attr('href',current_compare_link());
}

function current_compare_link () {
    var rev1 = settings.compare.from;
    var rev2 = settings.compare.to;
    if (rev1 && rev2) {
	return "#" + routes.compare.url(rev1, rev2);
    } else {
	return 'javascript:alert("Please drag two revisions here to compare them")';
    }
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

    // Code to implement the compare-revision-drag'n'drop interface
    $('.rev-draggable').draggable({
        revert: true,
	revertDuration: 0,
    });
    $('#compare-from, #compare-to').droppable({
	accept: ".rev-draggable",
        activeClass: "ui-state-highlight",
        hoverClass: "ui-state-active",
        drop: function( event, ui ) {
          $( this ).text(ui.draggable.text());
          $( this ).data('rev',ui.draggable.data('rev'));
	  remember_from_to();
	  $('#go-to-compare').attr('href',current_compare_link());
      }
    });
    recall_from_to();

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
