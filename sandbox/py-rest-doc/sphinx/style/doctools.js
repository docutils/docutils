/**
 * make the code below compatible with browsers without
 * an installed firebug like debugger
 */
if (!window.console || !console.firebug) {
  var names = ["log", "debug", "info", "warn", "error", "assert", "dir", "dirxml",
  "group", "groupEnd", "time", "timeEnd", "count", "trace", "profile", "profileEnd"];
  window.console = {};
  for (var i = 0; i < names.length; ++i) {
    window.console[names[i]] = function() {}
  }
}

/**
 * small helper function to urldecode strings
 */
jQuery.urldecode = function(x) {
  return decodeURIComponent(x).replace(/\+/g, ' ');
}

/**
 * small helper function to urlencode strings
 */
jQuery.urlencode = encodeURIComponent;

/**
 * This function returns the parsed url parameters of the
 * current request. Multiple values per key are supported,
 * it will always return arrays of strings for the value parts.
 */
jQuery.getQueryParameters = function() {
  var parts = document.location.search.substring(1).split('&');
  var result = {};
  for (var i = 0; i < parts.length; i++) {
    var tmp = parts[i].split('=', 2);
    var key = jQuery.urldecode(tmp[0]);
    var value = jQuery.urldecode(tmp[1]);
    if (key in result) {
      result[key].push(value);
    }
    else {
      result[key] = [value];
    }
  }
  return result;
}

/**
 * small function to check if an array contains
 * a given item.
 */
jQuery.contains = function(arr, item) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] == item) {
      return true;
    }
  }
  return false;
}

/**
 * highlight a given string on a jquery object by wrapping it in
 * span elements with the given class name.
 */
jQuery.fn.highlightText = function(text, className) {
  function highlight(node) {
    if (node.nodeType == 3) {
      var val = node.nodeValue;
      var pos = val.toLowerCase().indexOf(text);
      if (pos >= 0 && !jQuery.className.has(node.parentNode, className)) {
        var span = document.createElement("span");
        span.className = className;
        span.appendChild(document.createTextNode(val.substr(pos, text.length)));
        node.parentNode.insertBefore(span, node.parentNode.insertBefore(
          document.createTextNode(val.substr(pos + text.length)),
            node.nextSibling));
        node.nodeValue = val.substr(0, pos);
      }
    }
    else if (!jQuery(node).is("button, select, textarea")) {
      jQuery.each(node.childNodes, function() {
        highlight(this)
      });
    }
  }
  return this.each(function() {
    highlight(this);
  });
}

/**
 * helper function to return a node containing the
 * search summary for a given text. keywords is a list
 * of stemmed words, hlwords is the list of normal, unstemmed
 * words. the first one is used to find the occurance, the
 * latter for highlighting it.
 */
jQuery.makeSearchSummary = function(text, keywords, hlwords) {
  var textLower = text.toLowerCase();
  var start = 0;
  $.each(keywords, function() {
    var i = textLower.indexOf(this.toLowerCase());
    if (i > -1) {
      start = i;
    }
  });
  start = Math.max(start - 120, 0);
  var excerpt = ((start > 0) ? '...' : '') +
                $.trim(text.substr(start, 240)) +
                ((start + 240 - text.length) ? '...' : '');
  var rv = $('<div class="context"></div>').text(excerpt);
  $.each(hlwords, function() {
    rv = rv.highlightText(this, 'highlight');
  });
  return rv;
}

/**
 * Porter Stemmer
 */
var PorterStemmer = function() {

  var step2list = {
    ational: 'ate',
    tional: 'tion',
    enci: 'ence',
    anci: 'ance',
    izer: 'ize',
    bli: 'ble',
    alli: 'al',
    entli: 'ent',
    eli: 'e',
    ousli: 'ous',
    ization: 'ize',
    ation: 'ate',
    ator: 'ate',
    alism: 'al',
    iveness: 'ive',
    fulness: 'ful',
    ousness: 'ous',
    aliti: 'al',
    iviti: 'ive',
    biliti: 'ble',
    logi: 'log'
  };

  var step3list = {
    icate: 'ic',
    ative: '',
    alize: 'al',
    iciti: 'ic',
    ical: 'ic',
    ful: '',
    ness: ''
  };

  var c = "[^aeiou]";          // consonant
  var v = "[aeiouy]";          // vowel
  var C = c + "[^aeiouy]*";    // consonant sequence
  var V = v + "[aeiou]*";      // vowel sequence

  var mgr0 = "^(" + C + ")?" + V + C;                      // [C]VC... is m>0
  var meq1 = "^(" + C + ")?" + V + C + "(" + V + ")?$";    // [C]VC[V] is m=1
  var mgr1 = "^(" + C + ")?" + V + C + V + C;              // [C]VCVC... is m>1
  var s_v   = "^(" + C + ")?" + v;                        // vowel in stem

  this.stemWord = function (w) {
    var stem;
    var suffix;
    var firstch;
    var origword = w;

    if (w.length < 3) {
      return w;
    }

    var re;
    var re2;
    var re3;
    var re4;

    firstch = w.substr(0,1);
    if (firstch == "y") {
      w = firstch.toUpperCase() + w.substr(1);
    }

    // Step 1a
    re = /^(.+?)(ss|i)es$/;
    re2 = /^(.+?)([^s])s$/;

    if (re.test(w)) {
      w = w.replace(re,"$1$2");
    }
    else if (re2.test(w)) {
      w = w.replace(re2,"$1$2");
    }

    // Step 1b
    re = /^(.+?)eed$/;
    re2 = /^(.+?)(ed|ing)$/;
    if (re.test(w)) {
      var fp = re.exec(w);
      re = new RegExp(mgr0);
      if (re.test(fp[1])) {
        re = /.$/;
        w = w.replace(re,"");
      }
    }
    else if (re2.test(w)) {
      var fp = re2.exec(w);
      stem = fp[1];
      re2 = new RegExp(s_v);
      if (re2.test(stem)) {
        w = stem;
        re2 = /(at|bl|iz)$/;
        re3 = new RegExp("([^aeiouylsz])\\1$");
        re4 = new RegExp("^" + C + v + "[^aeiouwxy]$");
        if (re2.test(w)) {
          w = w + "e";
        }
        else if (re3.test(w)) {
          re = /.$/; w = w.replace(re,"");
        }
        else if (re4.test(w)) {
          w = w + "e";
        }
      }
    }

    // Step 1c
    re = /^(.+?)y$/;
    if (re.test(w)) {
      var fp = re.exec(w);
      stem = fp[1];
      re = new RegExp(s_v);
      if (re.test(stem)) { w = stem + "i"; }
    }

    // Step 2
    re = /^(.+?)(ational|tional|enci|anci|izer|bli|alli|entli|eli|ousli|ization|ation|ator|alism|iveness|fulness|ousness|aliti|iviti|biliti|logi)$/;
    if (re.test(w)) {
      var fp = re.exec(w);
      stem = fp[1];
      suffix = fp[2];
      re = new RegExp(mgr0);
      if (re.test(stem)) {
        w = stem + step2list[suffix];
      }
    }

    // Step 3
    re = /^(.+?)(icate|ative|alize|iciti|ical|ful|ness)$/;
    if (re.test(w)) {
      var fp = re.exec(w);
      stem = fp[1];
      suffix = fp[2];
      re = new RegExp(mgr0);
      if (re.test(stem)) {
        w = stem + step3list[suffix];
      }
    }

    // Step 4
    re = /^(.+?)(al|ance|ence|er|ic|able|ible|ant|ement|ment|ent|ou|ism|ate|iti|ous|ive|ize)$/;
    re2 = /^(.+?)(s|t)(ion)$/;
    if (re.test(w)) {
      var fp = re.exec(w);
      stem = fp[1];
      re = new RegExp(mgr1);
      if (re.test(stem)) {
        w = stem;
      }
    }
    else if (re2.test(w)) {
      var fp = re2.exec(w);
      stem = fp[1] + fp[2];
      re2 = new RegExp(mgr1);
      if (re2.test(stem)) {
        w = stem;
      }
    }

    // Step 5
    re = /^(.+?)e$/;
    if (re.test(w)) {
      var fp = re.exec(w);
      stem = fp[1];
      re = new RegExp(mgr1);
      re2 = new RegExp(meq1);
      re3 = new RegExp("^" + C + v + "[^aeiouwxy]$");
      if (re.test(stem) || (re2.test(stem) && !(re3.test(stem)))) {
        w = stem;
      }
    }
    re = /ll$/;
    re2 = new RegExp(mgr1);
    if (re.test(w) && re2.test(w)) {
      re = /.$/;
      w = w.replace(re,"");
    }

    // and turn initial Y back to y
    if (firstch == "y") {
      w = firstch.toLowerCase() + w.substr(1);
    }
    return w;
  }
}

/**
 * Small JavaScript module for the documentation.
 */
var Documentation = {

  init : function() {
    this.addContextElements();

    // fix firefox anchor bug
    if (document.location.hash && $.browser.mozilla) {
      window.setTimeout(function() {
        document.location.href += '';
      }, 10);
    }

    this.highlightSearchWords();

    // modindex toggle buttons
    $('img.toggler').click(function(){
            var src = $(this).attr('src');
            var idnum = $(this).attr('id').substr(7);
            $('tr.cg-'+idnum).toggle();
            if (src.substr(-9) == 'minus.png') {
                $(this).attr('src', src.substr(0, src.length-9) + 'plus.png');
            } else {
                $(this).attr('src', src.substr(0, src.length-8) + 'minus.png');
            }
        }).css('display', '').click();

    // inline comments
    $('.inlinecomments').hide();
    $('.commentmarker').css('cursor', 'pointer').click(function(){
            $(this).next().toggle();
        });
    $('.nocommentmarker').css('cursor', 'pointer').click(function(){
            // write comment 
        });
  },

  /**
   * highlight the search words provided in the url in the text
   */
  highlightSearchWords : function() {
    var params = $.getQueryParameters();
    var terms = (params.highlight) ? params.highlight[0].split(/\s+/) : [];
    if (terms.length) {
      var body = $('div.body');
      window.setTimeout(function() {
        $.each(terms, function() {
          body.highlightText(this.toLowerCase(), 'highlight');
        });
      }, 10);
      $('<li class="highlight-link"><a href="javascript:Documentation.' +
        'hideSearchWords()">Hide Search Matches</a></li>')
        .appendTo($('.sidebar .this-page-menu'));
    }
  },

  /**
   * helper function to hide the search marks again
   */
  hideSearchWords : function() {
    $('.sidebar .this-page-menu li.highlight-link').fadeOut(300);
    $('span.highlight').each(function() {
      this.className = '';
    });
  },

  /**
   * add context elements like header anchor links
   */
  addContextElements : function() {
    for (var i = 1; i <= 6; i++) {
      $('h' + i + '[@id]').each(function() {
        $('<a class="headerlink">\u00B6</a>').
          attr('href', '#' + this.id).
          attr('title', 'Permalink to this headline').
          appendTo(this);
      });
    }
    $('dt[@id]').each(function() {
      $('<a class="headerlink">\u00B6</a>').
          attr('href', '#' + this.id).
          attr('title', 'Permalink to this definition').
          appendTo(this);
    });
  },

  /**
   * Search Module
   */
  Search : {

    init : function() {
      var params = $.getQueryParameters();
      if (params.q) {
        var query = params.q[0];
        var areas = params.area || [];

        // auto default
        if (areas.length == 1 && areas[0] == 'default') {
          areas = ['tutorial', 'modules', 'install', 'distutils'];
        }

        // update input fields
        $('input[@type="checkbox"]').each(function() {
          this.checked = $.contains(areas, this.value);
        });
        $('input[@name="q"]')[0].value = query;

        this.performSearch(query, areas);
      }
    },

    /**
     * perform a search for something
     */
    performSearch : function(query, areas) {
      // create the required interface elements
      var out = $('#search-results');
      var title = $('<h2>Searching</h2>').appendTo(out);
      var dots = $('<span></span>').appendTo(title);
      var status = $('<p style="display: none"></p>').appendTo(out);
      var output = $('<ul class="search"/>').appendTo(out);

      // spawn a background runner for updating the dots
      // until the search has finished
      var pulseStatus = 0;
      function pulse() {
        pulseStatus = (pulseStatus + 1) % 4;
        var dotString = '';
        for (var i = 0; i < pulseStatus; i++) {
          dotString += '.';
        }
        dots.text(dotString);
        if (pulseStatus > -1) {
          window.setTimeout(pulse, 500);
        }
      };
      pulse();

      // stem the searchwords and add them to the
      // correct list
      var stemmer = new PorterStemmer();
      var searchwords = [];
      var excluded = [];
      var hlwords = [];
      var tmp = query.split(/\s+/);
      for (var i = 0; i < tmp.length; i++) {
        // stem the word
        var word = stemmer.stemWord(tmp[i]).toLowerCase();
        // select the correct list
        if (word[0] == '-') {
          var toAppend = excluded;
          word = word.substr(1);
        }
        else {
          var toAppend = searchwords;
          hlwords.push(tmp[i].toLowerCase());
        }
        // only add if not already in the list
        if (!$.contains(toAppend, word)) {
          toAppend.push(word);
        }
      };
      var highlightstring = '?highlight=' + $.urlencode(hlwords.join(" "));

      console.debug('SEARCH: searching for:');
      console.info('required: ', searchwords);
      console.info('excluded: ', excluded);
      console.info('areas:    ', areas);

      // fetch searchindex and perform search
      $.getJSON('searchindex.json', function(data) {

        // prepare search
        var filenames = data[0];
        var areaMap = data[1];
        var titles = data[2]
        var words = data[3];
        var fileMap = {};
        var files = null;

        // perform the search on the required words
        for (var i = 0; i < searchwords.length; i++) {
          var word = searchwords[i];
          // no match but word was a required one
          if ((files = words[word]) == null) {
            break;
          }
          // create the mapping
          for (var j = 0; j < files.length; j++) {
            var file = files[j];
            if (file in fileMap) {
              fileMap[file].push(word);
            }
            else {
              fileMap[file] = [word];
            }
          }
        }

        // now check if the files are in the correct
        // areas and if the don't contain excluded words
        var results = [];
        for (var file in fileMap) {

          // check if all requirements are matched
          if (fileMap[file].length != searchwords.length) {
            continue;
          }
          var valid = false;

          // check if the file is in one of the searched
          // areas.
          for (var i = 0; i < areas.length; i++) {
            if ($.contains(areaMap[areas[i]] || [], file)) {
              valid = true;
              break;
            }
          };

          // ensure that none of the excluded words is in the
          // search result.
          if (valid) {
            for (var i = 0; i < excluded.length; i++) {
              if ($.contains(words[excluded[i]] || [], file)) {
                valid = false;
                break;
              }
            }

            // if we have still a valid result we can add it
            // to the result list
            if (valid) {
              results.push([filenames[file], titles[file]]);
            }
          }
        }

        // delete unused variables in order to not waste
        // memory until list is retrieved completely
        delete filenames, areaMap, titles, words, data;

        // now sort the results by title
        results.sort(function(a, b) {
          var left = a[1].toLowerCase();
          var right = b[1].toLowerCase();
          return (left > right) ? -1 : ((left < right) ? 1 : 0);
        });

        // print the results
        var resultCount = results.length;
        function displayNextItem() {
          // results left, load the summary and display it
          if (results.length) {
            var item = results.pop();
            var listItem = $('<li style="display:none"></li>');
            listItem.append($('<a/>').attr('href', item[0] + '.html' +
                                           highlightstring).html(item[1]));
            $.get(item[0] + '.txt', function(data) {
              listItem.append($.makeSearchSummary(data, searchwords, hlwords));
              output.append(listItem);
              listItem.slideDown(10, function() {
                displayNextItem();
              });
            });
          }
          // search finished, update title and status message
          else {
            pulseStatus = -1;
            title.text('Search Results');
            if (!resultCount) {
              status.text('Your search did not match any documents. ' +
                          'Please make sure that all words are spelled ' +
                          'correctly and that you\'ve selected enough ' +
                          'categories.');
            }
            else {
              status.text('Search finished, found ' + resultCount +
                          ' page' + (resultCount != 1 ? 's' : '') +
                          ' matching the search query.');
            }
            status.fadeIn(500);
          }
        }
        displayNextItem();
      });
    }
  }
};

$(document).ready(function() {
  Documentation.init();
});
