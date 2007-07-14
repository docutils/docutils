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
            $('<li class="highlight-link"><a href="javascript:Document.' +
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
    }
};


$(document).ready(function() {
        Documentation.init();
    });
