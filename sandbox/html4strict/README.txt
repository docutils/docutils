.. readme.txt: introduction to the html4strict writer   -*- rst-mode -*-
.. sectnum::

=====================================================
            The XHTML 1.1 Writer
=====================================================

:Author: Günter Milde
:Date: $Date$
:Abstract: A HTML writer, generating `XHTML 1.1` for styling
           with CSS 2.1.

.. contents::

Introduction
============

State of the art
----------------

  Docutils' default HTML Writer, ``docutils.writers.html4css1`` generates
  output that conforms to the HTML 4.01 Transitional DTD and to the
  Extensible HTML version 1.0 Transitional DTD (*almost* strict).

*Almost*, as it contains some deprecated constructs and "a minimum of
formatting information" in order to ensure correct display with deficient
but (at the time of creation) widespread browsers (mainly IE6).

A new HTML5 writer with most features of this writer may become part of
Docutils 0.13.

Objective
----------

Goals of the `xhtml11 writer`:

* Strict standards compliance.

* Generate good looking, readable, and accessible documents.

* Clear distinction of content and layout:

  + Clean HTML output without "hard-coded" visual markup,

  + extended configurability by CSS style sheets.

* `Graceful Degradation
  <http://www.anybrowser.org/campaign/abdesign.html#degradability>`__

* Best viewed with any (CSS2-conforming) HTML browser. [#]_

* Support scientific documents (numbering tables and figures, formal
  tables, ...). Cf. [markschenk]_.


.. [#] Tested with Firefox_, Midori_, Konqueror_ and Opera_. As Safari
   and Google Chrome use the same rendering engine as Midori and
   Konqueror (WebKit), they should work fine as well.

.. _firefox: http://www.mozilla.com
.. _opera: http://www.opera.com
.. _midori: http://www.twotoasts.de/index.php?/pages/midori_summary.html
.. _konqueror: http://konqueror.kde.org/

Audience
--------

This writer is for you, if you

* care much about standards compliance,

* care less about the rendering in non-compliant browsers,

* want extended CSS configurability.


Usage
=====

The `<rst2xhtml.py>`_ front end reads standalone reStructuredText
source files and produces clean `XHTML 1.1`_
output. A CSS 2 stylesheet is required for proper rendering; a complete
sample stylesheet is provided.

:Reader: Standalone
:Parser: reStructuredText
:Writer: xhtml (xhtml11)

The front end can be called from the command line (when it is installed in
the BINARY PATH)::

  rst2xhtml.py [options] [<source> [<destination>]]

The full usage text can be obtained with the ``--help`` option.

The front end `rst2xhtml.py`_ is also an example of programmatic use.


Implementation
==============

The writer module subclasses the ``html_plain.Writer`` and
``html_plain.HTMLTranslator`` classes and adds compatibility to the strict
requirements of `XHTML 1.1`_:

* There is no attribute "lang" (only "xml:lang").

* Enumerated lists don't support the 'start' attribute.

  The style sheet xhtml11.css_ adds support for a "start" value for
  enumerated lists via a CSS-counter. This allows also nested enumeration.

* ``<sup>`` and ``<sub>`` tags are not allowed in preformatted blocks
  (``<pre>``).

The `math-output` configuration setting defaults to "MathML".

The `xhtml11.css <xhtml11/xhtml11.css>`_ style sheet extends the standard
layout for CSS2-conforming HTML browsers.


Changes to the html4css1 writer
-------------------------------

Document type
~~~~~~~~~~~~~

+ The output conforms to the XHTML version 1.1 DTD::

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
     '"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">

  or XHTML + MathML::

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN"
    "http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd">


Docinfo and field lists based on definition lists (instead of tables)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+ Reduced loading time for documents with long field lists.

+ Enables CSS styling for:

  - label width (obsoleting the ``--field-name-limit`` option)
  - handling of long labels: truncate, wrap, ...
  - label separator (default: ':')
  - compact vs. open list

Class arguments for docinfo items
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Items in the docinfo list are passed class arguments specifying
their type to enable customizing the docinfo layout.

The default style sheet contains example definitions: author and date
are typeset centered and without label, if they occur as first docinfo
fields.


Footnotes and citations
~~~~~~~~~~~~~~~~~~~~~~~

+ Typeset as CSS-styled definition lists.

+ Collect adjacent footnotes/citations in one list.

Counter for enumerated lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A CSS counter for `enumerated lists`_ replaces the deprecated "start"
attribute.

.. _enumerated lists:
   ../../docutils/docs/ref/rst/restructuredtext.html#enumerated-lists

+ Enables CSS styling for:

  - label style (including nested numbers),
  - label separator.

The complicated part was to find out a correct CSS rule-set to replicate the
standard behaviour with hanging indent (list-style: "outside"). There is a
`W3C example`_ to number nested list items, however, the result is similar
to 'list-style: inside': subsequent lines start below the label instead of a
hanging indent.

Most Internet resources come to the conclusion that "there’s no
straightforward replacement mechanism" [tekkie]_, "the solution is
buried so deep in CSS2 that there's no point in trying to do it in CSS
for the foreseeable future" [webjunction]_, or "the main point to note
is that there is no direct mapping from the previous behaviour to CSS"
[codelair]_.  `Taming Lists`_ did give valuable advise but no working
complete solution.

The common advise is "Use 'HTML 4.01 Transitional' and keep the START
attribute". [highdots]_, especially, since "There are arguments over
whether the start attribute is presentational or not, and indeed HTML5
has declared that it is no longer deprecated in the current working
drafts (at the time of writing)" [dev.opera]_.

However, a reasonable replacement of 'outside'-styled ordered lists
with CSS is possible:

* The ordered list defines/resets the counter, the automatic numbering
  is suppressed::

    ol {
      counter-reset: item;
      list-style-type: none  ! important;
    }

* The label is defined as "before" pseudo element. The content consists
  of the counter and a separator (by default a trailing dot)::

    ol > li:before {
      counter-increment: item;
      content: counter(item) ".";
    }

* The label is right aligned in a box. Both the label and the list
  content (which Docutils puts in a paragraph node) must be displayed
  as "inline-block" so that they line up::

    ol > li:before {
      display: inline-block;
      vertical-align: top;
      width: 2em;
      padding-right: 0.5em;
      text-align: right;
    }

    ol > li > p { display: inline-block; }

  However, subsequent paragraphs are to be set as nested block
  elements::

    ol > li > p + p {
      display: block;
      margin-top: 0em;
    }

* The hanging indent is realized via a negative "textindent"
  which must be reset for the list content to prevent over-striking::

    ol > li { text-indent: -2.5em; }
    ol > li > p { text-indent: 0em; }

The resulting list can be customized to a large extend

* Different label types and separators are possible, e.g.::

    ol.lowerroman  > li:before {
      content: "(" counter(item, lower-roman) ")";
    }

* nested counters (1, 1.1, 1.1.1, etc)::

    ol.nested > li:before {
      content: counters(item, ".") ". ";
    }

* chapter/section prefix, continued lists, ...

.. _W3C example: http://www.w3.org/TR/CSS2/generate.html#counters
.. _taming lists: http://www.alistapart.com/articles/taminglists/



Inline literal role with ``pre-wrap``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In contrast to the html4css1 writer, runs of whitespace are not
replaced by ``&nbsp;`` entities (cf. bug #1938891).

Whitespace-handling and wrapping are configured with the CSS
property ``white-space: pre-wrap``:

  Whitespace is preserved by the browser. Text will wrap when
  necessary, and on line breaks

However, most browsers wrap on non-word chars, too, if set to wrap
at white-space. Text like "--an-option"  or the regular expression
``[+]?(\d+(\.\d*)?|\.\d+)`` may be broken at the wrong place!
The setting ``white-space: pre;`` prevents this, but also
prevents wrapping at white space, contrary to the specification__

In order to allow line-wrap at whitespace only,
words-with-non-word-chars are wrapped in <span>s with class "pre".

+ simpler HTML code

+ White-space handling in inline literals configurable with the CSS
  stylesheet. Possible values: ``normal, nowrap, pre, pre-wrap,
  pre-line``.

__ http://docutils.sf.net/docs/ref/rst/restructuredtext.html#inline-literals


Table styling with CSS
~~~~~~~~~~~~~~~~~~~~~~

+ No hard-coded border setting.

+ Pre-defined table styles selected by class arguments "borderless"
  and "booktabs" matching the interpretation in the latex2e writer.

SimpleListChecker also checks field-lists and docinfo
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unified test if a list is compactable:

+ cleaner code

+ also works for nesting field-list in enumeration/bullet-list and
  vice versa

+ also test docinfo, as a field may contain more than one paragraph


Docutils-generated section numbers in a <span>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Instead of hard-coded formatting with trailing ``&nbsp;``,
section numbers in section headings and the toc are placed in spans
with ``class='sectnum'`` and separated from the heading by a CSS rule.

Omit redundant class arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Do not  mark the first child with 'class="first"' and the last
child with 'class="last"' in definitions, table cells, field
bodies, option descriptions, and list items. Use the
``:first-child`` and ``:last-child`` selectors instad.

Language attribute name changed to 'xml:lang'
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The name of the language attribute changed from 'lang' in XHTML 1.0 to
'xml:lang' in XHTML 1.1. Documents using 'lang' do not validate.


Do not omit <p> tags
~~~~~~~~~~~~~~~~~~~~

The HTML4CSS1 writer does this to "produce visually compact lists
(less vertical whitespace)". This writer relies on CSS rules
for"visual compactness".


TODO
----

* The first list in the test `2.3. Enumerated Lists` should be
  compact.

* Hanging indent for numbered section headings and ToC entries.

* search stylesheets along standard path if enclosed in <>
  (like the RST syntax for include files).

* Validate output with "critical" cases not covered by
  the functional test (e.g. headings with level > 6).

* Move widely supported constructs to the html4css1 writer.

* Number sections with CSS if sectnum_xform is False.

* Footnotes and Citations (for footnotes see
  http://www.archiva.net/footnote/index.htm and
  http://www.xmlplease.com/footnotes


References
==========

.. [cmdline-tool]
   `Inside A Docutils Command-Line Front-End Tool
   <http://docutils.sourceforge.net/docs/api/cmdline-tool.html>`_
.. [API]
   `API Reference Material for Client-Developers
   <http://docutils.sf.net/docs/index.html#api-api-reference-material-for-client-developers>`_
.. [ilovetypography]
   http://ilovetypography.com/2008/02/28/a-guide-to-web-typography/
.. [webtypography]
   http://webtypography.net/toc/
.. [tekkie]
   http://tekkie.flashbit.net/css/replacement-for-deprecated-ol-li-start-value-html-attributes,
   2009.
.. [webjunction]
   http://lists.webjunction.org/wjlists/web4lib/2001-September/026413.html,
   2001.
.. [codelair] http://www.doheth.co.uk/codelair/html-css/deprecated#start,
   2007.
.. [highdots]
   http://www.highdots.com/forums/cascading-style-sheets/using-css-set-start-number-262555.html,
   2008.
.. [dev.opera]
   http://dev.opera.com/articles/view/automatic-numbering-with-css-counters/,
   2008.
.. [markschenk] `Publishing scientific documents with XHTML and CSS
   <http://www.markschenk.com/cssexp/publication/article.xml>`__

`<../html4trans>`_
  is a similar sandbox project, a HTML writer producing XHTML that
  contains enough formatting information to be viewed without a
  cascading style sheet by a lightweight html browser
  (e.g. `Dillo <http://www.dillo.org>`__ or the console browser
  `elinks <http://elinks.cz>`__).

_`XHTML 1.1`
   `XHTML™ 1.1 - Module-based XHTML - Second Edition`,
   W3C Recommendation, 23 November 2010.
   http://www.w3.org/TR/xhtml11/

