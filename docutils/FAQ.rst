.. include:: docs/header0.rst

.. NOTE TO MAINTAINERS: Please add new questions to the end of their
   sections, so section/question numbers remain stable.

===========================================
 Docutils FAQ (Frequently Asked Questions)
===========================================

:Date: $Date$
:Revision: $Revision$
:Web site: https://docutils.sourceforge.io/
:Copyright: This document has been placed in the public domain.

.. contents::
.. sectnum::


This is a work in progress.  If you are reading a local copy, the
`master copy`_ might be newer.  This document uses relative links;
if they don't work, please use the master copy.

Please feel free to ask questions and/or provide answers; send email
to the `Docutils-users`_ mailing list.  Project members should feel
free to edit the source text file directly.

.. _master copy: https://docutils.sourceforge.io/FAQ.html
.. _let us know:
.. _Docutils-users: docs/user/mailing-lists.html#docutils-users



Docutils
========

What is Docutils?
-----------------

Docutils_ is a system for processing plaintext documentation into
useful formats, such as HTML, XML, and LaTeX.  It supports multiple
types of input, such as standalone files, `PEPs (Python Enhancement
Proposals)`_, and string input. Client code may add other input types,
e.g. Sphinx_ comes with an extension to extract inline documentation
from Python modules and packages.

The Docutils distribution consists of:

* a library (the "docutils" package), which `can be used by client
  code`_;
* several `front-end tools`_ (such as ``rst2html``, which converts
  reStructuredText input into HTML output);
* a `test suite`_; and
* documentation_.

For an overview of the Docutils project implementation,
see :PEP:`258`, "Docutils Design Specification".

Docutils is implemented in Python_.

.. _Docutils: https://docutils.sourceforge.io/
.. _PEPs (Python Enhancement Proposals):
   https://peps.python.org/pep-0012
.. _can be used by client code: docs/api/publisher.html
.. _front-end tools: docs/user/tools.html
.. _test suite: docs/dev/testing.html
.. _documentation: docs/index.html
.. _Python: https://www.python.org/


Why is it called "Docutils"?
----------------------------

Docutils is short for "Python Documentation Utilities".  The name
"Docutils" was inspired by "Distutils", the Python Distribution
Utilities architected by Greg Ward, a component of Python's standard
library.

The earliest known use of the term "docutils" in a Python context was
a `fleeting reference`__ in a message by Fred Drake on 1999-12-02 in
the Python Doc-SIG mailing list.  It was suggested `as a project
name`__ on 2000-11-27 on Doc-SIG, again by Fred Drake, in response to
a question from Tony "Tibs" Ibbs: "What do we want to *call* this
thing?".  This was shortly after David Goodger first `announced
reStructuredText`__ on Doc-SIG.

Tibs used the name "Docutils" for `his effort`__ "to document what the
Python docutils package should support, with a particular emphasis on
documentation strings".  Tibs joined the current project (and its
predecessors) and graciously donated the name.

For more history of reStructuredText and the Docutils project, see `An
Introduction to reStructuredText`_.

Please note that the name is "Docutils", not "DocUtils" or "Doc-Utils"
or any other variation.  It is pronounced as in "DOCumentation
UTILitieS", with emphasis on the first syllable.

.. _An Introduction to reStructuredText: docs/ref/rst/introduction.html
__ https://mail.python.org/pipermail/doc-sig/1999-December/000878.html
__ https://mail.python.org/pipermail/doc-sig/2000-November/001252.html
__ https://mail.python.org/pipermail/doc-sig/2000-November/001239.html
__ http://homepage.ntlworld.com/tibsnjoan/docutils/STpy.html


Is there a GUI authoring environment for Docutils?
--------------------------------------------------

See the links to editors_ supporting reStructuredText.

.. _editors: docs/user/links.html#editors


What is the status of the Docutils project?
-------------------------------------------

Docutils is mainly stable, with documented APIs and architecture
subject to change after announcement and a transition period.

We fix bugs as they are reported.  This means the code from the
repository_ (or the snapshots_) is the most stable as well as the most
featureful most of the time.


What is the Docutils project release policy?
--------------------------------------------

Besides the regular releases_, we provide automatically-generated
snapshots_ which always contain the latest code from the repository_.
As the project matures, we may formalize on a
stable/development-branch scheme, but we're not using anything like
that yet.

.. _releases: https://pypi.org/project/docutils/#history
.. _snapshots: https://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/
.. _repository: docs/dev/repository.html


How can I get a new feature into Docutils?
------------------------------------------

* Present your idea at the docutils-develop_ mailing list or file a
  ticket at Docutils' `feature request tracker`_.
  Convince the Docutils developers that this is a valuable addition.

* Contribute_.

* Be patient, and be persistent.  None of us are paid to do this,
  it's all in our spare time, which is precious and rare.

.. _docutils-develop: docs/user/mailing-lists.html#docutils-develop
.. _extensions and related projects:
   docs/dev/policies.html#extensions-and-related-projects
.. _feature request tracker:
    https://sourceforge.net/p/docutils/feature-requests/


reStructuredText
================

What is reStructuredText?
-------------------------

reStructuredText_ is an easy-to-read, what-you-see-is-what-you-get
plaintext markup syntax and parser system.  The reStructuredText
parser is a component of Docutils_.  reStructuredText is a revision
and reinterpretation of the StructuredText_ and Setext_ lightweight
markup systems.

If you are reading this on the web, you can see for yourself.  `The
source for this FAQ <FAQ.rst>`_ is written in reStructuredText; open
it in another window and compare them side by side.

`A ReStructuredText Primer`_ and the `Quick reStructuredText`_ user
reference are a good place to start.  The `reStructuredText Markup
Specification`_ is a detailed technical specification.

.. _A ReStructuredText Primer: docs/user/rst/quickstart.html
.. _Quick reStructuredText: docs/user/rst/quickref.html
.. _reStructuredText Markup Specification:
   docs/ref/rst/restructuredtext.html
.. _reStructuredText: https://docutils.sourceforge.io/rst.html
.. _StructuredText:
   http://dev.zope.org/Members/jim/StructuredTextWiki/FrontPage/
.. _Setext: https://docutils.sourceforge.io/mirror/setext.html


Why is it called "reStructuredText"?
------------------------------------

The name came from a combination of "StructuredText", one of
reStructuredText's predecessors, with "re": "revised", "reworked", and
"reinterpreted", and as in the ``re.py`` regular expression module.
For a detailed history of reStructuredText and the Docutils project,
see `An Introduction to reStructuredText`_.

"reStructuredText" is **ONE** word, *not two!*


What's the standard abbreviation for "reStructuredText"?
--------------------------------------------------------

"RST" (or "rST") and "ReST" (or "reST") are both acceptable.
Care should be taken with capitalization, to avoid confusion with
"REST__", an acronym for "Representational State Transfer".

__ https://en.wikipedia.org/wiki/Representational_State_Transfer


What's the standard filename extension for a reStructuredText file?
-------------------------------------------------------------------

The most commonly used extensions are ".rst" and ".txt".

Also see `What's the official MIME type for reStructuredText data?`_


Are there any reStructuredText editor extensions?
-------------------------------------------------

See the list of editors_ supporting reStructuredText.


How can I indicate the document title?  Subtitle?
-------------------------------------------------

A uniquely-adorned section title at the beginning of a document is
treated specially, as the document title.  Similarly, a
uniquely-adorned section title immediately after the document title
becomes the document subtitle.  For example::

    This is the Document Title
    ==========================

    This is the Document Subtitle
    -----------------------------

    Here's an ordinary paragraph.

See also `A reStructuredText Primer`__
and the `reStructuredText Markup Specification`__.

__ docs/user/rst/quickstart.html#document-title-subtitle
__ docs/ref/rst/restructuredtext.html#document-title

How can I represent esoteric characters (e.g. character entities) in a document?
--------------------------------------------------------------------------------

For example, say you want an em-dash (XML character entity &mdash;,
Unicode character U+2014) in your document.

ReStructuredText has no character entity subsystem. [#]_
However, Docutils uses Unicode so you use a literal character
(e.g. a "real" em-dash) for all charactes supported by the
`input encoding`_ (by default "UTF-8").

If you are restricted to 7-bit ASCII or a legacy encoding,
there is a workaround:
`Standard Substitution Definition Sets`_ provide equivalents of
XML & HTML character entity sets as substitution definitions. [#]_
For example, the Japanese yen currency symbol can be used as follows::

    .. include:: <xhtml1-lat1.txt>

    |yen| 600 for a complete meal?  That's cheap!

You can also create custom `substitution definitions`_ in your document
using the "unicode_" directive, e.g.::

    .. |--| unicode:: U+2013   .. en dash
    .. |---| unicode:: U+2014  .. em dash, trimming surrounding whitespace
       :trim:

.. |--| unicode:: U+2013   .. en dash
.. |---| unicode:: U+2014  .. em dash, trimming surrounding whitespace
   :trim:

Now you can write dashes using pure ASCII: "``foo |--| bar; foo |---|
bar``", rendered as "foo |--| bar; foo |---| bar".
The ``:trim:`` option for
the em dash is necessary because you cannot write "``foo|---|bar``";
thus you need to add spaces ("``foo |---| bar``") and advise the
reStructuredText parser to trim the spaces.

.. [#] reStructuredText doesn't know anything about XML character
   entities. To Docutils, "&mdash;" in input text is 7 discrete
   characters; no interpretation happens.  When writing HTML, the
   "&" is converted to "``&amp;``", so in the raw output you'd see
   "``&amp;mdash;``" and in the HTML browser "&mdash;".

.. [#] Thanks to David Priest for the original idea.

.. _input encoding: docs/user/config.html#input-encoding
.. _Standard Substitution Definition Sets: docs/ref/rst/definitions.html
.. _substitution definitions: docs/ref/rst/restructuredtext.html
                              #substitution-definitions
.. _unicode: docs/ref/rst/directives.html#unicode-character-codes


How can I generate backticks using a Scandinavian keyboard?
-----------------------------------------------------------

The use of backticks in reStructuredText is a bit awkward with
Scandinavian keyboards, where the backtick is a "dead" key.  To get
one ` character one must press SHIFT-` + SPACE.

Unfortunately, with all the variations out there, there's no way to
please everyone.  For Scandinavian programmers and technical writers,
this is not limited to reStructuredText but affects many languages and
environments.

Possible solutions include

* If you have to input a lot of backticks, simply type one in the
  normal/awkward way, select it, copy and then paste the rest (CTRL-V
  is a lot faster than SHIFT-` + SPACE).

* Use keyboard macros.

* Remap the keyboard.  The Scandinavian keyboard layout is awkward for
  other programming/technical characters too; for example, []{}
  etc. are a bit awkward compared to US keyboards.

  According to Axel Kollmorgen,

      Under Windows, you can use the `Microsoft Keyboard Layout Creator
      <http://www.microsoft.com/globaldev/tools/msklc.mspx>`__ to easily
      map the backtick key to a real backtick (no dead key). took me
      five minutes to load my default (german) keyboard layout, untick
      "Dead Key?" from the backtick key properties ("in all shift
      states"), "build dll and setup package", install the generated
      .msi, and add my custom keyboard layout via Control Panel >
      Regional and Language Options > Languages > Details > Add
      Keyboard layout (and setting it as default "when you start your
      computer").

* Use a virtual/screen keyboard or character palette, such as:

  - `Web-based keyboards <http://keyboard.lab.co.il/>`__ (IE only
    unfortunately).
  - Windows: `Click-N-Type <http://www.lakefolks.org/cnt/>`__.
  - Mac OS X: the Character Palette can store a set of favorite
    characters for easy input.  Open System Preferences,
    International, Input Menu tab, enable "Show input menu in menu
    bar", and be sure that Character Palette is enabled in the list.

If anyone knows of other/better solutions, please `let us know`_.


Are there any tools for HTML/XML-to-reStructuredText?  (Round-tripping)
-----------------------------------------------------------------------

People have tossed the idea around, and some implementations of
reStructuredText-generating tools can be found in the `Docutils Links`_.

There's no reason why reStructuredText should not be round-trippable
to/from XML; any technicalities which prevent round-tripping would be
considered bugs.  Whitespace would not be identical, but paragraphs
shouldn't suffer.  The tricky parts would be the smaller details, like
links and IDs and other bookkeeping.

For HTML, true round-tripping may not be possible.  Even adding lots
of extra "class" attributes may not be enough.  A "simple HTML" to RST
filter is possible -- for some definition of "simple HTML" -- but HTML
is used as dumb formatting so much that such a filter may not be
particularly useful.  An 80/20 approach should work though: build a
tool that does 80% of the work automatically, leaving the other 20%
for manual tweaks.

.. _Docutils Link List:
.. _Docutils Links: docs/user/links.html


Are there any Wikis that use reStructuredText syntax?
-----------------------------------------------------

Yes, see `Wikis`__ in the `Docutils Links`_.

__ docs/user/links.html#wikis



Are there any Weblog (Blog) projects that use reStructuredText syntax?
----------------------------------------------------------------------

With no implied endorsement or recommendation, and in no particular
order:

* `Firedrop <http://www.voidspace.org.uk/python/firedrop2/>`__
* `PyBloxsom <http://pyblosxom.github.io/>`__
* `Lino WebMan <http://lino.sourceforge.net/webman.html>`__
* `Pelican <http://blog.getpelican.com/>`__
  (also  listed `on PyPi <http://pypi.python.org/pypi/pelican>`__)

Please `let us know`_ of any other reStructuredText Blogs.


.. _Can lists be indented without generating block quotes?:

How should I mark up lists?
---------------------------

Bullet_ & enumerated_ list markup is very intuitive but there are 2
points that must be noted:

.. _bullet: docs/ref/rst/restructuredtext.html#bullet-lists
.. _enumerated: docs/ref/rst/restructuredtext.html#enumerated-lists

1. Lists should **not** be indented.  This is correct::

       paragraph

       * list item 1

         * nested item 1.1
         * nested item 1.2

       * list item 2

   while this is probably incorrect::

       paragraph

         * list item 1

             * nested item 1.1
             * nested item 1.2

         * list item 2

   The extra indentation (of the list containing items 1.1 and 1.2) is
   recognized as a block quote.  This is usually not what you mean and
   it causes the list in the output to be indented too much.

2. There **must** be blank lines around list items, except between
   items of the same level, where blank lines are optional.  The
   example above shows this.

Note that formatting of the *output* is independent of the input, and
is decided by the writer and the stylesheet.  For instance, lists
*are* indented in HTML output by default.  See `How are lists
formatted in HTML?`_ for details.


Could lists be indented without generating block quotes?
--------------------------------------------------------

Some people like to write lists with indentation but don't intend a
blockquote context.  There has been a lot of discussion about allowing
this in reStructuredText, but there are some issues that would need to
be resolved before it could be implemented.  There is a summary of the
issues and pointers to the discussions in `the to-do list`__.

__ docs/dev/todo.html#indented-lists


Could the requirement for blank lines around lists be relaxed?
--------------------------------------------------------------

Short answer: no.

In reStructuredText, it would be impossible to unambiguously mark up
and parse lists without blank lines before and after.  Deeply nested
lists may look ugly with so many blank lines, but it's a price we pay
for unambiguous markup.  Some other plaintext markup systems do not
require blank lines in nested lists, but they have to compromise
somehow, either accepting ambiguity or requiring extra complexity.
For example, `Epytext <http://epydoc.sourceforge.net/epytext.html#list>`__ does
not require blank lines around lists, but it does require that lists
be indented and that ambiguous cases be escaped.


How can I include mathematical equations in documents?
------------------------------------------------------

Use `LaTeX math syntax`_ in a `math directive`_ or `math role`_.

.. _LaTeX math syntax: docs/ref/rst/mathematics.html
.. _math directive: docs/ref/rst/directives.html#math
.. _math role: docs/ref/rst/roles.html#math


Is nested inline markup possible?
---------------------------------

Not currently, no.  It's on the `to-do list`__ (`details here`__), and
hopefully will be part of the reStructuredText parser soon.  At that
time, markup like this will become possible::

    Here is some *emphasized text containing a `hyperlink`_ and
    ``inline literals``*.

__ docs/dev/todo.html#nested-inline-markup
__ docs/dev/rst/alternatives.html#nested-inline-markup

There are workarounds, but they are either convoluted or ugly or both.
They are not recommended.

* Inline markup can be combined with hyperlinks using `substitution
  definitions`__ and references__ with the `"replace" directive`__.
  For example::

      Here is an |emphasized hyperlink|_.

      .. |emphasized hyperlink| replace:: *emphasized hyperlink*
      .. _emphasized hyperlink: https://example.org

  It is not possible for just a portion of the replacement text to be
  a hyperlink; it's the entire replacement text or nothing.

  __ docs/ref/rst/restructuredtext.html#substitution-definitions
  __ docs/ref/rst/restructuredtext.html#substitution-references
  __ docs/ref/rst/directives.html#replace

* The `"raw" directive`__ can be used to insert raw HTML into HTML
  output::

      Here is some |stuff|.

      .. |stuff| raw:: html

         <em>emphasized text containing a
         <a href="https://example.org">hyperlink</a> and
         <tt>inline literals</tt></em>

  Raw LaTeX is supported for LaTeX output, etc.

  __ docs/ref/rst/directives.html#raw


How to indicate a line break or a significant newline?
------------------------------------------------------

`Line blocks`__ are designed for address blocks, verse, and other
cases where line breaks are significant and must be preserved.  Unlike
literal blocks, the typeface is not changed, and inline markup is
recognized.  For example::

    | A one, two, a one two three four
    |
    | Half a bee, philosophically,
    |     must, *ipso facto*, half not be.
    | But half the bee has got to be,
    |     *vis a vis* its entity.  D'you see?
    |
    | But can a bee be said to be
    |     or not to be an entire bee,
    |         when half the bee is not a bee,
    |             due to some ancient injury?
    |
    | Singing...

__ docs/ref/rst/restructuredtext.html#line-blocks

Here's a workaround for manually inserting explicit line breaks in
HTML output::

    .. |br| raw:: html

       <br />

    I want to break this line here: |br| this is after the break.

    If the extra whitespace bothers you, |br|\ backslash-escape it.


A URL containing asterisks doesn't work.  What to do?
-----------------------------------------------------

Asterisks are valid URL characters (see :RFC:`2396`), sometimes used
in URLs.  For example::

    http://cvs.example.org/viewcvs.py/*checkout*/module/file

Unfortunately, the parser thinks the asterisks are indicating
emphasis.  The slashes serve as delineating punctuation, allowing the
asterisks to be recognized as markup.  The example above is separated
by the parser into a truncated URL, an emphasized word, and some
regular text::

    http://cvs.example.org/viewcvs.py/
    *checkout*
    /module/file

To turn off markup recognition, use a backslash to escape at least the
first asterisk, like this::

    http://cvs.example.org/viewcvs.py/\*checkout*/module/file

Escaping the second asterisk doesn't hurt, but it isn't necessary.


How can I make a literal block with *some* formatting?
------------------------------------------------------

Use the `parsed-literal`_ directive.

.. _parsed-literal: docs/ref/rst/directives.html#parsed-literal

Scenario: a document contains some source code, which calls for a
literal block to preserve linebreaks and whitespace.  But part of the
source code should be formatted, for example as emphasis or as a
hyperlink.  This calls for a *parsed* literal block::

    .. parsed-literal::

       print "Hello world!"  # *tricky* code [1]_

The emphasis (``*tricky*``) and footnote reference (``[1]_``) will be
parsed.


Can reStructuredText be used for web or generic templating?
-----------------------------------------------------------

Docutils and reStructuredText can be used with or as a component of a
templating system, but they do not themselves include templating
functionality.  Templating should simply be left to dedicated
templating systems.  Users can choose a templating system to apply to
their reStructuredText documents as best serves their interests.

There are many good templating systems for Python (ht2html_, YAPTU_,
Quixote_'s PTL, Cheetah_, etc.; see this non-exhaustive list of `some
other templating systems`_), and many more for other languages, each
with different approaches.  We invite you to try several and find one
you like.  If you adapt it to use Docutils/reStructuredText, please
consider contributing the code to Docutils or `let us know`_ and we'll
keep a list here.

One reST-specific web templating system is `rest2web
<http://www.voidspace.org.uk/python/rest2web>`_, a tool for
automatically building websites, or parts of websites.

.. _ht2html: http://ht2html.sourceforge.net/
.. _YAPTU:
   http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52305
.. _Quixote: http://www.mems-exchange.org/software/quixote/
.. _Cheetah: http://www.cheetahtemplate.org/
.. _some other templating systems:
   http://webware.sourceforge.net/Papers/Templates/


How can I mark up a FAQ or other list of questions & answers?
-------------------------------------------------------------

There is no specific syntax for FAQs and Q&A lists.  Here are two
options:

1. For a FAQ (Frequently Asked Questions, usually with answers), a
   convenient way to mark up the questions is as section titles, with
   the answer(s) as section content.  This document is marked up in
   this way.

   The advantages of using section titles for questions are: sections
   can be numbered automatically, and a table of contents can be
   generated automatically.  One limitation of this format is that
   questions must fit on one line (section titles may not wrap, in the
   source text).  For very long questions, the title may be a summary
   of the question, with the full question in the section body.

2. Field lists work well as Q&A lists::

       :Q: What kind of questions can we
           put here?

       :A: Any kind we like!

   In order to separate questions, lists can be used:

       1. :Q: What kind of question can we
              put here?
          :A: Any kind we like!

       2. :Q: How many answers can a question have?
          :A: It can have one,
          :A: or more.
          :A3: Answers can be numbered like this.
          :A: 1. Or like this.
              2. We're flexible!

   If you don't want to number or otherwise mark questions, you can
   use an empty comment between individual field lists to separate
   them::

       :Q: First question?
       :A: Answer.

       ..

       :Q: Second question?
       :A: Answer.


.. _bidi:

Can I produce documents in right-to-left languages?
---------------------------------------------------

Languages written from right to left, such as Arabic and Hebrew, must
be reordered according to the `Unicode Bidi Algorithm`_.  This
requires support from the editor and special markup in the output
format.

The source format of reStructuredText is relatively bidi-friendly:
most constructs are denoted by punctuation without intrusion of
English and when you must write in English, it's usually on a separate
line.  So any editor that auto-detects direction per-line (like gedit
or geresh_) will suffice.

Moreover, it's possible to translate_ all reStructuredText keywords.
Docutils 0.17 includes translations for Arab, Hebrew, and
Persian/Farsi.  This should allow reasonable use of editors limited to
a single base direction for the whole document (like Notepad, Vim and
text boxes in Firefox).

.. _Unicode Bidi Algorithm: https://www.unicode.org/reports/tr9/
.. _geresh: http://www.typo.co.il/~mooffie/geresh/
.. _translate: docs/howto/i18n.html

The second problem is bidi markup of the output.  There is an almost
transparent implicit solution for HTML:

* Grab http://cben-hacks.sourceforge.net/bidi/hibidi.py and
  http://cben-hacks.sourceforge.net/bidi/rst2html_hibidi.py.
  Put them both in the same directory and make them executable.

* Use ``rst2html_hibidi.py`` instead of ``rst2html``.

* It infers dir attributes in the HTML from the text.  It does it
  hierachically, giving much better results than usual.  You can still
  use LRM/RLM and LRE/RLE/PDF control codes to help it.

  * If you want the gory details: See the full theory_, and note the
    incomplete practice_ (this is still a partial implementation - but
    sufficient for most needs).

    .. _theory: http://cben-hacks.sourceforge.net/bidi/hibidi.html
    .. _practice: http://cben-hacks.sourceforge.net/bidi/hibidi.html#practice

There is also an explicit way to set directions through CSS and
classes in the HTML:

* Copy ``default.css`` to a new file and add relevant parts of the
  following::

      /* Use these two if the main document direction is RTL */
      body { direction: rtl; }
      div.sidebar { float: left !important; }

      /* The next 3 rules are very useful in documents containing pieces
      of code in english */
      /* Use this if you all your literal blocks (::) are LTR */
      pre {direction: ltr; unicode-bidi: embed; }
      /* Use this if you all your inline literals (``) are LTR */
      tt {direction: ltr; unicode-bidi: embed; }
      /* Use this if you all your interpreted text (`) is LTR */
      cite {direction: ltr; unicode-bidi: embed; }

      /* Allow manual direction override by class directive and roles */
      .rtl { direction: rtl; }
      .ltr { direction: ltr; }

* Select this new stylesheet with ``--stylesheet=<file>`` or the
  stylesheet_ setting.

* Now if you need to override the direction of some element (from a
  paragraph to a whole section), write::

      .. class:: rtl

  or::

      .. class:: ltr

  before it (see the class_ directive for details).

* To change the direction of some inline text fragment, you can use
  RLE/LRE/PDF control characters, or write ``:rtl:`RTL text``` /
  ``:ltr:`RTL text```.  To use the latter syntax, you must write this
  once at the beginning of your document::

      .. role:: ltr
      .. role:: rtl

.. _stylesheet: docs/user/config.html#stylesheet
.. _class: docs/ref/rst/directives.rst#class

LaTeX is quite hard to implement (it doesn't support the bidi
algorithm, so all direction changes - even numbers in RTL text - must
be explicitly marked).  Other formats are more-or-less easy.

If you have any questions/problems/bugs related to bidi with docutils,
ask `Beni Cherniavsky`__ directly or the `Docutils-users`_ mailing
list.

__ mailto:cben@users.sf.net


What's the official MIME type for reStructuredText data?
--------------------------------------------------------

While there is no registered MIME type for reStructuredText, the
"official unofficial" standard MIME type is "text/x-rst". [#]_  This was
invented for the build system for PEPs (Python Enhancement Proposals),
and it's used by the python.org web site build system.

Also see `What's the standard filename extension for a
reStructuredText file?`_

.. [#] The "x-" prefix means it's an unregistered MIME type.


How can I mark up a TODO list?
------------------------------

You may use a field list with class argument and some CSS styling.
For an example see `Docutils TODO lists`_ and its source todo-lists.rst_.

.. _Docutils TODO lists: docs/user/todo-lists.html
.. _todo-lists.rst: docs/user/todo-lists.rst


How can I specify an image grid?
--------------------------------

In order to arrange images (or other content) in a grid,
a borderless `list table`_ can be used. For example::

    .. list-table::
       :class: borderless

       * - .. image:: rst/images/title-scaling.svg
         - .. image:: rst/images/biohazard.svg
       * - .. image:: rst/images/biohazard.svg
         - .. image:: rst/images/biohazard.svg

Use figures, if you want also captions::

    .. list-table::
      :class: borderless

      * - .. figure:: rst/images/title-scaling.svg

            Figure 1/1

        - .. figure:: rst/images/biohazard.svg

            Figure 1/2

.. _list table: docs/ref/rst/directives.html#list-table


HTML Writer
===========

What is the status of the HTML Writer?
--------------------------------------

The default HTML Writer module, `html4css1`_, is
a proof-of-concept reference implementation.  While it is a complete
implementation, some aspects of the HTML it produces may be outdated or
incompatible with older browsers or specialized applications (such as
web templating).

The `html5 writer`_ generates semantic HTML output compatible with HTML5.
For the full selection see `Docutils HTML writers`_

.. _html4css1: docs/user/html.html#html4css1
.. _HTML5 writer: docs/user/html.html#html5
.. _Docutils HTML writers: docs/user/html.html


What browsers are supported?
----------------------------

No specific browser is targeted; all modern graphical browsers should
work.  Some older browsers, text-only browsers, and browsers without
full CSS support are known to produce inferior results.  Firefox,
Safari, Opera, Chrome, Edge, and MS Internet Explorer (version 5.0 and up)
are known to give good results.  Reports of experiences with other
browsers are welcome.


Unexpected results from ``rst2html``: H1, H1 instead of H1, H2.  Why?
--------------------------------------------------------------------------

This only regards output from the `html4css1`_ writer and is configurable
via the initial_header_level_ setting.

.. class:: details

details
    Here's the question in full:

        I have this text::

            Heading 1
            =========

            All my life, I wanted to be H1.

            Heading 1.1
            -----------

            But along came H1, and so shouldn't I be H2?
            No!  I'm H1!

            Heading 1.1.1
            *************

            Yeah, imagine me, I'm stuck at H3!  No?!?

        When I run it through ``rst2html4``, I get unexpected results
        (below).  I was expecting H1, H2, then H3; instead, I get H1, H1,
        H2::

            ...
            <html lang="en">
            <head>
            ...
            <title>Heading 1</title>
            </head>
            <body>
            <div class="document" id="heading-1">
            <h1 class="title">Heading 1</h1>                <-- first H1
            <p>All my life, I wanted to be H1.</p>
            <div class="section" id="heading-1-1">
            <h1><a name="heading-1-1">Heading 1.1</a></h1>        <-- H1
            <p>But along came H1, and so now I must be H2.</p>
            <div class="section" id="heading-1-1-1">
            <h2><a name="heading-1-1-1">Heading 1.1.1</a></h2>
            <p>Yeah, imagine me, I'm stuck at H3!</p>
            ...

        What gives?

    Check the "class" attribute on the H1 tags, and you will see a
    difference.  The first H1 is actually ``<h1 class="title">``; this is
    the document title, and the default stylesheet renders it centered.
    There can also be an ``<h2 class="subtitle">`` for the document
    subtitle.

    If there's only one highest-level section title at the beginning of a
    document, it is treated specially, as the document title.  (Similarly, a
    lone second-highest-level section title may become the document
    subtitle.)  See `How can I indicate the document title?  Subtitle?`_ for
    details.  Rather than use a plain H1 for the document title, we use ``<h1
    class="title">`` so that we can use H1 again within the document.  Why
    do we do this?  HTML only has H1-H6, so by making H1 do double duty, we
    effectively reserve these tags to provide 6 levels of heading beyond the
    single document title.

    With "html4css1", HTML is being used for dumb formatting for nothing
    but final display. A stylesheet *is required*, and one is provided;
    see `Docutils HTML writers`_.  Of course, you're
    welcome to roll your own.  The default stylesheet provides rules to
    format ``<h1 class="title">`` and ``<h2 class="subtitle">``
    differently from ordinary ``<h1>`` and ``<h2>``::

        h1.title {
          text-align: center }

        h2.subtitle {
          text-align: center }

    If you don't want the top section heading to be interpreted as a
    title at all, disable the `doctitle_xform`_ setting
    (``--no-doc-title`` option).  This will interpret your document
    differently from the standard settings, which might not be a good
    idea.  If you don't like the reuse of the H1 in the HTML output, you
    can tweak the `initial_header_level`_ setting
    (``--initial-header-level`` option) -- but unless you match its value
    to your specific document, you might end up with bad HTML (e.g. H3
    without H2).

    .. _doctitle_xform: docs/user/config.html#doctitle-xform
    .. _initial_header_level: docs/user/config.html#initial-header-level

    (Thanks to Mark McEahern for the question and much of the answer.)

    .. note:: For the `html5 writer`_, `initial_header_level`_ defaults to
       ``2`` because this is what the `HTML5 standard`__ expects as
       start value for headings nested in <section> elements.

       .. Sectioning content elements are always considered subsections of
          their nearest ancestor *sectioning root* [#]_ or their nearest
          ancestor element of *sectioning content*  [#]_, whichever is nearest,
          [...]

          .. [#] <blockquote>, <body>, <details>, <dialog>, <fieldset>,
                 <figure>, <td>
          .. [#] <article>, <aside>, <nav>, <section>

          I.e., a top-level <section> is a subsection of <body>.

       __ https://www.w3.org/TR/html53/sections.html#headings-and-sections


How are lists formatted in HTML?
--------------------------------

If list formatting looks strange, first check that you understand
`list markup`__.

__ `How should I mark up lists?`_

* By default, HTML browsers indent lists relative to their context.
  This follows a long tradition in browsers (but isn't so established
  in print).  If you don't like it, you should change the stylesheet.

  This is different from how lists look in reStructuredText source.
  Extra indentation in the source indicates a blockquote, resulting in
  too much indentation in the browser.

* A list item can contain multiple paragraphs etc.  In complex cases
  list items are separated by vertical space.  By default this spacing
  is omitted in "simple" lists.  A list is simple if every item
  contains a simple paragraph and/or a "simple" nested list.  For
  example:

      * text

        * simple

          * simple
          * simple

        * simple

        text after a nested list

      * multiple

        paragraphs

  In this example the nested lists are simple (and should appear
  compacted) but the outer list is not.

  If you want all lists to have equal spacing, disable the
  `compact_lists`_ setting (``--no-compact-lists`` option).  The
  precise spacing can be controlled in the stylesheet.

  Note again that this is not exactly WYSIWYG: it partially resembles
  the rules about blank lines being optional between list items in
  reStructuredText -- but adding/removing optional blank lines does
  not affect spacing in the output!  It's a feature, not a bug: you
  write it as you like but the output is styled consistently.

  .. _compact_lists: docs/user/config.html#compact-lists


Why do enumerated lists only use numbers (no letters or roman numerals)?
------------------------------------------------------------------------

The rendering of enumerators (the numbers or letters acting as list
markers) is completely governed by the stylesheet, so either the
browser can't find the stylesheet (try enabling the
`embed_stylesheet`_ setting [``--embed-stylesheet`` option]), or the
browser can't understand it (try a not too old Firefox, Chrome, Opera,
Safari, Edge, or even MSIE).

.. _embed_stylesheet: docs/user/config.html#embed-stylesheet


There appear to be garbage characters in the HTML.  What's up?
--------------------------------------------------------------

What you're seeing is most probably not garbage, but the result of a
mismatch between the actual encoding of the HTML output and the
encoding your browser is expecting.  Your browser is misinterpreting
the HTML data, which is encoded text.  A discussion of text encodings
is beyond the scope of this FAQ; see one or more of these documents
for more info:

* `UTF-8 and Unicode FAQ for Unix/Linux
  <http://www.cl.cam.ac.uk/~mgk25/unicode.html>`_

* Chapters 3 and 4 of `Introduction to i18n [Internationalization]
  <http://www.debian.org/doc/manuals/intro-i18n/>`_

* `Python Unicode Tutorial
  <http://www.reportlab.com/i18n/python_unicode_tutorial.html>`_

The common case is with the default output encoding (UTF-8), when
using symbol-footnotes.  Most footnote symbols are not available in ASCII.
When encoded with UTF-8 and viewed with ordinary ASCII tools,
these characters will appear to be multi-character garbage.

You may have an decoding problem in your browser (or editor, etc.).
The encoding of the output is set to UTF-8, but your browser isn't
recognizing that.  You can either try to fix your browser (enable
"UTF-8 character set", sometimes called "Unicode"), or choose a
different `output-encoding`_.  You can also try
``--output-encoding=ascii:xmlcharrefreplace`` for HTML or XML, but not
applicable to non-XMLish outputs.

If you're generating document fragments, the "Content-Type" metadata
(between the HTML ``<head>`` and ``</head>`` tags) must agree with the
encoding of the rest of the document.  For UTF-8, it should be::

    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

Also, Docutils normally generates an XML declaration as the first line
of the output.  It must also match the document encoding.  For UTF-8::

    <?xml version="1.0" encoding="utf-8" ?>

.. _sectnum: docs/ref/rst/directives.html#sectnum
.. _output-encoding: docs/user/config.html#output-encoding


How can I retrieve the body of the HTML document?
-------------------------------------------------

(This is usually needed when using Docutils in conjunction with a
templating system.)

You can use the `docutils.core.publish_parts()`_ function, which
returns a dictionary containing an 'html_body_' entry.

.. _docutils.core.publish_parts(): docs/api/publisher.html#publish-parts
.. _html_body: docs/api/publisher.html#html-body


Why is the Docutils XHTML served as "Content-type: text/html"?
--------------------------------------------------------------

Full question:

    Docutils' HTML output looks like XHTML and is advertised as such::

      <?xml version="1.0" encoding="utf-8" ?>
      <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
       "https://www.w3.org/TR/xht ml1/DTD/xhtml1-transitional.dtd">

    But this is followed by::

      <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

    Shouldn't this be "application/xhtml+xml" instead of "text/html"?

Short answer: if we didn't serve XHTML as "text/html" (which is a
perfectly valid thing to do), it couldn't be viewed in Internet
Explorer.

Long answer: In a perfect web, the Docutils XHTML output would be 100%
strict XHTML.  But it's not a perfect web, and a major source of
imperfection is Internet Explorer.  When the "html4css1"
writer was created, IE represented the overwhelming majority of web
browsers "in the wild".
The behaviour is kept for backwards compatibility while
modern, semantic HTML output is available with the "html5" writer.

(Thanks to Martin F. Krafft, Robert Kern, Michael Foord, and Alan
G. Isaac.)


Python Source Reader
====================

Can I use Docutils for Python auto-documentation?
-------------------------------------------------

Yes, in conjunction with other projects.

The Sphinx_ documentation generator includes an autodoc module.

.. _Sphinx: http://www.sphinx-doc.org

Version 2.0 of Ed Loper's `Epydoc <http://epydoc.sourceforge.net/>`_
supports reStructuredText-format docstrings for HTML output.  Docutils
0.3 or newer is required.

Development of a Docutils-specific auto-documentation tool is suspended.

.. Epydoc works by importing
   Python modules to be documented, whereas the Docutils-specific tool,
   described above, will parse modules without importing them (as with
   `HappyDoc <http://happydoc.sourceforge.net/>`_, which doesn't support
   reStructuredText).

   The advantages of parsing over importing are security and flexibility;
   the disadvantage is complexity/difficulty.

   * Security: untrusted code that shouldn't be executed can be parsed;
     importing a module executes its top-level code.
   * Flexibility: comments and unofficial docstrings (those not supported
     by Python syntax) can only be processed by parsing.
   * Complexity/difficulty: it's a lot harder to parse and analyze a
     module than it is to ``import`` and analyze one.

   For more details, please see "Docstring Extraction Rules"
   in :PEP:`258`, item 3 ("How").


Miscellaneous
=============

Is the Docutils document model based on any existing XML models?
----------------------------------------------------------------

Not directly, no.  It borrows bits from DocBook, HTML, and others.  I
(David Goodger) have designed several document models over the years,
and have my own biases.  The Docutils document model is designed for
simplicity and extensibility, and has been influenced by the needs of
the reStructuredText markup.


.. _contribute:

How to make code contributions that are easily accepted
-------------------------------------------------------

* Follow the `Python coding conventions`_ and `documentation
  conventions`_ in the Docutils Policies.
  Ensure the addition works with all `supported Python versions`_.

  Look at the Docutils sources to see how similar features are
  implemented, learn to do it "the Docutils way".

* Prepare tests_. Test cases are also examples and showcases for new
  features.

* Include documentation.

* For larger changes, consider creating a `feature branch`_ in a
  Docutils repository_ checkout. [#]_

* Mail your patch to the Docutils-develop_ mailing list or attach it to the
  relevant ticket at Docutils' `bug tracker`_ or `feature request tracker`_.
  We accept patches created with diff, SVN, or Git.

The developers will make sure that contributions fit nicely into Docutils.
This might involve discussing (and compromising on) design and
implementation details. It might also lead to the conclusion that the
addition fits better in the `extensions and related projects`_.

.. [#] Working with branches is much easier with Git_. You can get a Git
   clone of the repository from http://repo.or.cz/w/docutils.git or with
   git-svn.

.. _Python coding conventions: docs/dev/policies.html#python-coding-conventions
.. _documentation conventions: docs/dev/policies.html#documentation-conventions
.. _tests: docs/dev/testing.html
.. _supported Python versions: README.html#requirements
.. _feature branch: docs/dev/policies.html#feature-branch
.. _Git: http://git-scm.com/
.. _bug tracker: https://sourceforge.net/p/docutils/bugs/



.. Emacs settings

   Local Variables:
   mode: indented-text
   mode: rst
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:

.. Here's a code css to make a table colourful::

   /* Table: */

   th {
       background-color: #ede;
   }

   /* alternating colors in table rows */
   table.docutils tr:nth-child(even) {
       background-color: #F3F3FF;
   }
   table.docutils tr:nth-child(odd) {
       background-color: #FFFFEE;
   }

   table.docutils tr {
       border-style: solid none solid none;
       border-width: 1px 0 1px 0;
       border-color: #AAAAAA;
   }
