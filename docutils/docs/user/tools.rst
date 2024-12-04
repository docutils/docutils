.. include:: ../header.rst

==========================
 Docutils Front-End Tools
==========================

:Author: David Goodger
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

.. contents::

--------------
 Introduction
--------------

In addition to the generic "docutils_" application, Docutils installs
a set of small front ends, each specialized for a specific
"Reader" (which knows how to interpret a file in context),
"Parser" (which understands the syntax of the text), and
"Writer" (which knows how to generate a specific data format).

.. note::
   Docutils front-end tool names, install details and the set of
   auto-installed tools changed in Docutils 0.21 (see the RELEASE-NOTES_).

.. _usage pattern:

Most [#]_ front-end tools have common options and the same command-line
usage pattern::

    <toolname> [options] [<source> [<destination>]]

See `the tools`_ below for concrete examples.

Each tool has a "``--help``" option which lists the
`command-line options`_ and arguments it supports.
Processing can also be customized with `configuration files`_.

The two arguments, "source" and "destination", are optional.  If only
one argument (source) is specified, the standard output (stdout) is
used for the destination.  If no arguments are specified, the standard
input (stdin) is used for the source.

.. [#] The exception is buildhtml.py_.

.. note::
   The command line usage pattern will change over the next releases.

   See `Future changes`_ in the RELEASE-NOTES.


.. _RELEASE-NOTES: ../../RELEASE-NOTES.html
.. _future changes: ../../RELEASE-NOTES.html#future-changes


Getting Help
============

First, try the "``--help``" option each front-end tool has.

Command line options and their corresponding configuration file entries
are detailed in `Docutils Configuration`_.

Users who have questions or need assistance with Docutils or
reStructuredText should post a message to the Docutils-users_ mailing
list.

.. _Docutils-users: mailing-lists.html#docutils-users


-----------
 The Tools
-----------

Generic Command Line Front End
==============================

docutils
--------

:CLI name: ``docutils``
:Readers:  Standalone (default), PEP
:Parsers:  reStructuredText (default), Markdown (requires 3rd party packages)
:Writers:  html_, html4css1_, html5_ (default), latex2e_, manpage_,
           odt_, pep_html_, pseudo-xml_, s5_html_, xelatex_, xml_,
:Config_:  `[docutils application]`_

Since Docutils 0.19, you can start the generic front end like::

    docutils test.rst > test.html

Alternatively, use Python's ``-m`` option, or the ``docutils-cli.py``
script in the ``tools/`` directory.

The generic front end allows combining "reader", "parser", and
"writer" components from the Docutils package or 3rd party plug-ins.

For example, to process a Markdown_ file "``test.md``" into
Pseudo-XML_::

    docutils --parser=markdown --writer=pseudoxml test.md > test.rst

Another example is converting a reStructuredText PEP_ source into a HTML preview [#]_::

    docutils --reader=pep --writer=pep_html pep-0287.rst > pep-0287.html

The pep_html_ writer makes use of a "``pep-html-template``" file and
the "``pep.css``" stylesheet (both in the ``docutils/writers/pep_html/``
directory), but these can be overridden by command-line options or
configuration files.

Use the "--help" option together with the component-selection options
to get the correct list of supported command-line options. Example::

    docutils --parser=markdown --writer=xml --help

.. [#] The rendering of published PEPs is done by a Sphinx-based build system
       (see :PEP:`676`).

.. _[docutils application]: config.html#docutils-application
.. _Markdown: https://www.markdownguide.org/
.. _PEP: https://peps.python.org
.. _pep_html: html.html#pep-html


HTML-Generating Tools
=====================

.. _rst2html.py:

rst2html
--------

:Reader: Standalone
:Parser: reStructuredText
:Writer: html_

``rst2html`` is the front-end for the default Docutils HTML writer.
The default writer may change with the development of HTML, browsers,
Docutils, and the web.
The current default is html4css1_, it will change to html5_ in Docutils 2.0.

.. caution::
   Use a specific front end like rst2html4_ or rst2html5_,
   if you depend on stability of the generated HTML code
   (e.g., because you use a custom style sheet or post-processing
   that may break otherwise).

.. _html: html.html#html


.. _rst2html4.py:

rst2html4
---------

:Reader: Standalone
:Parser: reStructuredText
:Writer: html4css1_

The ``rst2html4`` front end reads standalone reStructuredText source
files and produces `XHTML 1.0 Transitional`_ output.
A CSS stylesheet is required for proper rendering; a simple but
complete stylesheet is installed and used by default (see Stylesheets_
below).

For example, to process a reStructuredText file "``test.rst``" into
HTML::

    rst2html test.rst > test.html

Now open the "``test.html``" file in your favourite browser to see the
results.  To get a footer with a link to the source file, date & time
of processing, and links to the Docutils project, add some options::

    rst2html --source-link --time --generator test.rst > test.html


Stylesheets
```````````

``rst2html`` inserts into the generated HTML a cascading stylesheet
(or a link to a stylesheet, when passing the "``--link-stylesheet``"
option).  A stylesheet is required for proper rendering.
The default stylesheet (``docutils/writers/html4css1/html4css1.css``,
located in the installation directory) is provided for basic use.

To use different stylesheet(s), specify the stylesheets' location(s) as
comma-separated list with the "`-\-stylesheet`_"
or "`-\-stylesheet-path`_" options.
To experiment with styles, please see the
`guide to writing HTML (CSS) stylesheets for Docutils`__.

.. _XHTML 1.0 Transitional: https://www.w3.org/TR/xhtml1/
.. _html4css1: html.html#html4css1
.. _link-stylesheet: config.html#embed-stylesheet
.. _--stylesheet: config.html#stylesheet
.. _--stylesheet-path: config.html#stylesheet-path
__ ../howto/html-stylesheets.html


.. _rst2html5.py:

rst2html5
---------

:Reader: Standalone
:Parser: reStructuredText
:Writer: html5_

The ``rst2html5`` front end reads standalone reStructuredText source
files and produces `HTML 5`_ output.
Correct rendering of elements not directly supported by HTML depends on a
CSS style sheet. The provided style sheets ``minimal.css`` and ``plain.css``
define required and optional styling rules respectively.

.. _html5: html.html#html5
.. _HTML 5: https://www.w3.org/TR/html5/


.. _rst2s5.py:

rst2s5
------

:Reader: Standalone
:Parser: reStructuredText
:Writer: s5_html_

The ``rst2s5`` front end reads standalone reStructuredText source
files and produces (X)HTML output compatible with S5_, the "Simple
Standards-based Slide Show System" by Eric Meyer.  A theme is required
for proper rendering; several are distributed with Docutils and others
are available; see Themes_ below.

For example, to process a reStructuredText file "``slides.rst``" into
S5/HTML::

    rst2s5 slides.rst --output=slides.html

Now open the "``slides.html``" file in your favourite browser, switch
to full-screen mode, and enjoy the results.

.. _S5: http://meyerweb.com/eric/tools/s5/
.. _s5_html: html.html#s5-html

Themes
``````

Each S5 theme consists of a directory containing several files:
stylesheets, JavaScript, and graphics.  These are copied into a
``ui/<theme>`` directory beside the generated HTML. [#copy-theme]_
A theme is chosen
using the "``--theme``" option (for themes that come with Docutils) or
the "``--theme-url``" option (for themes anywhere).  For example, the
"medium-black" theme can be specified as follows::

    rst2s5 --theme medium-black slides.rst --output=slides.html

The theme will be copied [#copy-theme]_ to the ``ui/medium-black`` directory.

Several themes are included with Docutils:

``default``
    This is a simplified version of S5's default theme.

    :Main content: black serif text on a white background
    :Text capacity: about 13 lines
    :Headers: light blue, bold sans-serif text on a dark blue
              background; titles are limited to one line
    :Footers: small, gray, bold sans-serif text on a dark blue
              background

``small-white``
    (Small text on a white background.)

    :Main content: black serif text on a white background
    :Text capacity: about 15 lines
    :Headers: black, bold sans-serif text on a white background;
              titles wrap
    :Footers: small, dark gray, bold sans-serif text on a white
              background

``small-black``
    :Main content: white serif text on a black background
    :Text capacity: about 15 lines
    :Headers: white, bold sans-serif text on a black background;
              titles wrap
    :Footers: small, light gray, bold sans-serif text on a black
              background

``medium-white``
    :Main content: black serif text on a white background
    :Text capacity: about 9 lines
    :Headers: black, bold sans-serif text on a white background;
              titles wrap
    :Footers: small, dark gray, bold sans-serif text on a white
              background

``medium-black``
    :Main content: white serif text on a black background
    :Text capacity: about 9 lines
    :Headers: white, bold sans-serif text on a black background;
              titles wrap
    :Footers: small, light gray, bold sans-serif text on a black
              background

``big-white``
    :Main content: black, bold sans-serif text on a white background
    :Text capacity: about 5 lines
    :Headers: black, bold sans-serif text on a white background;
              titles wrap
    :Footers: not displayed

``big-black``
    :Main content: white, bold sans-serif text on a black background
    :Text capacity: about 5 lines
    :Headers: white, bold sans-serif text on a black background;
              titles wrap
    :Footers: not displayed

If a theme directory contains a file named ``__base__``, the name of
the theme's base theme will be read from it.  Files are accumulated
from the named theme, any base themes, and the "default" theme (which
is the implicit base of all themes).

For details, please see `Easy Slide Shows With reStructuredText & S5
<slide-shows.html>`_.

.. [#copy-theme] Theme files can only be copied by Docutils, if the
   `output path`_ is specified.

.. _output path: config.html#output-path


buildhtml.py
------------

:Readers: Standalone, PEP
:Parser:  reStructuredText
:Writers: html_, html5_, pep_html_
:Config_: `[buildhtml application]`_

The ``buildhtml.py`` script can be found in the ``/tools`` directory of
the "docutils" source. It is not included in binary packages. [#]_

Usage::

    buildhtml.py [options] [<directory> ...]

Use ``buildhtml.py`` to generate ``*.html`` from reStructuredText
source files in each <directory> given, and their subdirectories too.
The current directory is chosen by default if no directory is specified.
Use the ``--local`` option to skip all subdirectories.
See `[buildhtml application]`_ in `Docutils Configuration`_
for more specific selection options.

After unpacking the Docutils source package, you can ``cd`` into its root
directory (``docutils-X.Y/`` where "X.Y" is the release version, for
official releases) and generate HTML pages for all included documentation
files with::

    tools/buildhtml.py

Some files may generate system messages (docs/user/rst/demo.rst contains
intentional errors); use the ``--quiet`` option to suppress all warnings.

.. [#] The Debian package "python3-docutils" includes the script
   under the name ``rst-buildhtml``.

.. _[buildhtml application]: config.html#buildhtml-application
.. _configuration file: `configuration files`_


LaTeX-Generating Tools
======================

.. _rst2latex.py:

rst2latex
---------

:Reader: Standalone
:Parser: reStructuredText
:Writer: latex2e_

The ``rst2latex`` front end reads standalone reStructuredText
source files and produces LaTeX_ output. For example, to process a
reStructuredText file "``test.rst``" into LaTeX::

    rst2latex test.rst > test.tex

The output file "``test.tex``" should then be processed with ``latex``
or ``pdflatex`` to get a document in DVI, PostScript or PDF format for
printing or on-screen viewing.

For details see `Generating LaTeX with Docutils`_.

.. _latex2e:
.. _Generating LaTeX with Docutils: latex.html


.. _rst2xetex.py:

rst2xetex
---------

:Reader: Standalone
:Parser: reStructuredText
:Writer: _`xelatex`

The ``rst2xetex`` front end reads standalone reStructuredText source
files and produces `LaTeX` output for processing with Unicode-aware
TeX engines (`LuaTeX`_ or `XeTeX`_). For example, to process a
reStructuredText file "``test.rst``" into LaTeX::

    rst2xetex test.rst > test.tex

The output file "``test.tex``" should then be processed with ``xelatex`` or
``lualatex`` to get a document in PDF format for printing or on-screen
viewing.

For details see `Generating LaTeX with Docutils`_.

.. _LaTeX: https://en.wikipedia.org/wiki/LaTeX
.. _XeTeX: https://en.wikipedia.org/wiki/XeTeX
.. _LuaTeX: https://en.wikipedia.org/wiki/LuaTeX


Man-Page-Generating Tools
=========================

.. _rst2man.py:

rst2man
-------

:Reader: Standalone
:Parser: reStructuredText
:Writer: manpage_

The ``rst2man`` front end reads standalone reStructuredText source
files and produces troff_ sources for Unix man pages.

.. _manpage: manpage.html
.. _troff: https://troff.org/


OpenDocument-Generating Tools
=============================

.. _rst2odt.py:

rst2odt
-------

:Reader: Standalone
:Parser: reStructuredText
:Writer: odt_

The ``rst2odt`` front end reads standalone reStructuredText
source files and produces ODF/.odt files that can be read, edited,
printed, etc with OpenOffice_ ``oowriter`` or LibreOffice_ ``lowriter``.
A stylesheet file is required.  A
stylesheet file is an OpenOffice .odt file containing definitions
of the styles required for ``rst2odt``.
For details, see `Odt Writer for Docutils`_.

.. _OpenOffice: https://www.openoffice.org/
.. _LibreOffice: https://www.libreoffice.org/
.. _odt:
.. _Odt Writer for Docutils: odt.html



reStructuredText-Generating Tools
=================================

Currently, there is no reStructuredText writer in Docutils and therefore
an ``rst2rst`` tool is still missing.

To generate reStructuredText documents with Docutils, you can use
the XML (Docutils native) writer and the xml2rst_ processor.


XML-Generating Tools
====================

.. _rst2xml.py:

rst2xml
-------

:Reader: Standalone
:Parser: reStructuredText
:Writer: _`XML` (Docutils native)

The ``rst2xml`` front end produces Docutils-native XML output.
This can be transformed with standard XML tools such as XSLT
processors into arbitrary final forms. An example is the xml2rst_ processor
by Stefan Merten.

.. _xml2rst: http://www.merten-home.de/FreeSoftware/xml2rst/index.html


Testing/Debugging Tools
=======================

.. _rst2pseudoxml.py:

rst2pseudoxml
-------------

:Reader: Standalone
:Parser: reStructuredText
:Writer: _`Pseudo-XML`

``rst2pseudoxml`` is used for debugging the Docutils "Reader to
Transform to Writer" pipeline.  It produces a compact pretty-printed
"pseudo-XML", where nesting is indicated by indentation (no end-tags).
External attributes for all elements are output, and internal
attributes for any leftover "pending" elements are also given.


---------------
 Customization
---------------

All front-end tools support the options/settings from the generic
`configuration file sections`_ plus the sections of their components
(reader, writer, parser).
Some front-end tools also add application-specific settings.


Command-Line Options
====================

Command-line options are intended for one-off customization.
They take priority over configuration file settings.

Use the ``--help`` option on each of the front ends to list the
command-line options it supports.


Configuration Files
===================

Configuration files are used for persistent customization; they can be
set once and take effect every time you use a front-end tool.

Command-line options and their corresponding configuration file entry
names are listed in the `Docutils Configuration`_ document.

.. _Docutils Configuration: config.html
.. _Config:
.. _configuration file sections:
   config.html#configuration-file-sections-entries

.. Emacs settings

   Local Variables:
   mode: indented-text
   mode: rst
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
