..                            -*- rst-mode -*-

=====================
Latex Writer Variants
=====================

:Author: Günter Milde <milde@users.berlios.de>
:Date: $Date$

This sandbox project is a place to try out and discuss
`latex2e writer extensions`_ and `alternative latex writers`_
for special needs.

.. contents::

``latex2e`` writer extensions
*****************************

Questionnaire
=============

#. Which `default font`_ do you prefer for the output?

#. Did you experience problems with a missing ``aeguill.sty`` package?

   Did you have problems with Find or Export of words with
   non-ASCII chars (e.g. Umlauts) in the PDF reader (Acrobat, xpdf, ...)

   Which `font encoding`_ do you prefer for the output?

#. Would you use/configure a `configurable transition element`_?

#. Does the latex2e writer need a `--stylesheet-path option`_?

#. Feature wishes or ideas of improvement for the `latex2e` writer.


Users of ``rst2latex2.py`` are encouraged to respond by mail to the
docutils-users list.

Proposed Changes
================

See also the notes in
http://docutils.sourceforge.net/docs/user/latex.html#problems

Default font
------------

Which default font do you prefer for the output?

Answers:
  By default, PDFLaTeX uses embedded computer modern family, which look good
  on paper but poor on LCDs (even if outlined, due to suboptimal hinting),
  and result in large PDFs.  rst2pdf uses the "standard" PDF fonts by
  default (Times, Helvetica, Courier) which IMO look reasonable both on
  screen and on paper.  Because they aren't embedded, it also results in a
  smaller file size.

Using different fonts (e.g. standard postscript fonts) can be achieved
easily by selecting a font package with the ``--stylesheet`` command line
option or in a custom style-sheet.

Font embedding must be configured in the LaTeX installation.

Proposal
~~~~~~~~

Use one of the Postscript default fonts supported by standard LaTeX (pages
10 and 11 of the `PSNFSS documentation`_)

Bookman
  -2  very very wide
Charter
  +0  nonspectacular
  -1  no "Base35" font
New Century Schoolbook
  -1  very wide
Palatino
  +1  recommended by font experts
  +1  good LaTeX support including matching math fonts, small caps, old-style figures
Times
  +1  'de facto standard'
  -1  overused
  -1  narrow (devised for multi-column layouts)
Utopia
  +1  recommended by font experts
  -1  no `Base35` font, no required part of LaTeX any more.

Implement as default stylesheet option, so it can be easily overridden.

   My vote is for Palatino.
   --Matthew Leingang


Adaptive preamble
-----------------

User complaint:
  TeXlive is a large download, and in addition to the base package rst2latex
  also requires a bunch of extra ``*.sty`` packages, even if they aren't
  used in the current document.

Solution:
  include only package loading commands and definitions that are
  needed in the current document.

Implemented:
  `fallback definitions`_ for Docutils-specific constructs.

TODO:
  A similar ``LaTeXTranslator.latex_requirements`` dictionary for additions
  needed *before* the custom style sheet (required packages and their
  settings).


Configurable transition element
-------------------------------

By default, the `transition element`_ is rendered as a horizontal line.

In novels, ofte the form of three stars or some ornamental is used.
A simple vertical space is also common.

Propsal
~~~~~~~
Use semantic markup with a ``\DUtransition`` command that can be
customised in a style sheet.


Table classes
-------------

Currently, table export uses a logic based on the relative width of the
columns in the input to set the column width in the output.

Formal (booktabs) vs. standard (fully bordered) tables can be chosen in the
configuration settings (only document wide).

Tables without borders are possible with the ``borderless`` class argument
like::

   The following Unicode characters may also precede inline markup:

       .. class:: borderless

       ===  ==========================================================
       ‘    (U+2018, left single-quote)
       “    (U+201C, left double-quote)
       ’    (U+2019, apostrophe)
       «    (U+00AB, left guillemet, or double angle quotation mark)
       ¡    (U+00A1, inverted exclamation mark)
       ¿    (U+00BF, inverted question mark)
       ===  ==========================================================

Proposal
~~~~~~~~

Add more classes e.g. for column width set by latex, horizontal aligment.


--stylesheet-path option
------------------------

There are 2 use cases:

1. files in the TEXINPUTS path ("installed", site-wide style files (standard
   or local))

   * specify only the filename
   * include literally

2. files outside the TEXINPUTS path (not installed, local style files)

   * a relative path should be rewritten if the output document is in a
     different dir than the pwd

Currently, 1) is done with --stylesheet and 2) with --stylesheet-path.

But:

-1  having both ``--stylesheet`` and ``stylesheet-path`` makes things complicated:

  Explaining the two options and their interaction to the user is
  not straightforward.

  This holds even more if you take into account the third related
  option, ``--embed-stylesheet``.

-1  it is impossible to have some paths rewritten and some not, as in e.g. ::

      --stylesheet=mathpazo -stylesheet-path=mystyle.tex

    "mystyle.tex" would overwrite "mathpazo".

Proposal
~~~~~~~~

Instead of two options, do "the right thing" based on simple rules, e.g:

a) Use the "package" vs. "custom style sheet" distinction:

   Rewrite, if there is an filename extension and it is not ``.sty`` ::

     --stylesheet='mathpazo,mystyle.tex'

   will use 'mathpazo' verbatim and rewrite 'mystyle.tex'.

   -1  will not work for latex packages in the pwd or outside the TEXINPUTS
       path.

b) rewrite paths that have a directory part::

     --stylesheet='mathpazo,./mystyle.sty'

   will use 'mathpazo' verbatim and rewrite './mystyle.sty'.

   +1  explicite and flexible

   +1  the common case (files in the TEXINPUTS path) is the most simple

   -1  need to document/learn special syntax

c) rewrite path if this prevents errors:

   * Check for a given file (or relative path) relative to pwd and output dir.
   * If it is available relative to pwd but not relative to the output dir,
     rewrite the path.

   +1  no need to explain any additional syntax

   +1  does "the right thing" in most usual cases

   -1  hidden automatism might lead to problems in corner cases

   -1  mechanism depends on the availability of files at the time of the run,
       which is harder to explain and less predictable than b)

.. Use case:

  A project with rst documents sorted into a hierarchy of sub-directories
  and a common style file in the base dir or a sub dir::

   .
   |_ base.txt
   |_ style.tex
   |_ docutils.conf
   |_ A/
   |  |_ a.txt
   |  | ...
   |_ B/
      |_ b.txt


  With the line ::

   stylesheet-path: style.tex

  in docutils.conf, all documents will get a valid link to the style file,
  if the conversion is started from the base dir.


Image and figure directives
---------------------------

* Document graphics peculiarities, e.g. accepted formats.

* should start a new paragraph.

.. compare the functional test result:
   /home/milde/Code/Python/docutils-svn/docutils/test/functional/input/data/standard.txt
   and /home/milde/Code/Python/docutils-svn/docutils/test/functional/output/standalone_rst_latex.tex

* centered and aligned images with ``\centerline``, ``\flushleft``,
  ``\flushright``.

* aligning a figure also aligns the legend *but not the caption*

  What should be aligned?

  Should the surrounding text wrap around the figure?


image width
~~~~~~~~~~~

#. add default unit if none given (to prevent LaTeX error)
   a poll on docutils-users favoured ``bp`` (Big Point: 1 bp  = 1/72 in)

#. Do not change ``px`` to ``pt``.

  * ``px`` is a valid unit in pdftex since version 1.3.0 released on
    2005-02-04:

     1px defaults to 1bp (or 72dpi), but can be changed with the \pdfpxdimen
     primitive::

       \pdfpxdimen=1in % 1 dpi
       \divide\pdfpxdimen by 96 % 96 dpi

     --  http://www.tug.org/applications/pdftex/NEWS

  * Modern TeX distributions use pdftex also for dvi generation
    (i.e. ``latex`` actually calls ``pdftex`` with some options).

  * Users of legacy TeX versions can be advised to strip the ``px``: while
    HTML will default to pixels, LaTeX will use points (1/72 in) which
    conforms to the ``pdftex`` default.

Backwards compatibility
~~~~~~~~~~~~~~~~~~~~~~~

Images with width specification in ``px`` come out slightly larger:

  1 bp  = 1/72 in > 1 pt = 1/72.25 in

Needed Action:
  if this is an issue, add ``\pdfpxdimen=1pt`` to the style-sheet or as
  raw latex.


Implemented Changes
===================

Changes to the latex2e writer in the SVN version since docutils version 0.5.

Also see the `Docutils Release Notes`_ and the `Docutils History`_

.. Steps for uploading Changes:

  * provide patch to latex2e/__init__.py
  * get approvement from the latex writer maintainer (Engelbert Gruber)

  * (add and) run functional tests::
    
       ../../docutils/test/test_functional.py
  
    + if output changed: test-compile and approve new output in
      ../../docutils/test/functional/output/
           
  * fix documentation in ../../docutils/docs/user/latex.txt
  
  * add changes to ../../docutils/HISTORY.txt
  
  * add backwards-incompatible changes to ../../docutils/RELEASE-NOTES.txt
    
Custom roles
------------

New Feature: failsave implementation
 As with classes to HTML objects, class arguments are silently ignored if
 there is no styling rule for this class in a custom style sheet.

TODO: Custom roles based on standard roles.

Backwards compatibility
~~~~~~~~~~~~~~~~~~~~~~~

SVN versions 5742 to 5861 contained an implementation that
did not work with commands expecting an argument.
The implementation from version 5862 is fully backwards compatible.

LaTeX style sheets
------------------

New Feature:
  LaTeX packages can be used as ``--stylesheet`` argument without
  restriction.

Implementation:
  Use ``\usepackage`` if style sheet ends with ``.sty`` or has no
  extension and ``\input`` else.

Rationale:
  while ``\input`` works with extension as well as without extension,
  ``\usepackage`` expects the package name without extension. (The latex2e
  writer will strip a ``.sty`` extension.)


Backwards compatibility
~~~~~~~~~~~~~~~~~~~~~~~

Up to Docutils 0.5, if no filename extension is given in the ``stylesheet``
argument, ``.tex`` is assumed (by latex).

Since Docutils 0.6, a stylesheet without filename extension is assumed to be
a LaTeX package (``*.sty``) and referenced with the ``\usepackage`` command.

Needed Action:
  Always specify the extension if you want the style sheet to be
  ``\input`` by LaTeX.


Fallback definitions
--------------------

Include only definitions that are needed in the
current document.

Implementation:

  The ``LaTeXTranslator.visit<node>`` functions store needed definitions and
  commands in the dictionary ``LaTeXTranslator.latex_fallbacks``.

  The fallbacks are defined with ``\providecommand``.

  The content of ``LaTeXTranslator.latex_fallbacks`` is written to the
  preamble *after* the custom stylesheet reference/inclusion.  Customising
  in a style sheet is possible with ``\newcommand``.


Length units
------------

- Add ``bp`` to lenghts without unit (prevents LaTeX errors)

- Do not convert ``px`` to ``pt`` (``px`` is supported by pdfTeX since
  2005-02-04 as a configurable length unit)

Backwards compatibility
~~~~~~~~~~~~~~~~~~~~~~~

If a length without unit is recognised by latex, it reports an error and add
``pt``. The new default unit is ``bp`` (big points) which is slightly bigger,
1 bp = 1.00374 pt.

The unit ``px`` is not defined in "pure" LaTeX, but introduced by the
`pdfTeX` converter on 2005-02-04. `pdfTeX` is used in all modern LaTeX
distributions (since ca. 2006) also for conversion into DVI.

Needed Action:
  If updating LaTeX is not an option, just remove the ``px`` from the lengh
  specification. HTML/CSS will default to ``px`` while the `latexe2` writer
  will add the fallback unit ``bp``.

Font encoding
-------------

Do not mix font-encoding and font settings: do not load `ae` and `aeguill`
unless explicitely required via the ``--stylesheet`` option.


Example:
  ``--font-encoding=LGR,T1`` becomes ``\usepackage[LGR,T1]{fontenc}``
  (Latin, Latin-1 Supplement, and Greek)


Backwards compatibility
~~~~~~~~~~~~~~~~~~~~~~~

The following behaviour is new:

:font-encoding = '':  do not load `ae` and `aeguill` packages, i.e. 
	       	      * do not change font settings,
		      * do not use the fontenc package 
		        (implicitely loaded via `ae`),
		      * use LaTeX default font encoding (OT1)
:font-encoding = OT1: load `fontenc` with ``\usepackage[OT1]{fontenc}``

Needed Action:
  You get the old behaviour with ``--stylesheet=ae,aeguill``.

  However, using `ae` is not recommended. A similar look (but better
  implementation) can be achieved with the packages `lmodern`, `cmsuper`, or
  `cmlgr` all providing Computer Modern look-alikes in vector format and T1
  encoding, e.g. ``--font-encoding=T1 --stylesheet=lmodern``.



Alternative latex writers
*************************

Browse the `SVN repository at berlios.de`__

__ http://svn.berlios.de/viewvc/docutils/trunk/sandbox/latex-variants/

* The `docs` dir contains generic documentation and discussion.

  Writer specific documentation is placed in the respective "literate"
  source.

latex2e variants
================

Mainly of historic interest. The experiences gained with the alternative
writers will go into the main latex2e writer.

* `latex2e_external_stylesheet` replaces most of the generated preamble by
  a latex stylesheet.

* `latex2e_headings_as_multi_line_strings.py` tries to make the latex2e
  writer source more readable by the use of raw literal strings
  instead of escaped backslashes.

* `latex2e_listings` provides an alternative implementation of the new
  ``--literal-env`` option to select the environment for
  typesetting literal blocks.

  This options allows e.g. to choose `lstlisting` from the ``listings.sty``
  LaTeX package. See also `<docs/syntax-highlight-with-listings.html>`_

other latex writers
=========================

Currently none.

Tests
*****

Test documents and unit tests highlighting problems with the LaTeX
export and testing alternatives.


See `<tests>`_

Related sandbox projects
************************

* `<../dkuhlman/Docs>`__,
* `<../docpy-writer>`__,
* `<../jensj/latex_math>`__,
* `<../latex_directive>`__, and
* `<../verse-writer>`__.

.. References
   ==========

.. _PSNFSS documentation:
   http://dante.ctan.org/CTAN/macros/latex/required/psnfss/psnfss2e.pdf
.. _transition element:
   http://docutils.sourceforge.net/docs/user/rst/quickref.html#transitions
.. _Docutils Release Notes:
   http://docutils.sourceforge.net/RELEASE-NOTES.html
.. _Docutils History:
   http://docutils.sourceforge.net/HISTORY.html
