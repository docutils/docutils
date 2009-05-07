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

#. Did you have problems with Find or Export of words with
   non-ASCII chars (e.g. Umlauts) in the PDF reader (Acrobat, xpdf, ...)

   Which font encoding do you prefer for the output?

#. Does the latex2e writer need a `--stylesheet-path option`_?

#. Feature wishes or ideas of improvement for the `latex2e` writer.


Users of ``rst2latex2.py`` are encouraged to respond by mail to the
docutils-users list.

Proposed Changes
================

See also the notes in
http://docutils.sourceforge.net/docs/user/latex.html#problems
and the TODO list in
http://docutils.sourceforge.net/docs/dev/todo.html#latex-writer


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
  | -2  very very wide
Charter
  | +0  nonspectacular
  | -1  no "Base35" font
New Century Schoolbook
  | -1  very wide
Palatino
  | +1  recommended by font experts
  | +1  good LaTeX support including matching math fonts, small caps,
    	old-style figures
  | -1  bad rendering in xpdf viewer (auto-hinting leads to different
        x-hight for different characters at some magnifications)
Times
  | +1  'de facto standard'
  | -1  overused
  | -1  narrow (devised for multi-column layouts)
Utopia
  | +1  recommended by font experts
  | -1  no `Base35` font, no required part of LaTeX any more.

Implement as default stylesheet option, so it can be easily overridden.

   My vote is for Palatino.
   --Matthew Leingang


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

-1 having both ``--stylesheet`` and ``stylesheet-path`` makes things
   complicated:

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
   |_ mystyle.sty
   |_ docutils.conf
   |_ A/
   |  |_ a.txt
   |  | ...
   |_ B/
      |_ b.txt


  With the line ::

   stylesheet-path: mystyle.sty

  in docutils.conf, all documents will get a valid link to the style file,
  if the conversion is started from the base dir.


Require only standard packages
------------------------------

Provide fallback solutions for the use of "exotic" packages in case they do
not exist on the target system.

.. \@ifpackageloaded{} \IfPackageExists{}



Bibliography directive
----------------------

Alan suggested a bibliography directive so that BibTex references are not
automatically placed at the end (and bibtex for html via ... can be supported)

This would imply supression of the "Bibliography" or "References" header
added by LaTeX which is possible with:

  From: Martin Scharrer <martin@scharrer-online.de>
  Subject: Re: Literaturverzeichnis ?berschrift entfernen
  Date: Sun, 22 Mar 2009 06:30:33 -0700 (PDT)

  On Mar 21, 11:10?pm, Elfish26111...@gmail.com wrote:
  > Hallo,
  >
  > ich benutze Bibtex und w?rde gerne die komplette ?berschrift, also
  > auch die daf?r reservierte Box entfernen.
  > Hat jemand eine Idee wie ich das machen kann?
  >
  > \renewcommand{\bibname}{}
  >
  > habe ich bereits versucht. Aber wie gesagt den reservierten Platz in
  > der Überschrift behält er immernoch.

  Die Ueberschrift ist denk ich mal ein '\chapter*{\bibname}' was du
  dann zu einem '\chapter*{}' machst.
  Die Loesung ist einfach das \chapter macro umzudefinieren:

  \begingroup
  \renewcommand{\chapter}[2]{}
  % Literaturverzeichnis einbinden: ueber BibTeX oder manuell ...
  \endgroup

  Wobei der '*' hier einfach als ein zusaetzliches Argument genommen
  wird.
  Dann braucht \bibname gar nicht mehr geaendert werden.

  Gruesse,
  Martin



Implemented Changes
===================

Changes to the latex2e writer in the SVN version since docutils version 0.5
are documented in the compatibility style sheet latex2e-compat_.

Also see the `Docutils Release Notes`_, the `Docutils History`_.

.. Steps for uploading Changes:

  * provide patch to latex2e/__init__.py
  * get approvement from the latex writer maintainer (Engelbert Gruber)

  * (add and) run functional and unit- tests::

       ../../docutils/test/test_functional.py
       ../../docutils/test/test_writers/test_latex2e.py

    or all at once::

       cd ../../docutils/test/
       ./alltests.py

    + if output changed: test-compile and approve new output in
      ../../docutils/test/functional/output/

  * Documentation

    * remove from TODO list in ../../docutils/docs/dev/todo.txt

    * describe new end-user features in ../../docutils/docs/user/latex.txt

    * add backwards-incompatible changes to ../../docutils/RELEASE-NOTES.txt
    * add summary of changes to ../../docutils/HISTORY.txt
    * describe change in ../../docutils/docs/user/docutils-05-compat.sty.txt


Alternative latex writers
*************************

Browse the `SVN repository at berlios.de`__

__ http://svn.berlios.de/viewvc/docutils/trunk/sandbox/latex-variants/

* The `docs` dir contains generic documentation and discussion.

  Writer specific documentation is placed in the respective "literate"
  source.

other latex writers
===================

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

.. _latex.html: ../../docutils/docs/user/latex.html
.. _latex2e-compat:
   ../../docutils/docs/user/docutils-05-compat.sty.html

.. _font encoding: ../../docutils/docs/user/latex.html#


.. _PSNFSS documentation:
   http://dante.ctan.org/CTAN/macros/latex/required/psnfss/psnfss2e.pdf
.. _Docutils Release Notes:
   http://docutils.sourceforge.net/RELEASE-NOTES.html
.. _Docutils History:
   http://docutils.sourceforge.net/HISTORY.html
