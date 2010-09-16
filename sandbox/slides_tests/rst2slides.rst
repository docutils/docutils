============================
Slides from reStructuredText
============================

:author: Alan Glen Isaac
:date: 2010-09-06
:source: `rst2slides.rst`_

.. _rst2slides.rst: rst2slides.rst

A Need
======

Slide writers need one or more test documents
that all slide writers should be able to handle.
(See below.)

The present document has very low requirements.
It does not include:

- subtitle
- sections and subsections
- images and tables
- overlays
- math

Therefore it is not an adequate test document.
But certainly all writers should be able to handle the present document.

Test Suite
==========

Here is a test suite based on the discussion below.

- http://subversion.american.edu/aisaac/misc/slides_test01.rst
- http://subversion.american.edu/aisaac/misc/slides_test02.rst
- http://subversion.american.edu/aisaac/misc/slides_test03.rst
- http://subversion.american.edu/aisaac/misc/slides_test04.rst


Some Problems
=============

1. There is no settled reST slide format, so writers
   do not offer consistent handling
   The test documents are intended to lead toward
   greater uniformity.

2. The lack of math support in the docutils core is pretty devastating.
   (Jens’s work should be moved to the docutils core,
   at least as an “experimental” feature.)
   However some writers implement math support independently.


Core Goal
=========

Implementing slides with standard reST sectioning

- is easiest to write
- looks a lot better as text

Therefore sections should be the basic way to determine
what is a slide.


Sectioning
==========

Proposal: the lowest section level present should always treated as a slides.

What about sections within slides?
That what the ``topic`` or ``rubric`` directives are for!

Some writers must change to implement this.
E.g., rst2beamer takes this approach, but rst2s5 does not.
For backwards compatibility,
this default could be overridden with an option:
``--slide-section-level=N``

Note that docutils imposes section-level consistency.
So if you want any slide in a sections and a subsection,
the first slides must be in a section and in a subsection.
(Because slides are based on sectioning.)




Other Objects as Slides
=============================

Guenter Milde suggests that slide writers should recognize a
``slide`` class on objects.
An object with the slide class always generates a new slide.

This would be excellent and very flexible.
In this setting, Milde proposed that the above-mentioned option
``--slide-section-level=N``
simply tag all sections at that level.

If the default is ``--slide-section-level=-1``
and that means "tag the lowest subsection level".
This has a good fit with the previous proposed solution.

If two nested objects are both tagged with the ``slide`` class,
the initial content of the first goes on a slide,
the content of the second goes on a second slide,
and any left-over content from the first then goes on a third slide.


Additional Proposals for Slide Writers
======================================

Recognize the following special class arguments
(proposed by G. Milde):

overlay
   place object on a new "overlay" (which gives the appearance of
   incremental exposure of a given slide).
notesonly
   do not place object on any slide (i.e. ignore when producing slides)
   (Achievable via docutils’ ``strip-elements-with-class`` setting).
slidesonly
   do not place object in the notes (i.e. when generating
   standard HTML/LaTeX/PDF output for the handout or notes).
   (Achievable via docutils’ ``strip-elements-with-class`` setting).


Additional Proposals for Slide Writers
======================================

Additionally:
it is important to be able to tag lists (at least)
as say ``incremental``, to signal to the writer to generate
a sequence of overlays.

rst2beamer currently allows users to tag any slides
with ``overlay`` or ``nooverlay``, which override
the default.

As a LaTeX specific attribute, Ryan Krauss
also added ``fragile`` and ``notfragile``
class attribute support for beamer slides.
This is a good idea for any LaTeX slide writer.




HTML Slides: rst2s5
====================

rst2s5 is part of the standard docutils distribution.

::

  rst2s5 --theme=small-white slides.rst slides.html

There is a useful introduction: http://docutils.sourceforge.net/docs/user/slide-shows.html



rst2s5 Limitations
==================

- no sectioning.
- browser font resizing is disabled
- no math directive or role


rst2s5 Improvement Suggestions
==============================

- allow slides to be within sections and subsections

  - display these instead of the title at the bottom


PDF Slides: rst2pdf
===================

Solution: ``rst2pdf``

- available on Pypi.
- available from http://code.google.com/p/rst2pdf/

::

  rst2pdf -b2 -s a4-landscape -o c:\temp\temp.pdf slides.rst 

There is a helpful `rst2pdf manual`_
You may also want to look Roberto Alsina's `slides.style`_.

.. _rst2pdf manual: http://rst2pdf.googlecode.com/svn/trunk/doc/manual.txt
.. _slides.style: http://lateral.netmanagers.com.ar/static/rst2pdf-slides/slides.style


rst2pdf Limitations
===================

rst2pdf has

- math directive and role
- great flexibility of style

So with the right style file, there may be no limitations
relevant to this summary.  But

- images: upgrade to latest ReportLab to get PNG images to work;
  there is no support for PDF 1.6 images
- I don't know how to do incremental revelation of slide material
- I like beamer's handling of sections and subsections
  (i.e., display in headers or footers, not on separate slides)
  but I don't know how to get that from rst2pdf
 

PDF Slides: rst2beamer
======================

Solution: ``rst2beamer``

- old version available on Pypi (authors are Ryan Krauss and Paul-Michael) has some bugs.
- working version is in docutils sandbox has fewer bugs and more features

::

  rst2beamer slides.rst slides.tex
  pdflatex slides.tex slides.pdf

Quick notes: http://www.agapow.net/software/rst2beamer

Comment: this solution produces very good looking output.

.. Ryan Krauss rkrauss@siue.edu
.. Paul-Michael Agapow (pma@agapow.net)


rst2beamer Limitations
======================

- no math directive or role (but can add math as raw LaTeX)
- unlike rst2latex, Unicode characters are not translated
- literal text handling is partly broken; it should copy the
  approach in rst2latex
- suppose you want a one row, two-column table with a figure
  in one cell and a related list in the other (i.e., a standard
  presentation slide style).  rst2beamer will not be correctly format this.
  (Nor will rst2latex, for that matter.)

Unhappy defaults:

- every slide is assigned ``fragile`` option -> amsmath is broken
- every slide is incremental by default (ugh!).
  However, can turn off incremental for the whole set with ``--overlaybullets=False``.
  Can then turn on on per-slide basis (with ``overlay`` class)

 
rst2beamer Improvement Suggestions
==================================

- Don’t make slides fragile and incremental by default.
- handle literal text like rst2latex does; the current
  approach does not work correctly
- handle math like rst2latexmath does
 
rst2beamer Improvement Suggestions
==================================

Here is a reason not to set the ``fragile`` option by default:

    12.9     Verbatim and Fragile Text
    If you wish to use a {verbatim} environment in a frame,
    you have to add the option [fragile] to the {frame}
    environment. In this case, you really have to use the
    {frame} environment (not the \frame command) and the
    \end{frame} must be alone on a single line. Using this
    option will cause the frame contents to be written to an
    external file and then read back. See the description of
    the {frame} environment for more details.

 
rst2beamer Improvement Suggestions (continued)
==============================================

An example implication: suppose you put in a slide::

        .. raw:: latex

           \[ a = b \]

Then rst2beamer will write a document that won't compile
(because the fragile option is set, but ``\end{frame}``
will not be on its own line).  However this can be fixed
by adding an reST comment line.



ODP Slides
==========

Solution: rst2odp

- available on PyPI
- development version in docutils sandbox

::

  rst2opd slides.rst slides.odp



rst2odp Limitations
======================

- does not have a math directive or role
- does not handle subtitle
- does not handle citations
- I have not been able to get it to work


PowerPoint Slides
=================

Solution: rst2outline http://docutils.sourceforge.net/sandbox/rst2outline/

::

   rst2outline slides.rst slides.txt
   powerpnt.exe slides.txt

This solution takes advantage of how PowerPoint reads plain text files
(as described at http://www.pptfaq.com/FAQ00246.htm).

rst2outline Limitations
=======================

- only handles text
- no title, subtitle, or sectioning


