Introduction
============

A docutils script converting restructured text into Beamer-flavoured LaTeX.

Beamer is a LaTeX document class for presentations. rst2beamer [#homepage]_
provides a Docutils [#docutils]_ writer that transforms restructured text
[#rst]_ into Beamer-flavoured LaTeX. and provides a commandline script for the
same. Via this script, ReST can therefore be used to prepare slides and
presentations.


Installation
============

rst2beamer can be installed via setuptools / easy_install or manually. Consult
the enclosed manual for further details.


Usage
=====

rst2beamer should interpret a reasonable subset of restructured text and
produce reasonable LaTeX. Not all features of beamer have been implemented,
just a (large) subset that allows the quick production of good looking slides.
These include:

* Overlay lists (i.e. list items that appear point-by-point)
* Beamer themes
* Automatic centering and resizing of figures
* Embedded notes and the output of note slides.
* Arranging slide contents into columns.

Some examples can be found in the ``docs/examples`` directory of the
distribution. In practice, rst2beamer can be used with ``pdflatex`` to get PDF
versions of a presentation.

.. note::

	Depending on your platform, the script may be installed as ``rst2beamer``,
	or ``rst2beamer.py``.

rst2beamer is called::

  rst2beamer [options] [<source> [<destination>]]

For example::

	rst2beamer infile.txt outfile.tex

where ``infile.txt`` contains the ReST and ``outfile.tex`` contains the
produced Beamer LaTeX.

It supports the usual docutils and LaTeX writer (rst2latex) options, save the
``documentclass`` option (which is fixed to ``beamer``) and hyperref options
(which are already set in beamer). It also supports:

--theme=THEME           Specify Beamer theme.
--overlaybullets=OVERLAYBULLETS
                        Overlay bulleted items. Put [<+-| alert@+>] at the end
                        of \begin{itemize} so that Beamer creats an overlay
                        for each bulleted item and the presentation reveals
                        one bullet at a time
--centerfigs=CENTERFIGS
                        Center figures.  All includegraphics statements will
                        be put inside center environments.
--documentoptions=DOCUMENTOPTIONS
                        Specify document options. Multiple options can be
                        given, separated by commas.  Default is
                        "10pt,a4paper".
--shownotes=SHOWNOTES   Print embedded notes along with the slides. Possible
                        arguments include 'false' (don't show), 'only' (show
                        only notes), 'left', 'right', 'top', 'bottom' (show in
                        relation to the annotated slide).


Limitations
===========

Earlier versions of rst2beamer did not work with docutils 0.4, seemingly due
to changes in the LaTeX writer. While this has been fixed, most work has been
done with docutils snapshots from version 0.5 and up. In balance, users are
recommended to update docutils.

More recently, changes in the LaTeX writer in docutils 0.6 broke rst2beamer
again. We think all those bugs have been caught.

Not all features of beamer are supported, and some - that deal with with page
layout or presentation - may never be. Introducing complex syntax to achieve
complex and specific page effects defeats the point of ReST's simple and
easy-to-write format. If you need a complex presentation, use Powerpoint or
Keynote.

If the content for an individual slide is too large, it will simply overflow
the edges of the slide and disappear. Arguably, this is a sign you should put
less on each slide.


Credits
=======

rst2beamer is developed by `Ryan Krauss <ryanwkrauss@gmail.com>`__ and
`Paul-Michael Agapow <agapow@bbsrc.ac.uk>`__. Thanks to those who reported and
helped us track down bugs: Perttu Laurinen, Mike Pennington, James Haggerty
and Dale Hathaway.


References
==========

.. [#homepage] rst2beamer homepages at `agapow.net
   <http://www.agapow/net/software/rst2beamer>`__ and `cs.siue.edu
   <http://home.cs.siue.edu/rkrauss/python_website/>`__

.. [#docutils] `Docutils homepage <http://docutils.sourceforge.net/>`__

.. [#rst] `Restructured text <http://docutils.sourceforge.net/rst.html>`__

.. [#beamer] `Beamer homepage <http://latex-beamer.sourceforge.net/>`__

