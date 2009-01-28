..  #!/usr/bin/env python
  # -*- coding: utf8 -*-
  # $Id$
.. -*- rst-mode -*-

rst2pdf README
==============

`<rst2pdf.py>`_ is a PDF front-end for Docutils that is compatible
with the ``rst2*.py`` front ends of the Docutils_ suite.
It enables the generation of PDF documents from a reStructuredText source in
one step cleaning up intermediate files.

``rst2pdf.py`` is implemented as a combination of Docutils' ``rst2latex.py``
and rubber_ by Emmanuel Beffara.

Copyright: © 2008 Günter Milde
	   Licensed under the `Apache License, Version 2.0`_
           Provided WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND

Requirements
------------

Docutils_
  for the reStructuredText -> LaTeX conversion

pdflatex and friends
  A recent LaTeX installation including pdflatex.

rubber_
  Rubber is a program whose purpose is to handle all tasks related to the
  compilation of LaTeX documents. This includes compiling the document
  itself, of course, enough times so that all references are defined, and
  running BibTeX to manage bibliographic references. Automatic execution of
  dvips to produce PostScript documents is also included, as well as usage
  of pdfLaTeX to produce PDF documents.
  
  Rubber packages exist for Debian, Gentoo, FreeBSD, NetBSD, Fink and
  Darwin.

  The current development version contains many bug fixes: `rubber snapshot`_

Installation
------------

Copy the file rst2pdf.py_ into a directory in the operating
system's PATH for executable binaries or create a link.

Usage
-----
::

  rst2pdf.py [options] [<source> [<destination>]]

Get a list of supported options with ``rst2pdf.py --help``.

Configuration
-------------

``rst2pdf.py`` uses the "latex" writer for conversion from reStructuredText
which is configured in the ``[latex2e writer]`` section of the configuration
files. (See `Docutils Configuration`_)

Currently, the options for ``rubber`` are "hardcoded" in rst2pdf.py_.
Therfore, the configuration of the LaTeX -> PDF conversion requires changing
the settings directly in the (well documented) rst2pdf.py_ file.

Troubleshooting
---------------

In case of errors, reproduce the document-generating steps by hand to
localise the problem:

* export the document with ``rst2latex.py INFILE OUTFILE``

* convert to PDF with ``rubber --pdf OUTFILE`` 
  
  If this fails, try ``pdflatex OUTFILE``

Send bug reports to the author or the docutils-users list.

.. References

.. _Docutils: http://docutils.sourceforge.net/
.. old _rubber site: http://www.pps.jussieu.fr/~beffara/soft/rubber/
.. _rubber: http://iml.univ-mrs.fr/~beffara/soft/rubber/
.. _rubber snapshot: http://iml.univ-mrs.fr/~beffara/soft/rubber/rubber-20080323.tar.gz
.. _Apache License, Version 2.0: http://www.apache.org/licenses/LICENSE-2.0

.. _Docutils Configuration:
      http://docutils.sourceforge.net/docs/user/config.html
