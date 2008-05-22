..  #!/usr/bin/env python
  # -*- coding: utf8 -*-
  # $Id$
.. -*- rst-mode -*-

rst2pdf README
==============

`<rst2pdf.py>`_ is a PDF front-end for docutils that is compatible
with the ``rst2*.py`` front ends of the docutils_ suite.
It enables the generation of PDF documents from a reStructuredText source in
one step cleaning up intermediate files.

``rst2pdf.py`` is implemented as a combination of docutils' ``rst2latex.py``
by David Goodger and rubber_ by Emmanuel Beffara.

Copyright: © 2008 Günter Milde
	   Licensed under the `Apache License, Version 2.0`_
           Provided WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND

Requirements
------------

docutils_
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

Installation
------------

Copy or link the file ``rst2pdf.py`` into a directory in the operating
systems PATH for executable binaries.

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

No configuration of the latex -> PDF conversion is currently possible
(except changing the settings directly in the (well documented)
``rst2pdf.py`` file itself).

Troubleshooting
---------------

In case of errors, try to export the document with ``rst2latex.py`` and
convert to PDF "by hand" -- either with rubber or running pdflatex, bibtex,
etc. the requested number of times. This should help to locate the error.

Send bugreports to the author or the docutils-users list.

.. References

.. _docutils: http://docutils.sourceforge.net/
.. _rubber: http://www.pps.jussieu.fr/~beffara/soft/rubber/
.. _Apache License, Version 2.0: http://www.apache.org/licenses/LICENSE-2.0

.. _Docutils Configuration:
      http://docutils.sourceforge.net/docs/user/config.html
