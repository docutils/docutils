====================================
 Docutils_ for python documentation
====================================

:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

This tries to explore the posibilities to use docutils as format for python
library documentation as specified in 
`Documenting Python <http://docs.python.org/dev/doc/doc.html>`_.

This version is based on a script from Greg Ward, hacked by Thomas Heller.

There is also 

* `Edward Loper's sandbox
  <http://docutils.sf.net/sandbox/edloper/docpy/>`__.  The example 
  "asyncore.rst" file was originally adapted by Steve Holden and Bill
  Sconce.

* `Dave Kuhlman's sandbox
  <http://docutils.sf.net/sandbox/dkuhlman/>`__ and `his homepage
  <http://www.rexx.com/~dkuhlman/rstpythonlatex_intro.html>`__.


Problems
--------

* An unpatched latex2html is unable to handle ``longtable`` options.
  Maybe remove the ``[c]`` and put the longtable into a center environment,
  but python doc uses ``tablei`` and `` longtablei``.
* (th) the table in the ctypes tutorial has a totally different look than the other
  tables in the docs.  Compare 
  `ctypes <http://docs.python.org/dev/lib/ctypes-simple-data-types.html>`_
  with `std pydoc <http://docs.python.org/dev/lib/module-struct.html>`__ .
* (th) feature request: it would be very nice if it were possible to generate links
  into the index for functions and types from the rest sources.
* Document ``markup.py`` and ``missing.py``.
* Title, author, ... are hardcoded.

Change log
----------

2006-04-28 

* mkpydoc.py:

  - Patch for python 2.3.
  - Filenames from command line.
  - Guard definition of ``locallinewidth`` against redefinition.
  - latex needs definition of ``locallinewidth``.

2006-04-xx  

* mkpydoc.py - from theller, ctypes repository.




.. _Docutils: http://docutils.sourceforge.net/
