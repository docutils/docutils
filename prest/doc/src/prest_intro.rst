======================
Introduction to Prest
======================
:Author: Mark Nodine
:Contact: mnodine@alum.mit.edu
:Revision: $Revision: 762 $
:Date: $Date: 2006-01-27 11:47:47 -0600 (Fri, 27 Jan 2006) $
:Copyright: This document has been placed in the public domain.

.. contents::

--------------
 Introduction
--------------

This document gives an introduction to the ``prest`` program, which is
a Perl implementation of a reStructuredText_ parser (and writers) and
gives a map of the documentation.

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

reStructuredText_ is an easy-to-read, what-you-see-is-what-you-get
plaintext markup syntax and parser system.  It is useful for inline
program documentation (such as Python docstrings), for quickly
creating simple web pages, and for standalone documents.  This web
page was generated from reStructuredText.  The original implementation
of a reStructuredText parser is that of the docutils_ project, written
in Python.  The "prest" name is an acronym for "***P***\ erl
***reS***\ tructured\ ***T***\ ext".

.. _docutils: http://docutils.sourceforge.net

---------------
 Documentation
---------------

Since prest is a re-implementation of a reStructuredText parser, it has
diverged from the original Python version, both because the Python
version has made changes not tracked by the Perl version and because
the Perl version has added some innovations of its own.  So the
documentation references have potentially three versions: the prest
version, the Docutils version, and a difference between the two.
The difference is based on the latest Docutils version available when
the prest documentation was compiled.

======================================== ================= ==================== ======
A ReStructuredText Primer                                  `docutils version`__
Quick reStructuredText                                     `docutils version`__
An Introduction to reStructuredText      `prest version`__ `docutils version`__ diff__
reStructuredText Markup Specification    `prest version`__ `docutils version`__ diff__
reStructuredText Directives              `prest version`__ `docutils version`__ diff__
reStructuredText Interpreted Text Roles  `prest version`__ `docutils version`__ diff__
Usage of Prest                           `prest version`__
Internals of Prest                       `prest version`__
How to Extend Prest                      `prest version`__
======================================== ================= ==================== ======

__ http://docutils.sourceforge.net/docs/rst/quickstart.html

__ http://docutils.sourceforge.net/docs/rst/quickref.html

__ introduction.html
__ http://docutils.sourceforge.net/spec/rst/introduction.html
__ introduction_diff.html

__ reStructuredText.html
__ http://docutils.sourceforge.net/spec/rst/reStructuredText.html
__ reStructuredText_diff.html

__ directives.html
__ http://docutils.sourceforge.net/spec/rst/directives.html
__ directives_diff.html

__ roles.html
__ http://docutils.sourceforge.net/spec/rst/roles.html
__ roles_diff.html

__ prest_usage.html

__ prest_internals.html

__ prest_extend.html

..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
