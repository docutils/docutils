=====================
 README: The Sandbox
=====================

The Sandbox_ is a place to play around, to try out and share ideas.
It's a part of the Subversion repository but it isn't distributed as
part of Docutils_ releases.  Each developer who wants to play here
should create their own subdirectory (suggested name: SourceForge ID,
or given name + family initial).

It's OK to make a mess here!  But please, play nice.

For more information, please see the `Docutils Project Policies`_.

.. This link gives readers a way to *get to* the sandbox:
.. _Sandbox: ./

.. _Docutils: http://docutils.sourceforge.net/
.. _Docutils Project Policies:
   http://docutils.sourceforge.net/docs/dev/policies.html#the-sandbox


Sandbox Projects
================

.. contents:: :local:


Project Infrastructure
----------------------

`docutils-update <davidg/infrastructure/docutils-update>`_ is a script
that is installed as a cron job on BerliOS.de to automatically update
the Docutils_ web site whenever the SVN files change.  Any .html
document with a corresponding .txt file is regenerated whenever the
.txt file changes.


Documenting Python
------------------

Some work has been done on enabling Docutils to write the
`"Documenting Python" variant of LaTeX`__, which we're calling
"DocPy".

__ http://docs.python.org/doc/doc.html

* `Edward Loper's sandbox
  <http://docutils.sf.net/sandbox/edloper/docpy/>`__.  The example 
  "asyncore.rst" file was originally adapted by Steve Holden and Bill
  Sconce.

* `Dave Kuhlman's sandbox
  <http://docutils.sf.net/sandbox/dkuhlman/>`__ and `his homepage
  <http://www.rexx.com/~dkuhlman/rstpythonlatex_intro.html>`__.


----------------------------------------------------------------------


**To-do:** The following list must be updated.

Other Sandbox Projects
----------------------

Anyone is welcome to contribute to any of these projects.  Interested
developers are welcome to take on any projects which appear to be
dormant.  Please direct any email to the Docutils-develop_ mailing
list.

.. _Docutils-develop:
   http://docutils.sf.net/docs/user/mailing-lists.html#docutils-develop

* ZReST_, by Richard Jones, is a "ReStructuredText Document for Zope"
  application that is complete and ready to install.

* PySource_, by Tony Ibbs, is an experimental Python source Reader.
  In some form, it will soon become part of core Docutils.  There is
  some related code in David Goodger's sandbox (pysource_reader_) and
  a `Python Source Reader`_ document.

* Docutils interface to PythonPoint_, also by Richard Jones, produces
  PDF presentations using ReportLabs.

* Engelbert Gruber has begun `LaTeX Writer`_ and `ManPage Writer`_ components.

* ? has taken over `ReportLabs/PDF Writer`_ components.

* Oliver Rutherfurd has begun a `DocBook Writer`_ component and
  `HT2HTML integration`_ component.

* Gunnar Schwant's DocFactory_ is a wxPython GUI application for
  Docutils.

* Aahz has begun an `OpenOffice.org Writer`_.

* Ian Bicking is working on code for a Wiki_.

* Bill Bumgarner has written a `simple HTML writer`_ that doesn't rely
  on CSS (stylesheets).

* Beni Cherniavsky has written a generic `preprocessing module`_ for
  roles and/or directives and built preprocessors for TeX math for
  both LaTeX and HTML output on top of it.

* Beni Cherniavsky maintains a Makefile_ for driving Docutils, hoping
  to handle everything one might do with docutils.

.. _ZReST: richard/ZReST/
.. _PySource: tibs/pysource/
.. _pysource_reader: davidg/pysource_reader/
.. _Python Source Reader: http://docutils.sf.net/docs/dev/pysource.html
.. _PythonPoint: richard/pythonpoint/
.. _LaTeX Writer: grubert/latex/
.. _Manpage Writer: grubert/man/
.. _ReportLabs/PDF Writer: dreamcatcher/rlpdf/
.. _DocBook Writer: oliverr/docbook/
.. _HT2HTML integration: oliverr/ht/
.. _DocFactory: gschwant/docfactory/doc/
.. _OpenOffice.org Writer: aahz/OO/
.. _Wiki: ianb/wiki/
.. _simple HTML writer: bbum/DocArticle/
.. _preprocessing module: cben/rolehack/
.. _Makefile: cben/make/
