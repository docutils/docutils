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

`docutils-update <infrastructure/docutils-update>`_ is a script that
is installed as a cron job on BerliOS.de to automatically update the
Docutils `web site`_ whenever the SVN files change.  Any .html
document with a corresponding .txt file is regenerated whenever the
.txt file changes.

.. _web site: http://docutils.sf.net/docs/dev/website.html


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


Other Sandbox Projects
----------------------

For other sandbox projects, please see the `Docutils Link List`_.

.. _Docutils Link List: http://docutils.sf.net/docs/user/links.html
