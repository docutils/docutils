..                            -*- rst-mode -*-

=====================
 README: The Sandbox
=====================

The Sandbox_ is a place to play around, to try out and share ideas. The
`sandbox/`_ directory is a part of the Subversion repository but it isn't
distributed as part of Docutils_ releases.

It's OK to make a mess in the Sandbox! But please, play nice.

A project-specific subdirectory should be created for each new project.
Any developer who wants to play in the sandbox may create their own
subdirectory (suggested name: SourceForge ID, or given name + family
initial), but project directories are recommended over personal directories,
which discourage collaboration.

For more information, please see the `Docutils Project Policies`_. The
Sandbox_ is mirrored at the Docutils web site, so the `Docutils Web Site`_
documentation applies as well.


.. These links give readers a way to *go to* the sandbox:
.. _sandbox:
.. _sandbox/: ./

.. _Docutils: http://docutils.sourceforge.net/
.. _Docutils Project Policies:
   http://docutils.sourceforge.net/docs/dev/policies.html#the-sandbox
.. _Docutils Web Site: http://docutils.sourceforge.net/docs/dev/website.html


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

* `docpy-writer <http://docutils.sf.net/sandbox/docpy-writer/>`__ was
  used for ctypes documentation.

Since Python 2.6, the markup used for the official `Python documentation`_
is `reStructuredText`_, amended by custom directives. The Sphinx_ toolset is
used to generate the HTML and LaTeX versions.

.. _Python documentation: http://docs.python.org/
.. _reStructuredText: http://docutils.sf.net/rst.html
.. _Sphinx: http://sphinx.pocoo.org/

.. Could we convert the `Documenting Python`_ section into an item in the
   `Other Sandbox Projects`_ list, as it is obsoleted by the move to Sphinx?
   [GM]


Other Sandbox Projects
----------------------

See the `sandbox/`_ directory for a complete list.

* Many Sandbox projects, are sorted into the `Docutils Link List`_.

* `<code-block-directive>`_ contains documentation, reasoning and
  experimental code for support of syntax highlight in docutils.

* `<latex-variants>`_ is a place to try out and discuss improvements, fixes
  and variants of the latex2e writer.

* `LaTeX math for reST`_ by Jens J. Mortensen writes Math to
  LaTeX or MathML.

.. _Docutils Link List: ../docutils/docs/user/links.html
.. _LaTeX math for reST:
   http://docutils.sourceforge.net/sandbox/jensj/latex_math/
