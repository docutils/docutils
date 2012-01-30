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
Sandbox is mirrored at the Docutils web site, so the `Docutils Web Site`_
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

See the `sandbox/`_ directory for a complete list.

Some Sandbox projects, are also sorted into the `Docutils Link List`_.

.. _Docutils Link List: http://docutils.sf.net/docs/user/links.html

.. TODO: update the list of important sandbox projects (here or in the link
   list?)

   The remainder of this section contained descriptions and links to obsolete
   or abadoned projects. Commented out because this give an impression of an
   attic instead of a place for experimenting.

  Projects that are now implemented in Docutils
  ---------------------------------------------
  
  * `<code-block-directive>`_ contains documentation, reasoning and
    experimental code for support of syntax highlight in Docutils.
    Preparational work for the `code`_ directive available since version 0.9.
  
  * `LaTeX math for reST`_ by Jens J. Mortensen writes Math to
    LaTeX or MathML. This is the base of math_ support since version 0.8.
  
  .. _code: docutils/docs/ref/rst/directives.txt#code
  .. _math: docutils/docs/ref/rst/directives.txt#math
  .. _LaTeX math for reST:
     http://docutils.sourceforge.net/sandbox/jensj/latex_math/
  
  Documenting Python
  ------------------
  
  Since Python 2.6, the markup used for the official `Python documentation`_
  is `reStructuredText`_, amended by custom directives. The Sphinx_ toolset is
  used to generate the HTML and LaTeX versions.
  
  The sandbox contains some of the pervious work done on enabling Docutils to
  write the `"Documenting Python"`__ using rST.
  
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
  
  .. _Python documentation: http://docs.python.org/
  .. _reStructuredText: http://docutils.sf.net/rst.html
  .. _Sphinx: http://sphinx.pocoo.org/
  
  Project Infrastructure
  ----------------------
  
  `docutils-update <infrastructure/docutils-update>`_ is a script that
  was installed as a cron job on BerliOS.de to automatically update the
  Docutils `web site`_ whenever the SVN files change.  Any .html
  document with a corresponding .txt file is regenerated whenever the
  .txt file changes.
  
  .. _web site: http://docutils.sf.net/docs/dev/website.html
