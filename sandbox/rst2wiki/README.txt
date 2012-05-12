========
rst2wiki
========

.. contents::


What is rst2wiki?
=================

``rst2wiki`` is a tool that transforms reStructuredText_ into various
Wiki texts. To date ``rst2wiki`` supports two Wiki texts TWiki_ and 
Atlassian Confluence_.


Usage
=====

``rst2wiki.py`` currently has one option ``--wiki`` beyond "General", 
"Parser", and "Standalone" options.

Example::

  # TWiki text
  $ rst2wiki.py --wiki twiki test.rst test.twiki
  
  # Confluence text
  $ rst2wiki.py --wiki confluence test.rst test.confluence

By default ``rst2wiki.py`` will generate TWiki text.


Availability
============

``rst2wiki`` is available through the `Docutils Subversion repository`_
as part of the Docutils sandbox in ``sandbox/rst2wiki``.


Contact
=======

If you have questions, patches, etc ... feel free to contact me at: joshua.graff@ccrypt.org


Installation
============

Currently, ``rst2wiki`` requires you copy the frontend tool ``rst2wiki.py``
and writer ``wiki.py`` into the appropriate directories under your docutils
install.

Example::

  $ cd <rst2wiki sandbox>/tools
  $ dirname `which rst2html.py`
  <bin path>  
  $ cp rst2wiki.py <bin path> 
  $ cd <rst2wiki sandbox>/docutils/writers
  $ cp wiki.py <path to docutils install>/writers


Copyright and license
=====================

Copyright (C) 2012 by Joshua Graff

License is BSD_ 2-Clause


TODO
====

* Add some actual automated tests

* Expand support for a few more Wiki dialects


Links
=====

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

.. _TWiki: http://twiki.org

.. _Confluence: http://www.atlassian.com/software/confluence/overview

.. _Docutils Subversion repository: http://docutils.sourceforge.net/docs/dev/repository.html

.. _BSD: http://www.opensource.org/licenses/BSD-2-Clause