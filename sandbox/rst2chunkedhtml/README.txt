===============
rst2chunkedhtml
===============

------------------------------------
An HTML Chunker for reStructuredText
------------------------------------


:Author: Andras Mohari <mayday at vizslamail dot hu>
:Date: $Date$
:Status: *Experimental*
:Copyright: This document has been placed in the public domain.

:Abstract: ``rst2chunkedhtml`` reads standalone reStructuredText source files
   and produces chunked HTML output. It chunks sections up to a specified
   depth, and uses the DocUtils HTML writer to convert the chunks to HTML.

.. contents::
.. sectnum::


Requirements
============

* Python 2.4 or newer.
* A recent development version of DocUtils.
* `This patch`_. It adds chunking
  support to the HTML writer (``docutils/writers/html4css1/__init__.py``).

.. _This patch:
.. _patch: html4css1-external-refs.patch


Setting Up
==========

To try rst2chunkedhtml, grab the latest development version of DocUtils, copy
the ``docutils`` Python package to a directory of your choice, and apply the
patch_ to it. Finally adjust your PYTHONPATH environment variable so that the
package can be imported.


Quick Start
===========

Let's chunk a file called ``demo.txt``::

   mkdir demo
   rst2chunkedhtml --chunk-depth=0 demo.txt demo/index.html

This chunks all sections, writes the root chunk to ``demo/index.html``, and
all other chunks to ``demo/<id>.html``, where ``<id>`` is the section ID. As
you can see above, the directory must already exist, it will not be created
for you. Note that if a chunked section node has an ID called ``index``, the
filename of the chunk would conflict with that of the root chunk, and
rst2chunkedhtml will exit with an error.


Usage
=====

You can invoke ``rst2chunkedhtml`` the same way you would invoke ``rst2html``::

   rst2chunkedhtml [options] [<source> [<destination>]]

TODO

.. vim: set tw=78 ts=3 sw=3 sts=3 et ft=rst:
