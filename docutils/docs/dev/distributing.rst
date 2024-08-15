.. include:: ../header.rst

===============================
 Docutils_ Distributor's Guide
===============================

:Author: Lea Wiemann
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

.. contents::

This document describes how to create packages of Docutils (e.g. for
shipping with a Linux distribution).  If you have any questions,
please direct them to the Docutils-develop_ mailing list.

First, please download the most current `release package`_ and unpack
it.

.. _Docutils: https://docutils.sourceforge.io/
.. _Docutils-develop: ../user/mailing-lists.html#docutils-develop
.. _release package: https://docutils.sourceforge.io/#download


Dependencies
============

Docutils has the following dependencies:

* Python 3.9 or later is required.
  Use ">= Python 3.9" in the dependencies.

* Docutils may optionally make use of the PIL (`Python Imaging
  Library`_ or Pillow_).  If PIL is present, it is automatically
  detected by Docutils.

* Docutils recommends the `Pygments`_ syntax hightlighter. If available, it
  is used for highlighting the content of `code directives`_ and roles as
  well as included source code files (with the "code" option to the include_
  directive).

* Docutils can use the `pycmark`_, `myst`_, or `recommonmark`_ parsers to
  process input in the Markdown format (since Docutils 0.19).

.. _Python Imaging Library:
    https://en.wikipedia.org/wiki/Python_Imaging_Library
.. _Pillow: https://pypi.org/project/Pillow/
.. _Pygments: https://pygments.org/
.. _pycmark: https://pypi.org/project/pycmark/
.. _myst: https://pypi.org/project/myst-docutils
.. _recommonmark: https://pypi.org/project/recommonmark/

.. _code directives: ../ref/rst/directives.html#code
.. _include: ../ref/rst/directives.html#include


Python Files
============

The Docutils Python files must be installed into the
``site-packages/`` directory of Python.  Installing with pip_
should do the trick, but if you want to place the files
yourself, you can just install the ``docutils/`` directory of the
Docutils tarball to ``/usr/lib/python/site-packages/docutils/``.  In
this case you should also compile the Python files to ``.pyc`` and/or
``.pyo`` files so that Docutils doesn't need to be recompiled every
time it's executed.

.. _pip: https://pip.pypa.io/en/stable/


Executables
===========

Executable front-end tools are generated and installed in the binary PATH
by the installer (pip_) from "console_scripts" `entry point`_ definitions.

Alternatively, you may install the ``rst2*.py`` tools from the
``tools/`` directory of the Docutils source package.
On systems that support executable Python scripts, dropping the ``.py``
extension is recommended.

.. _entry point:
    https://packaging.python.org/en/latest/specifications/entry-points/


Documentation
=============

The documentation should be generated using ``buildhtml.py``.  To
generate HTML for all documentation files, go to the ``tools/``
directory and run::

    # Place html4css1.css in base directory.
    cp ../docutils/writers/html4css1/html4css1.css ..
    ./buildhtml.py --stylesheet-path=../html4css1.css ..

Then install the following files to ``/usr/share/doc/docutils/`` (or
wherever you install documentation):

* All ``.html`` and ``.rst`` files in the base directory.

* The ``docs/`` directory.

  Do not install the contents of the ``docs/`` directory directly to
  ``/usr/share/doc/docutils/``; it's incomplete and would contain
  invalid references!

* The ``licenses/`` directory.

* ``html4css1.css`` in the base directory.


Removing the ``.rst`` Files
---------------------------

If you are tight with disk space, you can remove all ``.rst`` files in
the tree except for:

* those in the ``licenses/`` directory because they have not been
  processed to HTML and

* ``user/rst/cheatsheet.rst`` and ``user/rst/demo.rst``, which should
  be readable in source form.

Before you remove the ``.rst`` files you should rerun ``buildhtml.py``
with the ``--no-source-link`` switch to avoid broken references to the
source files.


Other Files
===========

You may want to install the Emacs-Lisp files
``tools/editors/emacs/*.el`` into the appropriate directory.


Configuration File
==================

It is possible to have a system-wide configuration file at
``/etc/docutils.conf``.  However, this is usually not necessary.  You
should *not* install ``tools/docutils.conf`` into ``/etc/``.


Tests
=====

While you probably do not need to ship the tests with your
distribution, you can test your package by installing it and then
running ``alltests.py`` from the ``tests/`` directory of the Docutils
tarball.

For more information on testing, view the `Docutils Testing`_ page.

.. _Docutils Testing: https://docutils.sourceforge.io/docs/dev/testing.html
