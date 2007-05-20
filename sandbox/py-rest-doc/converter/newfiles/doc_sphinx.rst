.. highlightlang:: rest

The Sphinx build system
=======================

XXX: intro...

.. _doc-build-config:

The build configuration file
----------------------------

The documentation root, that is the ``Doc`` subdirectory of the source
distribution, contains a file named ``conf.py``.  This file is called the "build
configuration file", and it contains several variables that are read and used
during a build run.

These variables are:

release : string
   A string that is used as a replacement for the ``|release|`` reST
   substitution.  It should be the full version string including
   alpha/beta/release candidate tags, e.g. ``2.5.2b3``.

version : string
   A string that is used as a replacement for the ``|version|`` reST
   substitution.  It should be the Python version the documentation refers to.
   This consists only of the major and minor version parts, e.g. ``2.5``, even
   for version 2.5.1.

today : string
   A string that contains the date that should be written to the documentation
   output.  Normally, this is today's date and has the format ``April 14,
   2007``.

unused_file : list of strings
   A list of reST filenames that are to be disregarded during building.  This
   could be docs for temporarily disabled modules or documentation that's not
   yet ready for public consumption.

last_updated_format : string
   If this is not an empty string, it will be given to ``time.strftime()`` and
   written to each generated output file after "last updated on:".

strip_trailing_parentheses : bool
   If true, trailing parentheses will be stripped from ``:func:`` etc.
   crossreferences.