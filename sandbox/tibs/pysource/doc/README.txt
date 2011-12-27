pysource
========

:Author: Tibs (tibs@tibsnjoan.co.uk)
:Date: $Date$
:Revision: $Revision$
:Version: 0.1

Ahem
----
This file needs updating - unless this notice has disappeared, treat the
rest of the file as out-of-date...

History
-------
2002-04:

* Name changed from 'pydps' to 'pysource' (in line with a suggestion
  made a while back on the Doc-SIG by David Goodger).

  It's not that that's a great name, just that it's better than 'pydps'
  (especially now that 'dps' has become 'docutils'), and the rather more
  obvious 'pydoc' is already taken. Any suggestions for a better name will
  be gratefully received!

* Converted from the old 'dps' and 'restructuredtext' packages to the
  new 'docutils' package.

* All previous history is elided with the change of name and first release...

Obtaining the package
---------------------
The latest version of 'pysource' may be found in the Docutils sandbox
(tarball_ and browsable_).

.. _tarball: http://docutils.sourceforge.net/docutils-sandbox-snapshot.tgz
.. _browsable: http://docutils.sourceforge.net/sandbox/tibs/pysource/

I hope to keep a copy of the current "released" version at

    http://www.tibsnjoan.co.uk/reST/pysource.tgz

Purpose
-------
This module provides code to

  * parse a Python file using the Python Compiler module, which is
    standard with Python 2.2 and later, and available in the Tools
    directory in earlier versions,
  * extract relevant information, including docstrings,
  * and ultimately produce (in the first instance) HTML documentation
    therefrom.

As a subsidiary capability, it can read a restructuredtext file and produce
HTML from that.

There are obviously other tools which perform similar tasks - see the
accompanying file whythis.rtxt for some comparisons, and an
explanation of why I think it is worth developing this tool
independently.


*** TO HERE ***

Usage
-----
The command ``python pysource/pysource.py --help`` gives the following
information::

  """The command line interface to docutil's Python documentation extractor.

  Usage: ``pysource.py <switches> <inpath> [<outfile>]``

      <inpath> is the path to a package or module.

      <outfile> is the path to the output file. If it's not given, then
      output will be written to a file with the same name as the input
      file, but defaulting to the current directory, and with extension
      derived from the type of output:

      - show   -> ``.show``
      - ast    -> ``.ast``
      - xml    -> ``.xml``
      - html   -> ``.html``
      - pretty -> ``.pretty``

      (unless --stdout is requested). The default is --html.

      Note that progress messages (and ``verb`` information) are written
      to ``sys.stderr``.
  """

      <switches> are:

        -v, --verbose   Report on progress in more detail
        -q, --quiet     Suppress normal progress messages
        -t, --text      The input file is a plain (text) reST file
        -s, --show      Output basic information about the input
        -a, --ast       Output a representation of the AST
        -x, --xml       Output an XML representation of the input
        -h, --html      Output an HTML representation of the input [default]
        -p, --pretty    Output a 'pretty' representation of the input
        -d, --doctest   Treat a reST file as doctest input.
        -h, --help      Show 'help' information
        -n, --new       Use David Goodger's HTML Writer (sort of)
            --stdout    Write output to stdout, instead of a file

I recommend use of the ``--pretty`` option for gaining an understanding of the
DPS tree itself.

Limitations and dependencies
----------------------------
This is beta software, and is still, to some extent, a proof and exploration of
concept.

The following limitations are obvious:

  * Its concept of a "package" is rather limited - it doesn't understand
    sub-packages (i.e., it only copes with a "flat" directory structure).
  * It only produces a single HTML file - a more sophisticated approach
    is clearly needed, particularly for large packages (or even large
    modules).
  * It is not fully integrated with the Docutils HTML Writer, which it
    should be using in preference to my own home-grown approach.
  * The Docutils tree that it produces could use some improvement - in
    particular the Python specific nodes need various design decisions
    to be made.

Also:

  * It doesn't throw away as much information as it should.
  * It doesn't check all assignments for use of global values.
  * It doesn't handle all Python entities that it should.
  * The HTML it produces is pretty yuck, and is *designed* not
    to look terribly nice (although you should remember not to
    ask my opinion of the HTML output by pydoc).
  * Autonumbered footnote resolution is done by the HTML writer,
    which means that it will likely go wrong if it needs to do
    anything with Python source that contains autonumbered
    footnotes in docstrings(!). But it does work (I believe)
    for .rtxt files.
  * Various other Docutils tree transforms that should be applied
    are not yet performed.

Subtler things:

  * The ``--doctest`` mode just pretends that the whole file is
    a single doctest string (i.e., just as if doctest had found
    it as a docstring in a Python file).

    That's quite sensible, except that the current doctest doesn't
    know that it should ignore literal blocks, and thus may find
    apparent Python code where it shouldn't.

It depends on:

  * The latest versions of Docutils, as of the
    time it was uploaded (I generally track these fairly well,
    so am normally using the latest versions whilst developing).
    These should have been installed (using the setup scripts
    they provide).
  * Python 2.0 or above
  * Tools/compiler for the current Python. For Pythons before
    2.2a4 (I think it was) this should be installed using the
    setup script it provides (after that it comes as standard).

I develop it with Python 2.1 on Windows/NT and with Python 2.2 on
Debian GNU/Linux.

Refactoring warning
-------------------
It is my aim to refactor this code to follow David Goodger's

    Reader - Transformer - Writer

model more closely than it currently does. And there is also lots of
tidying up to do (especially in `visit.py`).

Interesting things to do
------------------------
Run it over docutils/spec/reStructuredText.txt.

Run it over docutils/docutils.

Run it over pysource/visit.py.

Run it over the standard string module, and compare the result with that
of ``pydoc``.

