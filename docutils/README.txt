==================
 README: Docutils
==================

:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Date: $Date$
:Web-site: http://docutils.sourceforge.net/

Thank you for downloading the Python Docutils project archive.  As
this is a work in progress, please check the project website for
updated working files.  This project should be considered highly
experimental; APIs are subject to change at any time.

The purpose of the Docutils project is to create a set of tools for
processing plaintext documentation into useful formats, such as HTML,
XML, and TeX.  Support for the following sources has been implemented:

* Standalone files.

* `PEPs (Python Enhancement Proposals)`_.

Support for the following sources is planned:

* Inline documentation from Python modules and packages, extracted
  with namespace context.  **This is the focus of the current
  development effort.**

* Email (RFC-822 headers, quoted excerpts, signatures, MIME parts).

* Wikis, with global reference lookups of "wiki links".

* Compound documents, such as multiple chapter files merged into a
  book.

* And others as discovered.

.. _PEPs (Python Enhancement Proposals):
   http://www.python.org/peps/pep-0012.html

.. contents::


Releases & Snapshots
====================

Putting together an official "Release" of Docutils is a significant
effort, so it isn't done that often.  In the meantime, the CVS
snapshots always contain the latest code and documentation, usually
updated within an hour of changes being committed to the repository,
and usually bug-free:

- Snapshot of Docutils code, tests, documentation, and
  specifications: http://docutils.sf.net/docutils-snapshot.tgz

- Snapshot of the Sandbox (experimental, contributed code):
  http://docutils.sf.net/docutils-sandbox-snapshot.tgz

- `Snapshot of web files` (the files that generate the web site):
  http://docutils.sf.net/docutils-web-snapshot.tgz

To keep up to date on the latest developments, download fresh copies
of the snapshots regularly.  New functionality is being added weekly,
sometimes daily.  (There's also the CVS repository, and a mailing list
for CVS messages.  See the web site [address above] or spec/notes.txt
for details.)


Requirements
============

To run the code, Python 2.0 or later must already be installed.
Python 2.1 or later is required to run the test suite.  You can get
Python from http://www.python.org/.


Project Files & Directories
===========================

* README.txt: You're reading it.

* COPYING.txt: Copyright details for non-public-domain files (most are
  PD).

* HISTORY.txt: Release notes for the current and previous project
  releases.

* setup.py: Installation script.  See "Installation" below.

* install.py: Quick & dirty installation script.  Just run it.

* docutils: The project source directory, installed as a Python
  package.

* docs: The project user documentation directory.  Contains the
  following documents:

  - docs/tools.txt: Docutils Front-End Tools
  - docs/rst/quickstart.txt: A ReStructuredText Primer
  - docs/rst/quickref.html: Quick reStructuredText (HTML only)

* spec: The project specification directory.  Contains PEPs (Python
  Enhancement Proposals), XML DTDs (document type definitions), and
  other documents.  The ``spec/rst`` directory contains the
  reStructuredText specification.

* tools: Directory for Docutils front-end tools.  See docs/tools.txt
  for documentation.

* test: Unit tests; ``test/alltests.py`` runs all the tests.  Not
  required to use the software, but very useful if you're planning to
  modify it.


Installation
============

The first step is to expand the ``.tar.gz`` or ``.tgz`` archive.  It
contains a distutils setup file "setup.py".  OS-specific installation
instructions follow.

GNU/Linux, Unix, MacOS X, etc.
------------------------------

1. Open a shell.

2. Go to the directory created by expanding the archive::

       cd <archive_directory_path>

3. Install the package::

       python setup.py install

   If the python executable isn't on your path, you'll have to specify
   the complete path, such as /usr/local/bin/python.  You may need
   root permissions to complete this step.

You can also just run install.py; it does the same thing.

Windows
-------

1. Open a DOS box (Command Shell, MSDOS Prompt, or whatever they're
   calling it these days).

2. Go to the directory created by expanding the archive::

       cd <archive_directory_path>

3. Install the package::

       <path_to_python.exe>\python setup.py install

If your system is set up to run Python when you double-click on .py
files, you can run install.py to do the same as the above.

MacOS 8/9
---------

1. Open the folder containing the expanded archive.

2. Double-click on the file "setup.py", which should be a "Python
   module" file.

   If the file isn't a "Python module", the line endings are probably
   also wrong, and you will need to set up your system to recognize
   ".py" file extensions as Python files.  See
   http://gotools.sourceforge.net/mac/python.html for detailed
   instructions.  Once set up, it's easiest to start over by expanding
   the archive again.

3. The distutils options window will appear.  From the "Command" popup
   list choose "install", click "Add", then click "OK".

If install.py is a "Python module" (see step 2 above if it isn't), you
can run it instead of the above.  The distutils options window will
not appear.


Usage
=====

After unpacking the Docutils package, the following shell commands
will generate HTML for all included documentation::

    cd docutils/tools
    buildhtml.py ..

For official releases, the directory may be called "docutils-X.Y",
where "X.Y" is the release version.  Alternatively::

    cd docutils
    tools/buildhtml.py --config=tools/docutils.conf

There are many front-end tools in the unpacked "tools" subdirectory.
Most tools take up to two arguments, the source path and destination
path, with STDIN and STDOUT being the defaults.  Use the "--help"
option to the front-end tools for details on options and arguments.
See `Docutils Front-End Tools`_ (``docs/tools.txt``) for full
documentation.

The package modules are continually growing and evolving.  The
``docutils.statemachine`` module is usable independently.  It contains
extensive inline documentation (in reStructuredText format of course).

Contributions are welcome!

.. _Docutils Front-End Tools: http://docutils.sf.net/docs/tools.html


Getting Help
============

If you have questions or need assistance with Docutils or
reStructuredText, please `post a message`_ to the `Docutils-Users
mailing list`_.

.. _post a message: mailto:docutils-users@lists.sourceforge.net
.. _Docutils-Users mailing list:
   http://lists.sourceforge.net/lists/listinfo/docutils-users


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
