==================
 README: Docutils
==================

:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Date: $Date$
:Website: http://docutils.sourceforge.net/

Thank you for downloading the Python Docutils project arhive.  As this
is a work in progress, please check the project website for updated
working files.

To run the code, Python 2.0 or later must already be installed.  You
can get Python from http://www.python.org/.


Project Files & Directories
===========================

* README.txt: You're reading it.

* COPYING.txt: Copyright details for non-public-domain files (most are
  PD).

* HISTORY.txt: Release notes for the current and previous project
  releases.

* setup.py: Installation script.  See "Installation" below.

* install.py: Quick & dirty installation script.

* docutils: The project source directory, installed as a Python
  package.

* docs: The project user documentation directory.  The docs/rest
  directory contains reStructuredText user docs.

* spec: The project specification directory.  Contains PEPs (Python
  Enhancement Proposals), XML DTDs (document type definitions), and
  other documents.  The spec/rest directory contains the
  reStructuredText specification.

* tools: Directory for standalone scripts that use reStructuredText.

  - quicktest.py: Input reStructuredText, output pretty-printed
    pseudo-XML and various other forms.

  - publish.py: A minimal example of a complete Docutils system, using
    the "standalone" reader and "pformat" writer.

  - html.py: Read standalone reStructuredText documents and write
    HTML4/CSS1.  Uses the default.css stylesheet.

* test: Unit tests; ``test/alltests.py`` runs all the tests.  Not
  required to use the software, but very useful if you're planning to
  modify it.


Installation
============

The first step is to expand the .tar.gz archive.  It contains a
distutils setup file "setup.py".  OS-specific installation
instructions follow.

Linux, Unix, MacOS X
--------------------

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

MacOS
-----

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

Start with the html.py and publish.py front-ends from the unpacked
"tools" subdirectory.  Both tools take up to two arguments, the source
path and destination path, with STDIN and STDOUT being the defaults.

The package modules are continually growing and evolving.  The
``docutils.statemachine`` module is usable independently.  It contains
extensive inline documentation (in reStructuredText format).

The specs, the package structure, and the skeleton modules may also be
of interest to you.  Contributions are welcome!


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
