==============================
 README: Docutils 0.19.1b.dev
==============================

:Author: David Goodger
:Contact: goodger@python.org
:Date: $Date$
:Web site: https://docutils.sourceforge.io/
:Copyright: This document has been placed in the public domain.

.. contents::


Quick-Start
===========

This is for those who want to get up & running quickly.

1. Docutils requires **Python**, available from
   https://www.python.org/.
   See Dependencies_ below for details.

2. Install the latest stable release from PyPi with pip_::

       python3 -m pip install docutils

   For alternatives and details, see section `Installation`_ below.

3. Use the `front-end scripts`_ to convert reStructuredText documents.
   Try for example::

       rst2html.py FAQ.txt FAQ.html         (Unix)
       docutils FAQ.txt FAQ.html  (Unix and Windows)

   See Usage_ below for details.


Purpose
=======

The purpose of the Docutils project is to create a set of tools for
processing plaintext documentation into useful formats, such as HTML,
LaTeX, troff (man pages), OpenOffice, and native XML.  Support for the
following sources has been implemented:

* Standalone files.

* `PEPs (Python Enhancement Proposals)`_.

Support for the following sources is planned or provided by
`third party tools`_:

* Inline documentation from Python modules and packages, extracted
  with namespace context.

* Email (RFC-822 headers, quoted excerpts, signatures, MIME parts).

* Wikis, with global reference lookups of "wiki links".

* Compound documents, such as multiple chapter files merged into a
  book.

* And others as discovered.

.. _PEPs (Python Enhancement Proposals):
   https://peps.python.org/pep-0012
.. _third party tools: docs/user/links.html#related-applications


Dependencies
============

To run the code, Python_ must be installed.
(Python is pre-installed with most Linux distributions.)

* Docutils 0.19.1b.dev requires Python 3.7 or later.
* Docutils 0.16 to 0.18 require Python 2.7 or 3.5+.
* Docutils 0.14 dropped Python 2.4, 2.5, 3.1 and 3.2 support.

.. _Python: https://www.python.org/.


Optional Dependencies
---------------------

Docutils uses the following packages for enhanced functionality, if they
are installed:

* Installation_ is usually done with pip_ or setuptools_.

* The `Python Imaging Library`_, or PIL, is used for some image
  manipulation operations.

* The `Pygments`_ package provides syntax highlight of "code" directives
  and roles.

* The `myst`_ or `recommonmark`_ parsers can be used to parse input in
  Markdown format.

The `Docutils Link List <docs/user/links.html>`__ records projects that
users of Docutils and reStructuredText may find useful.

.. _pip: https://pypi.org/project/pip/
.. _setuptools: https://pypi.org/project/setuptools/
.. _Python Imaging Library: http://www.pythonware.com/products/pil/
.. _Pygments: https://pypi.org/project/Pygments/
.. _myst: https://pypi.org/project/myst-docutils/
.. _recommonmark: https://github.com/rtfd/recommonmark


Development version
===================

While we are trying to follow a "release early & often" policy,
features are added frequently.
Since the code in the `Docutils version repository`_ is usually in a
bug-free state, we recommend using a current snapshot or a working copy.

Snapshots:
  To get a repository _`snapshot`, go to
  https://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/
  and click the download snapshot button.

Repository check-out:
  To keep up to date on the latest developments,
  use a `working copy`__ of the `Docutils version repository`_.

Continue with the `Installation`_ instructions below.

.. _Docutils version repository: docs/dev/repository.html
.. _sandbox: https://docutils.sourceforge.io/sandbox/README.html

__ docs/dev/repository.html#checking-out-the-repository


Installation
============

* The simplest way is to install the latest stable release from PyPi with
  pip_::

    python3 -m pip install docutils

  To install a pre-relase, append the option ``--pre``.

* To install a `development version`_ from source with `setuptools`_:

  * Go to the directory containing the file ``setup.py``.

    A snapshot_ must be unpacked in a temporary directory
    (**not** directly in Python's ``site-packages``) first.

  * Run ``setup.py install``.
    See also OS-specific installation instructions below.

  Optional steps:
  
  * `Running the test suite`_
  
  * `Converting the documentation`_

* For installing "by hand" or in "development mode", see the
  `editable installs`_ section in the `Docutils version repository`_
  documentation.

  .. _editable installs: docs/dev/repository.html#editable-installs


GNU/Linux, BSDs, Unix, Mac OS X, etc.
-------------------------------------

1. Open a shell.

2. Go to the directory containing ``setup.py``::

       cd <archive_directory_path>

3. Install the package (you may need root permissions to complete this
   step)::

       su
       (enter admin password)
       python3 setup.py install

   If the python executable isn't on your path, you'll have to specify
   the complete path, such as ``/usr/local/bin/python``.

   To install for a specific Python version, use this version in the
   setup call, e.g. ::

       python3.7 setup.py install

   To install for different Python versions, repeat step 3 for every
   required version. The last installed version will be used in the
   `shebang line`_ of the `front-end scripts`_.

   .. _shebang line: https://en.wikipedia.org/wiki/Shebang_%28Unix%29

Windows
-------

Just double-click ``install.py``.  If this doesn't work, try the
following:

1. Open a DOS Box (Command Shell, MS-DOS Prompt, or whatever they're
   calling it these days).

2. Go to the directory created by expanding the archive::

       cd <archive_directory_path>

3. Install the package::

       <path_to_python.exe>\python setup.py install

   To install for a specific python version, specify the Python
   executable for this version.

   To install for different Python versions, repeat step 3 for every
   required version.


Usage
=====

There are many front-end tools in the unpacked "tools" subdirectory.
Installation under Unix places copies in the PATH.
You may want to begin with the "rst2html.py" front-end tool.  Most
tools take up to two arguments, the source path and destination path,
with STDIN and STDOUT being the defaults.  Use the ``--help`` option to
the front-end tools for details on options and arguments.  See
`Docutils Front-End Tools`_ for full documentation.

The package modules are continually growing and evolving.  The
``docutils.statemachine`` module is usable independently.  It contains
extensive inline documentation (in reStructuredText format of course).

Contributions are welcome!

.. _front-end scripts:
.. _Docutils Front-End Tools: docs/user/tools.html

Project Files & Directories
===========================

* README.txt: You're reading it.

* COPYING.txt: Public Domain Dedication and copyright details for
  non-public-domain files (most are PD).

* FAQ.txt: Frequently Asked Questions (with answers!).

* RELEASE-NOTES.txt: Summary of the major changes in recent releases.

* HISTORY.txt: A detailed change log, for the current and all previous
  project releases.

* BUGS.txt: Known bugs, and how to report a bug.

* THANKS.txt: List of contributors.

* setup.py: Installation script.  See "Installation" below.

* install.py: Quick & dirty installation script.  Just run it.  For
  any kind of customization or help though, setup.py must be used.

* docutils: The project source directory, installed as a Python
  package.

* docs: The project documentation directory.  Read ``docs/index.txt``
  for an overview.

* docs/user: The project user documentation directory.  Contains the
  following documents, among others:

  - docs/user/tools.txt: Docutils Front-End Tools
  - docs/user/latex.txt: Docutils LaTeX Writer
  - docs/user/rst/quickstart.txt: A ReStructuredText Primer
  - docs/user/rst/quickref.html: Quick reStructuredText (HTML only)

* docs/ref: The project reference directory.
  ``docs/ref/rst/restructuredtext.txt`` is the reStructuredText
  reference.

* licenses: Directory containing copies of license files for
  non-public-domain files.

* tools: Directory for Docutils front-end tools.  See
  ``docs/user/tools.txt`` for documentation.

* test: Unit tests.  Not required to use the software, but very useful
  if you're planning to modify it.  See `Running the Test Suite`_
  below.


Converting the documentation
============================

After unpacking and installing the Docutils package, the following
shell commands will generate HTML for all included documentation::

    cd <archive_directory_path>/tools
    ./buildhtml.py ../

On Windows systems, type::

    cd <archive_directory_path>\tools
    python buildhtml.py ..

The final directory name of the ``<archive_directory_path>`` is
"docutils" for snapshots.  For official releases, the directory may be
called "docutils-X.Y.Z", where "X.Y.Z" is the release version.
Alternatively::

    cd <archive_directory_path>
    tools/buildhtml.py --config=tools/docutils.conf          (Unix)
    python tools\buildhtml.py --config=tools\docutils.conf   (Windows)

Some files may generate system messages (warnings and errors).  The
``docs/user/rst/demo.txt`` file (under the archive directory) contains
five intentional errors.  (They test the error reporting mechanism!)


Running the Test Suite
======================

The test suite is documented in `Docutils Testing`_ (docs/dev/testing.txt).

To run the entire test suite, open a shell and use the following
commands::

    cd <archive_directory_path>/test
    ./alltests.py

Under Windows, type::

    cd <archive_directory_path>\test
    python alltests.py


You should see a long line of periods, one for each test, and then a
summary like this::

    Ran 1111 tests in 24.653s

    OK
    Elapsed time: 26.189 seconds

The number of tests will grow over time, and the times reported will
depend on the computer running the tests.  The difference between the
two times represents the time required to set up the tests (import
modules, create data structures, etc.).

A copy of the test output is written to the file ``alltests.out``.

If any of the tests fail, please `open a bug report`_ or `send an email`_
(see `Bugs <BUGS.html>`_).
Please include all relevant output, information about your operating
system, Python version, and Docutils version.  To see the Docutils
version, look at the test output or use one of the `front-end scripts`_ 
with the ``--version`` option, e.g.::

    docutils --version

.. _Docutils Testing: https://docutils.sourceforge.io/docs/dev/testing.html
.. _open a bug report:
   https://sourceforge.net/p/docutils/bugs/
.. _send an email: mailto:docutils-users@lists.sourceforge.net
   ?subject=Test%20suite%20failure
.. _web interface: https://sourceforge.net/p/docutils/mailman/


Getting Help
============

All documentation can be reached from the `Project Documentation
Overview`_. 

The SourceForge `project page`_ has links to the tracker, mailing
lists, and code repository.

If you have further questions or need assistance with Docutils or
reStructuredText, please post a message to the Docutils-users_ mailing
list.

.. _Project Documentation Overview: docs/index.html
.. _project page: https://sourceforge.net/p/docutils
.. _Docutils-users: docs/user/mailing-lists.html#docutils-users


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
