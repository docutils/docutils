.. include:: ../header.rst

=====================================
 The Docutils_ Version Repository
=====================================

:Author: Lea Wiemann, Docutils developers
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

.. _Docutils: https://docutils.sourceforge.io/

.. admonition:: Quick Instructions

   To get a checkout of the Docutils source tree (with the
   sandboxes) with SVN_, type ::

       svn checkout https://svn.code.sf.net/p/docutils/code/trunk docutils-code

   Users of Git_ can clone a mirror of the docutils repository with ::

      git clone git://repo.or.cz/docutils.git

   If you are going to commit changes to the repository, please read
   the **whole document**, especially the section "`Information for
   Developers`_"!

Docutils uses a Subversion_ (SVN) repository located at
``docutils.svn.sourceforge.net``.

While Unix and Mac OS X users will probably prefer the standard
Subversion command line interface, Windows user may want to try
TortoiseSVN_, a convenient explorer extension.  The instructions apply
analogously.

There is a Git_ mirror at http://repo.or.cz/docutils.git providing
`web access`_ and the base for `creating a local Git clone`_.
[#github-mirrors]_

For the project policy on repository use (check-in requirements,
branching, etc.), please see the `Docutils Project Policies`__.

__ policies.html#subversion-repository

.. _SVN:
.. _Subversion: https://subversion.apache.org/
.. _TortoiseSVN: https://tortoisesvn.net/
.. _SourceForge.net: https://sourceforge.net/
.. _Git: http://git-scm.com/

.. contents::


Accessing the Repository
========================

Web Access
----------

The repository can be browsed and examined via the web at
https://sourceforge.net/p/docutils/code.

Alternatively, use the web interface at http://repo.or.cz/docutils.git.
[#github-mirrors]_

.. [#github-mirrors] There are also 3rd-party mirrors and forks at
   GitHub, some of them orphaned. At the time of this writing (2021-11-03),
   https://github.com/live-clones/docutils/tree/master/docutils
   provides an hourly updated clone.

Repository Access Methods
-------------------------

To get a checkout, first determine the root of the repository depending
on your preferred protocol:

anonymous access: (read only)
    Subversion_: ``https://svn.code.sf.net/p/docutils/code``

    Git_: ``git://repo.or.cz/docutils.git``

`developer access`_: (read and write)
    ``svn+ssh://<USERNAME>@svn.code.sf.net/p/docutils/code``

Checking Out the Repository
---------------------------

.. _creating a local Git clone:

Git_ users can clone a mirror of the docutils repository with ::

      git clone git://repo.or.cz/docutils.git

and proceed according to the `Git documentation`_.
Developer access (read and write) is possible with `git svn`_.

.. _Git documentation: https://git.wiki.kernel.org/index.php/GitDocumentation
.. _git svn: https://git.wiki.kernel.org/index.php/Git-svn

Subversion_ users can use the following commands
(substitute your preferred repository root for ROOT):

* To check out only the current main source tree of Docutils, type ::

    svn checkout ROOT/trunk/docutils

* To check out everything (main tree, sandboxes, web site, and parallel
  projects), type ::

    svn checkout ROOT/trunk docutils

  This will create a working copy of the whole trunk in a new directory
  called ``docutils``.

Note that you probably do *not* want to check out the ROOT itself
(without "/trunk"), because then you'd end up fetching the whole
Docutils tree for every branch and tag over and over again.

To update your working copy later on, ``cd`` into the working copy and
type ::

    svn update

Switching the Repository Root
-----------------------------

If you changed your mind and want to use a different repository root,
``cd`` into your working copy and type::

    svn switch --relocate OLDROOT NEWROOT


Editable installs
=================

There are several ways to ensure that edits to the Docutils code are
picked up by Python.

We'll assume that the Docutils "trunk" is checked out under the
``~/projects/`` directory.

1. Do an `editable install`__ with pip_::

     python3 -m pip install -e ~/projects/docutils/docutils

   __ https://pip.pypa.io/en/stable/topics/local-project-installs/
      #editable-installs

   .. _manual install:

2. Install "manually".

   Ensure that the "docutils" package is in the module search path
   (``sys.path``) by one of the following actions:

   a) Set the ``PYTHONPATH`` environment variable.

      For the bash shell, add this to your ``~/.profile``::

         PYTHONPATH=$HOME/projects/docutils/docutils
         export PYTHONPATH

      The first line points to the directory containing the ``docutils``
      package, the second line exports the environment variable.

   b) Create a symlink to the docutils package directory somewhere in the
      ``sys.path``, e.g., ::

         ln -s ~/projects/docutils/docutils \
               /usr/local/lib/python3.9/dist-packages/

   c) Use a `path configuration file`__.

      __ https://docs.python.org/library/site.html

   Optionally, add some or all `front-end tools`_ to the binary search
   path, e.g.:

   a) add the ``tools`` directory to the ``PATH`` variable::

         PATH=$PATH:$HOME/projects/docutils/docutils/tools
         export PATH

      or

   b) link idividual front-end tools to a suitable place in the binary
      path::

         ln -s ~/projects/docutils/docutils/tools/docutils-cli.py \
               /usr/local/bin/docutils

3. Do a regular install. Repeat after any change.

   .. CAUTION::

      This method is **not** recommended for day-to-day development!

      If you ever forget to reinstall the "docutils" package, Python
      won't see your latest changes. Confusion inevitably ensues.

Tip:
  A useful addition to the ``docutils`` top-level directory in
  *SVN branches* and *alternate copies* of the code is a ``set-PATHS``
  shell script containing the following lines::

      # source this file
      export PYTHONPATH=$PWD:$PYTHONPATH
      export PATH=$PWD/tools:$PATH

  Open a shell for this branch, ``cd`` to the ``docutils`` top-level
  directory, and "source" this file.  For example, using the bash
  shell::

    $ cd some-branch/docutils
    $ . set-PATHS

.. _pip: https://pypi.org/project/pip/
.. _setuptools: https://pypi.org/project/setuptools/
.. _front-end tools: ../user/tools.html


.. _developer access:

Information for Developers
==========================

If you would like to have write access to the repository, register
with SourceForge.net_ and send your SourceForge.net
user names to docutils-develop@lists.sourceforge.net.
(Note that there may be a delay of several hours until you can commit
changes to the repository.)

Sourceforge SVN access is documented `here`__

__ https://sourceforge.net/p/forge/documentation/svn/


Ensure any changes comply with the `Docutils Project Policies`_
before `checking in`_,

.. _Docutils Project Policies: policies.html
.. _checking in: policies.html#check-ins


Setting Up Your Subversion Client For Development
-------------------------------------------------

Before committing changes to the repository, please ensure that the
following lines are contained (and uncommented) in your local
~/.subversion/config file, so that new files are added with the
correct properties set::

    [miscellany]
    # For your convenience:
    global-ignores = ... *.pyc ...
    # For correct properties:
    enable-auto-props = yes

    [auto-props]
    *.py = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.rst = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.txt = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.html = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.xml = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.tex = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.css = svn:eol-style=native;svn:keywords=Author Date Id Revision
    *.patch = svn:eol-style=native
    *.sh = svn:eol-style=native;svn:executable;svn:keywords=Author Date Id Revision
    *.png = svn:mime-type=image/png
    *.jpg = svn:mime-type=image/jpeg
    *.gif = svn:mime-type=image/gif


Repository Layout
=================

The following tree shows the repository layout::

    docutils/
    |-- branches/
    |   |-- branch1/
    |   |   |-- docutils/
    |   |   |-- sandbox/
    |   |   `-- web/
    |   `-- branch2/
    |       |-- docutils/
    |       |-- sandbox/
    |       `-- web/
    |-- tags/
    |   |-- tag1/
    |   |   |-- docutils/
    |   |   |-- sandbox/
    |   |   `-- web/
    |   `-- tag2/
    |       |-- docutils/
    |       |-- sandbox/
    |       `-- web/
    `-- trunk/
        |-- docutils/
        |-- sandbox/
        `-- web/

The main source tree lives at ``docutils/trunk/docutils/``, next to
the sandboxes (``docutils/trunk/sandbox/``) and the web site files
(``docutils/trunk/web/``).

``docutils/branches/`` and ``docutils/tags/`` contain (shallow) copies
of either the whole trunk or only the main source tree
(``docutils/trunk/docutils``).
