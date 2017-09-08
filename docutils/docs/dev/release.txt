=============================
 Docutils_ Release Procedure
=============================

:Authors: David Goodger; Lea Wiemann; open to all Docutils developers
:Contact: docutils-develop@lists.sourceforge.net
:Date: $Date$
:Revision: $Revision$
:Copyright: This document has been placed in the public domain.

.. _Docutils: http://docutils.sourceforge.net/

Assumptions and their failure
-----------------------------

On the test machine python 2.6, 2.7 and 3.2, 3.3, and 3.4
are installed.
Some in /usr/ some under /usr/local.

Assumption: If python2.6 is in /usr/bin/ then site-packages is under
/usr/local/lib/python2.6.

On new ubuntu 12.10

* python2.7 is here /usr/bin/python2.7.
* in /usr/lib are python2.7, pymodules and pyshared.

  in /usr/lib/python2.7 no site-packages directory even after installation
  of docutils

  in /usr/local/lib/python2.7 are dist-packages  site-packages.

  site-packages is empty.

  docutils is installed under dist-packages.

  pymodules has a python2.7 directory.

Releasing
---------

Version identifier
~~~~~~~~~~~~~~~~~~

For details, see `Version Numbering in Docutils Project Policies`__.

__ policies.html#version-numbering


How to change the version identifier
""""""""""""""""""""""""""""""""""""

The *version identifier* ``docutils.__version__`` is defined in
``docutils/docutils/__init__.py`` and used in in the following files::

    docutils/setup.py
    docutils/test/functional/expected/* ("Generator: Docutils X.Y[.Z]")
    docutils/README.txt
    web/index.txt

You can use the script ``set_release.sh`` to change the version
identifier:

#. ``cd`` to the "docutils" subdirectory in the repository checkout,

#. call ::

     ../sandbox/infrastructure/set_release.sh <new_version>

   where ``<new_version>`` is major.minor[.micro][<pre>][.dev].

   This also changes ``__version_info__`` in
   ``docutils/docutils/__init__.py``.

Run the test suite after changing the codebase version to ensure
accuracy and consistency.


Release steps
~~~~~~~~~~~~~

REWORK FOR SOURCEFORGE REPO


.. WARNING:: Steps in boldface text are *not* covered by the release script
   at sandbox/infrastructure/release.sh.  "Not covered" means that you
   aren't even reminded of them.

.. Note:: This document does not cover branching and tagging, but the
   release script does.

.. Note:: You may want to use ReleaseForge_ instead of using
   SourceForge's file release system.

   .. _ReleaseForge: http://releaseforge.sourceforge.net/


* **Announce**

  On the Docutils-develop mailing list, announce that the release is
  going to be made, update the release notes (consult HISTORY.TXT for
  changes) and ask for additions.

  Announce the upcoming release at the Sphinx-devel mailing list
  and ask for testing with Sphinx.
  (This step can be skipped for bugfix releases and pre-releases.)

  Announce the date of the feature freeze – at least a week ahead!

* **Feature freeze** From now on, only bug-fix commits are allowed.

  Update the `pre-release segment` of the `version identifier`_
  (``b``, ``rcN`` or empty) to match the level of the upcoming release
  (beta, candidate or final). Ensure ``docutils.__version_info__`` matches
  the version identifier. (See also `How to change the version
  identifier`_.)

* **Announce** the check-in freeze date on Docutils-develop – at least a
  week ahead.

* **Check-in freeze**

  **Update the version identifier**:
  Remove the `development release segment` (``.dev``) from the `version
  identifier`_ and set ``docutils.__version_info__.release`` to True.

  Check the `version identifier` in the following files (should be
  already correct if set according to `How to change the version
  identifier`_):

  + docutils/setup.py
  + docutils/docutils/__init__.py
  + docutils/test/functional/expected/* ("Generator: Docutils X.Y[.Z]")
  + docutils/README.txt
  + web/index.txt

* See what ``sandbox/infrastructure/release.sh`` can aid

  .. Note:: *BUG* test tarball requires root password, but it is possible to
     skip this stage interactively, and testing should be done before
     release.


* Close the "Changes Since ..." section in docutils/HISTORY.txt.

* Clear/unset the PYTHONPATH environment variable.

* Create the release tarball:

  (a) Create a new empty directory and ``cd`` into it.

  (b) Get a clean snapshot of the main tree::

          svn export svn://svn.code.sf.net/p/docutils/code/trunk/docutils

      or via the [Download Snapshot] button at
      http://sourceforge.net/p/docutils/code/HEAD/tree/trunk/docutils/

  (c) Use Distutils to create the release tarball::

          cd docutils
          python setup.py sdist

* Expand and _`install` the release tarball in isolation:

  (a) Expand the tarball in a new location, not over any existing
      files.

  (b) Remove the old installation from site-packages (including
      roman.py, and optparse.py, textwrap.py).

      "remove" might fail, see _`Assumptions and their failure`

      Install from expanded directory::

          cd docutils-X.Y.Z
          python setup.py install

      The "install" command may require root permissions.

  (c) Repeat step b) for all supported Python versions.

* Run the _`test suite` from the expanded archive directory with all
  supported Python versions on all available platforms (GNU/Linux, Mac
  OS X, Windows)::

      cd test ; python -u alltests.py

* Add a directory X.Y.Z (where X.Y.Z is the current version number
  of Docutils) in the webroot (i.e. the ``htdocs/`` directory).
  Put all documentation files into it::

      cd docutils-X.Y.Z
      rm -rf build
      cd tools/
      ./buildhtml.py ..
      cd ..
      find -name test -type d -prune -o -name \*.css -print0 \
          -o -name \*.html -print0 -o -name \*.txt -print0 \
          | tar -cjvf docutils-docs.tar.bz2 -T - --null
      scp docutils-docs.tar.bz2 <username>@shell.sourceforge.net:

  Now log in to shell.sourceforge.net and::

      cd /home/groups/d/do/docutils/htdocs/
      mkdir -m g+rwxs X.Y.Z
      cd X.Y.Z
      tar -xjvf ~/docutils-docs.tar.bz2
      rm ~/docutils-docs.tar.bz2

* Upload the release tarball, release.sh tries with scp.

* Access the _`file release system` on SourceForge (Admin
  interface).

  ``https://sourceforge.net/projects/docutils/files/docutils/``

  * change into the released version's directory
  * click ``(i)`` button of the tar.gz-file
  * select as default download for all operating systems.

* Submit a notification on project news.

* For verifying the integrity of the release, download the release
  tarball (you may need to wait up to 30 minutes), install_ it, and
  re-run the `test suite`_.

* Register with PyPI (``python setup.py register``).

  Set the download-url so eggs can access older releases.

* **build wheels**::

    pip wheel docutils
    pip3 wheel docutils

  This builds wheels_ (`pure Python wheels`_ for Python 2 and 3
  respectively) by downloading the new release from pypi.

  Upload the wheels to PyPI::

    twine upload docultils-<VERSION>-py2-none-any.whl
    twine upload wheelhouse/docultils-<VERSION>-py3-none-any.whl

* **Lift the freeze**

  Set the `version identifier`_ and ``__version_info__.releaselevel`` in
  docutils/docutils/__init__.py to mark the repository version as
  "in development", usually ``<major>.<minor+1>b.dev`` (cf. `How to change
  the version identifier`_).

* After a final release, add a new empty section "Changes Since ..." in
  HISTORY.txt.

* Update the web page (web/index.txt).

* Run docutils-update on the server.

* **Run alltests.py with svn version**

* **Send announcement email to:**

  * docutils-develop@lists.sourceforge.net (also announcing the end of
    the check-in freeze)
  * docutils-users@lists.sourceforge.net
  * doc-sig@python.org
  * python-announce@python.org

* **Add a** `SourceForge News item`__, **with title "Docutils X.Y.Z released"**

  __ https://sourceforge.net/p/docutils/news

  **Mark as default download for all platforms.**

.. _wheels: https://packaging.python.org/en/latest/distributing.html#wheels
.. _pure Python wheels:
   https://packaging.python.org/en/latest/distributing.html#pure-python-wheels


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
