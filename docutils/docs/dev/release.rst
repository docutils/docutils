.. include:: ../header.rst

=============================
 Docutils_ Release Procedure
=============================

:Authors: David Goodger; Lea Wiemann; open to all Docutils developers
:Contact: docutils-develop@lists.sourceforge.net
:Date: $Date$
:Revision: $Revision$
:Copyright: This document has been placed in the public domain.

.. _Docutils: https://docutils.sourceforge.io/

Steps to take and watch
-----------------------

Make a clean checkout of svn.code.sf.net/p/docutils/code/trunk/docutils
to avoid having development files in the released packages.

Test, package and release this checkout.

* Announce the upcoming release on docutils-develop list.

  Consider **feature freeze** or/and **check-in freeze** .

* set_version.sh does not change HISTORY.rst and RELEASE-NOTES.rst.
  
  Why ? Maybe because the underline of the section needs to be changed anyway
  and personally I like to look around.

  Maybe change this.

* Change HISTORY.rst title ``Release <version> (unublished)``:

  Change the version insert date for unpublished.

* Update RELEASE-NOTES.rst section ``Release <version> (unpublished)``.

  Change the version insert date for unpublished.

  Consult HISTORY.rst for important changes.

* Set new version (replace ``<version>`` with the new version indentifier
  and ``<docutils-repository-root>`` with the dir containing
  ``HISTORY.rst`` and ``RELEASE-NOTES.rst``)::

      cd <docutils-repository-root>
      ../sandbox/infrastructure/set_version.sh <version>

  Check what was changed by ``set_version.sh``.

  Change acchordingly docutils/__init__.py ::

    __version_info__ = VersionInfo(
        major=0,
        minor=21,
        micro=0,
        releaselevel='candidate',  # one of 'alpha', 'beta', 'candidate', 'final'
        serial=1,  # pre-release number (0 for final releases and snapshots)
        release=False  # True for official releases and pre-releases
        )


  Run tests ::

    export PYTHONWARNINGS=default
    python3 test/alltests.py

  or use tox.
  In case of errors, clearing ``docutils/__pycache__`` may help.

  ``export PYTHONWARNINGS=default`` prints DeprecationWarnings in python3.

* Generate wheel and source-distribution, e.g.::

    python3 -m pip install build
    python3 -m build .

  check file sizes: the 0.21.2 wheel was 574K the sdist 2,2M.

* Test the wheel in local environment

  From checkout::

    cd ..
    python3 -m venv du3
    cd du3
    . bin/activate
    pip install <checkout-dir>/dist/... .whl
    # CAUTION copy test from modified not yet committed source tree.
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  Uploads to pypi/test.pypi can not be overwritten, require a new version.

* Commit changes ... the changed version number.

* build again and test locally.

* Upload wheel and source to test.pypi.

  Set repository and key in ~/.pypirc with a <server-name> and
  password token::

    python3 -m twine upload --repository <server-name> dist/*

  Change directory outside of checkout and test in venv.
  NOTE use --pre for prereleases::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --index-url https://test.pypi.org/simple/ --no-deps docutils

    # CAUTION copy test from modified not yet committed source tree.
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  Ignore missing HISTORY.rst it is not in the wheel file.
  if ok ::

    deactivate ; cd .. ; rm -r du3

  Test the sdist::

    python3 -m venv du3s ; cd du3s
    export PYTHONPATH= ; . bin/activate

    pip install --index-url https://test.pypi.org/simple/ --no-binary docutils docutils

  sdist installation fails with subprocess not finding flit_core .
  test.pypi has flit 3.5.1 but installs 0.5 preinstalling flit_core did not work
  either ... why.

  postbone sdist test to pypi::

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  Ignore ``test_find_file_in_dirs`` fails because HISTORY.rst is missing or/and
  missing pil.::

    deactivate ; cd .. ; rm -r du3s

* If final release
 
  - tag #.# (Note: only directory docutils is copied)::

      svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
               svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-#.# \
               -m "tagging release #.#"

  - Update your source directory.

  - Rebuild wheel and source-distribution, e.g::

      python3 -m build

* Upload to pypi::

    python3 -m twine upload --repository <server-name> dist/docutils-#.#*

* Remove previous package from local cache::

    find .cache/pip/wheels -name docutils\*whl -exec rm -v -i {} \;

* Test the sdist
  ::

    python3 -m venv du3s ; cd du3s
    export PYTHONPATH= ; . bin/activate

    pip install --no-binary docutils docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  Ignore ``test_find_file_in_dirs`` fails because HISTORY.rst is missing::

    deactivate ; cd .. ; rm -r du3s

* and wheel::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --no-deps docutils
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    deactivate ; cd .. ; rm -r du3

* Notify to docutils-developer and user.

For candidate releases change version to rc#+1dev.

* set_version #.rc#+1.dev 

  change docutils/__init__.py 

  test 

* docutils/HISTORY.rst: add section "Release <version> (unpublished)"
* docutils/RELEASE-Notes.rst: add section "Release <version> (unpublished)"

* test again
* commit
* run sandbox/infrastructure/docutils-update.local

For final releases

* upload source and generated html to sf-htdocs/#.# ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-#.#.tar.gz
    cd docutils-#.#/
    python3 tools/buildhtml.py .
    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/#.#

* Check web/index.rst for necessary corrections.
* Run sandbox/infrastructure/docutils-update.local to update web-content.
* Release to sourceforge.

  - Upload docutils-#.#.tar.gz and release notes to sourceforge.
  - Select docutils-#.#.tar.gz as default for all OS.

* set_version #.{#+1}b.dev
* docutils/HISTORY.rst: add title "Changes Since #.#"
* run sandbox/infrastructure/docutils-update.local

.. Emacs settings

   Local Variables:
   mode: indented-text
   mode: rst
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
