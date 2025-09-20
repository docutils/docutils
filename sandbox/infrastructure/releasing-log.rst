===========
 Releasing
===========

:Contact: grubert@users.sourceforge.net, docutils-develop@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

Notes on what happend while releasing.


Release 0.22.2 (2025-09-20)
===========================

CI of snakemake on an MacOS arm failed, because homebrew possibly restored
a vim .swp-file ... maybe.

So release without the .swp.

* checkout current code
* run: tox -epy313
* set_version 0.22.2
* run: tox -epy311
* fix the version_info release to True
* run: tox -epy39
* Check README, HISTORY and RELEASE-NOTES titles.
* check: svn di
* svn commit 
* check for swap files: ``find . -name \*.sw\*``
* build wheel and tgz
* test tgz and wheel locally
* upload to pypi
* test src.tgz from pypi, ignore missing HISTORY::

    pip install --pre --no-binary docutils docutils

* check for swap files in testenvironmen: ``find . -name \*.sw\*``
* test wheel from pypi, ignore missing HISTORY.rst
* update code in working directory
* run tox : pass 39, 310, 311, 313, 314
* tag #.# (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.22.2 \
             -m "tagging release 0.22.2"

* check on sourceforge: https://sourceforge.net/p/docutils/code/HEAD/tree/tags/
* update code in working directory
* upload source and generated html to sf-htdocs/#.# ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.22.2.tar.gz
    cd docutils-0.22.2/
    python3 tools/buildhtml.py .
    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.22.2

* Check https://docutils.sourceforge.io/0.22.2/
* Check web/index.rst for necessary corrections.
* Run sandbox/infrastructure/docutils-update.local to update web-content.

* Release to sourceforge.

  - Upload docutils-0.22.2.tar.gz and release notes to sourceforge.
  - Upload RELEASE_NOTES.rst as README.rst.
  - Select docutils-0.22.2.tar.gz as default for all OS.

* update working directory
* set_version 0.22.3b1.dev
* check docutils/__init__ ok
* tox -epy39 310 312 313 pass
* tox-epy311 fails 

  somewhere still 0.22.2 but only via 3.11

* docutils/HISTORY.rst: add title "Release 0.22.3b1.dev (unpublished)"
* docutils/RELEASE-NOTES.rst: add title "Release 0.22.3b1.dev (unpublished)"
* Check README, HISTORY and RELEASE-NOTES titles.
* svn di
* commit
* now tox -epy311 passes
* run: sandbox/infrastructure/docutils-update.local


Release 0.22.1 (2025-09-17)
===========================

* checkout current code
* run: tox -epy313
* set_version 0.22.1
* run: tox -epy311
* fix the version_info release to True
* run: tox -epy39
* Check README, HISTORY and RELEASE-NOTES titles.
* check: svn di
* svn commit 
* build wheel and tgz
* test tgz and wheel locally
* upload to pypi
* test src.tgz from pypi, ignore missing HISTORY::

    pip install --pre --no-binary docutils docutils

* test wheel from pypi, ignore missing HISTORY.rst
* update code in working directory
* run tox : pass 39, 310, 311, 313, 314
* tag #.# (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.22.1 \
             -m "tagging release 0.22.1"

* check on sourceforge: https://sourceforge.net/p/docutils/code/HEAD/tree/tags/
* update code in working directory
* upload source and generated html to sf-htdocs/#.# ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.22.1.tar.gz
    cd docutils-0.22.1/
    python3 tools/buildhtml.py .
    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.22.1

* Check https://docutils.sourceforge.io/0.22.1/
* Check web/index.rst for necessary corrections.
* Run sandbox/infrastructure/docutils-update.local to update web-content.

* Release to sourceforge.

  - Upload docutils-0.22.1.tar.gz and release notes to sourceforge.
  - Upload RELEASE_NOTES.rst as README.rst.
  - Select docutils-0.22.1.tar.gz as default for all OS.

* update working directory
* set_version 0.22.2b1.dev
* tox -epy312
* check docutils/__init__ : failed
* fix __init__.py::

    __version_info__ = VersionInfo(
        major=0,
        minor=22, 
        micro=2,
        releaselevel='beta',  # one of 'alpha', 'beta', 'candidate', 'final'
        serial=1,  # pre-release number (0 for final releases and snapshots)
        release=False  # True for official releases and pre-releases
        )

* run: tox -epy312
* docutils/HISTORY.rst: add title "Release 0.22.2b1.dev (unpublished)"
* docutils/RELEASE-NOTES.rst: add title "Release 0.22.2b1.dev (unpublished)"
* Check README, HISTORY and RELEASE-NOTES titles.
* svn di
* commit
* run: sandbox/infrastructure/docutils-update.local


Release 0.22.1rc1 (2025-09-13)
==============================

* checkout current code
* run: tox -epy313
* set_version 0.22
* run: tox -epy311
* fix the __version_info__: release=True, micro=1, serial=1
* run: tox -epy39
* Check README, HISTORY and RELEASE-NOTES titles.
* check: svn di
* svn commit 
* build wheel and tgz
* test tgz and wheel locally
* upload to pypi
* test src.tgz from pypi, ignore missing HISTORY
* test wheel from pypi, ignore missing HISTORY.rst
* update code in working directory
* run tox : pass 39, 310, 311, 313, 314

* set_version 0.22.1b2.dev
* docutils/HISTORY.rst: add title "Release 0.22.1b2.dev (unpublished)"
* docutils/RELEASE-NOTES.rst: add title "Release 0.22.1b2.dev (unpublished)"
* Check README, HISTORY and RELEASE-NOTES changes.
* run: tox -epy313
* commit
* run: sandbox/infrastructure/docutils-update.local


Release 0.22 (2025-07-29)
=========================

* checkout current code
* run: tox -epy313
* set_version 0.22
* run: tox -epy311
* fix the version_info release to True
* run: tox -epy39
* Check README, HISTORY and RELEASE-NOTES titles.
* check: svn di
* svn commit 
* build wheel and tgz
* test tgz and wheel locally
* upload to pypi
* test src.tgz from pypi, ignore missing HISTORY
* test wheel from pypi, ignore missing HISTORY.rst
* update code in working directory
* run tox : pass 39, 310, 311, 313, 314
* tag #.# (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.22 \
             -m "tagging release 0.22"

* check on sourceforge
* update code in working directory
* upload source and generated html to sf-htdocs/#.# ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.22.tar.gz
    cd docutils-0.22/
    python3 tools/buildhtml.py .
    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.22

* Check https://docutils.sourceforge.io/0.22/
* Check web/index.rst for necessary corrections.
* Run sandbox/infrastructure/docutils-update.local to update web-content.

  Unnecessary will be 0.23b.dev soon.
* Release to sourceforge.

  - Upload docutils-0.22.tar.gz and release notes to sourceforge.
  - Upload RELEASE_NOTES.rst as README.rst.
  - Select docutils-0.22.tar.gz as default for all OS.

* update working directory
* set_version 0.23b.dev
* docutils/HISTORY.rst: add title "Release 0.23b0.dev (unpublished)"
* docutils/RELEASE-NOTES.rst: add title "Release 0.23b0.dev (unpublished)"
* Check README, HISTORY and RELEASE-NOTES titles.

  Fix: README, the version number should not be changed in line ::

   The **type hints** added in version 0.22 use Python 3.10 syntax.

* check docutils/__init__ : failed
* run: tox -epy313
* commit
* run: sandbox/infrastructure/docutils-update.local


Release 0.22rc5 (2025-06-24)
============================

A small correction:

  Don't report an error for duplicate targets with identical refname

Follow steps from rc4, varying the python versions in tox runs.

* checkout current code
* run: tox -epy313
* set_version 0.22rc5
* run: tox -epy314 : fail
* fix the version_info release to True
* run: tox -epy310
* Check README, HISTORY and RELEASE-NOTES titles.
* check: svn di
* svn commit 
* build wheel and tgz
* upload to pypi
* remove all docutils wheels from pip cache
* test src.tgz from pypi, do not forget --pre argument to get the pre-release:
  ignore missing HISTORY
* test wheel from pypi::
  
    pip install --pre  --no-cache-dir docutils

  ignore missing HISTORY.rst

* update code in working directory
* run tox : pass 39, 311, 312, 313
* set_version 0.22rc6.dev
* check docutils/__init__ : was False ... good
* run: tox -epy312
* Check README, HISTORY and RELEASE-NOTES titles.
* Check: svn di
* commit
* run: sandbox/infrastructure/docutils-update.local

Release 0.22rc4 (2025-06-17)
============================

Follow docs/release.rst.

* checkout current code
* run: tox -epy312
* set_version 0.22rc4
* run: tox -epy313 : fail
* fix the version_info release to True
* run: tox -epy314
* Check README, HISTORY and RELEASE-NOTES titles.
* run: tox -epy39
* check: svn di
* svn commit 
* run: tox -epy310
* build wheel and tgz
* test wheel locally: ignore missing HISTORY
* test src.tgz locally: ignore missing HISTORY
* upload to pypi
* remove all docutils wheels from pip cache
* test wheel from pypi, do not forget --pre argument to get the pre-release
  ignore missing HISTORY.rst
* test src.tgz from pypi: ignore missing HISTORY
* send notification emails
* update code in working directory
* run tox : pass 39, 311, 312, 313
* set_version 0.22rc5.dev
* check docutils/__init__ : was False ... good
* run: tox -epy314
* Check README, HISTORY and RELEASE-NOTES titles.
* Check: svn di
* commit
* run: sandbox/infrastructure/docutils-update.local



Release 0.22rc3 (2025-06-10)
============================

Follow docs/release.rst.

* Change release.rst: 

  - test locally.
  - Skip upload to test-pypi.

* test source from pypi.
  
  In a virtual environment::

    pip install --pre --no-binary docutils docutils
    ...


Release 0.22rc2 (2025-05-22)
============================

Follow docs/release.rst.

* Change release.rst: 

  - Commit before uploading to test-pypi.
  - test locally.

* test source from test.pypi breaks like before.

Release 0.22rc1 (2025-05-06)
============================

Follow docs/release.rst.

* Change release.rst: TODOs, MAYBEs and new section format.

* test source from test.pypi breaks like before.

Release 0.22b0.dev0 (2024-08-14)
================================

To test installing sdist from testpyi: failed flit is required to be
available on testpypi (IMHO).

* Make a clean checkout of svn.code.sf.net/p/docutils/code/trunk/docutils

* skip till

  Run tests ::

    export PYTHONWARNINGS=default
    python3 test/alltests.py

  and with tox.

  ``export PYTHONWARNINGS=default`` prints DeprecationWarnings in python3.

  No warnings or errors.

* Generate wheel and source-distribution::

    python3 -m pip install build
    python3 -m build .

  check file sizes: the 0.21.2 wheel was 574K the sdist 2,2M.
  ... similar sizes.

* Upload wheel and source to test.pypi.

  Set repository and key in ~/.pypirc with a <server-name> and
  password token::

    python3 -m twine upload --repository <server-name> dist/*

  Change directory outside of checkout and test in venv.
  NOTE use --pre for prereleases::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --index-url https://test.pypi.org/simple/ --pre --no-deps docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  Ignore missing HISTORY.txt it is not in the wheel file.
  if ok ::

    deactivate ; cd .. ; rm -r du3

  Test the sdist::

    python3 -m venv du3s ; cd du3s
    export PYTHONPATH= ; . bin/activate

    pip install --index-url https://test.pypi.org/simple/ --no-binary docutils docutils

  sdist installation fails with subprocess not finding flit_core .
  test.pypi has flit 3.5.1 but installs 0.5 preinstalling flit_core did not work
  either ... why.

  Installing build into the venv did not help and flit neither. 

Release 0.21.2 (2024-04-23)
===========================

follow docs/dev/release.txt

Release 0.21.1 (2024-04-10)
===========================

Because adding post-release numbers in a rush is not considered good
and on pypi it 0.21.post2 would be a new release anyway.

follow docs/dev/release.txt

everything fine till ... install sdist
--------------------------------------

The error ::

  pip install --index-url https://test.pypi.org/simple/ --no-deps --no-binary docutils -U docutils
  Looking in indexes: https://test.pypi.org/simple/
  Requirement already satisfied: docutils in ./lib/python3.12/site-packages (0.21)
  Collecting docutils
    Using cached https://test-files.pythonhosted.org/packages/14/1c/642f839d386b7e88da5ed5d15ad9ae100bac9e86b4cb0781ebfebdc9c42f/docutils-0.21.1.tar.gz (2.2 MB)
  Installing build dependencies ... error
  error: subprocess-exited-with-error
  
  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> [3 lines of output]
      Looking in indexes: https://test.pypi.org/simple/
      ERROR: Could not find a version that satisfies the requirement flit_core<4,>=3.4 (from versions: none)
      ERROR: No matching distribution found for flit_core<4,>=3.4
      [end of output]
  
  note: This error originates from a subprocess, and is likely not a problem with pip.
  error: subprocess-exited-with-error

  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> See above for output.

  note: This error originates from a subprocess, and is likely not a problem with pip.

there is no "flit_core" on testpypi only "flit 0.5".
Preinstalling "flit_core 3.9" in the "venv" does not help either.

Even with preinstalled flit_core 3.9 ::

  Using cached https://test-files.pythonhosted.org/docutils-0.21.1.tar.gz (2.2 MB)
  Installing build dependencies ... error
  error: subprocess-exited-with-error
  
  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> [3 lines of output]
      Looking in indexes: https://test.pypi.org/simple/
      ERROR: Could not find a version that satisfies the requirement flit_core<4,>=3.4 (from versions: none)
      ERROR: No matching distribution found for flit_core<4,>=3.4
      [end of output]
  
  note: This error originates from a subprocess, and is likely not a problem with pip.

  (du3s) engelbert@ooney:~/projects/du3s$ pip list
  Package   Version
  --------- -------
  docutils  0.21.1
  flit      3.2.0
  flit_core 3.9.0
  pip       24.0

Postbone to pypi. 

* retest wheel from test.pypi.

Continue release.txt

* Commit changes ... the changed version number.

* If final release tag #.# (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-#.# \
             -m "tagging release #.#"

* Rebuild wheel and source-distribution ::

    rm dist/*
    python3 -m flit build 

* Now upload to pypi::

    python3 -m twine upload --repository <server-name> dist/docutils-#.#*

* Remove previous package from local cache::

    find .cache/pip/wheels -name docutils\*whl -exec rm -v -i {} \;

* test sdist::

    pip install --no-binary docutils docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  Ignore missing pil.

  Note:

    ``--no-binary all`` did install docutils wheel.

* test the binary: ok

Continue to end.


Release 0.21 (2024-04-09)
=========================

(follow docs/dev/release.txt)

everything fine till ... 
------------------------

preparing docs upload

  For final releases continue

  * upload source and generated html to sf-htdocs/#.# ::
 
      mkdir tmp1
      cd tmp1
      tar xzvf ../dist/docutils-0.21.tar.gz
      cd docutils-#.#/
      python3 tools/buildhtml.py .

the sdist only contains ::

  COPYING.txt  docutils  PKG-INFO  pyproject.toml

on 20.1 it was ::

  BUGS.txt     docutils.conf      install.py   README.txt         test
  COPYING.txt  docutils.egg-info  licenses     RELEASE-NOTES.txt  THANKS.txt
  docs         FAQ.txt            MANIFEST.in  setup.cfg          tools
  docutils     HISTORY.txt        PKG-INFO     setup.py           tox.ini

HACK for the release, check flit later.
Copy following files and directories from source directory::

  BUGS.txt docutils.conf FAQ.txt HISTORY.txt licenses README.txt RELEASE-NOTES.txt
  THANKS.txt
  docs
  tools

Stop release process before uploading source tarball to sourceforge.

"flit"'s ``--use-vcs`` only works for "git" and "hg", therefore not for us.

* fix: pyproject.toml
* build new distribution: ``python -m flit build``
* check sdist
* copy the sdist to docutils-0.21.post1.tar.gz
* and upload to pypi 

  Error : only one sdist per release allowed.

* Deleting the sdist in pypi-web-interface.
* upload again ... worked.

* Upload to sourceforge.net

* commit changes: pyproject.toml, docs/dev/release.txt

* set version 0.22b.dev

pip does not like the post1
---------------------------

installing from source breaks ::

  pip install  --no-binary docutils docutils

  Discarding ... docutils-0.21.post1.tar.gz has inconsistent version: 
    expected '0.21.post1', but metadata has '0.21'

* patch VersionInfo to use serial for post# when releaselevel is "fimal".
* flit build::

    591K  docutils-0.21.post2-py3-none-any.whl
    2,2M  docutils-0.21.post2.tar.gz

* upload to testpypi
* test ::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

* test nobinary ::

    python3 -m venv du3p ; cd du3p
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --no-binary all docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  passes except the missing pil and pngs ... as usual.

But consensus is 0.21.1


Release 0.20.1 (2023-05-17)
===========================

(follow docs/dev/release.txt)

* Make a clean checkout of svn.code.sf.net/p/docutils/code/trunk/docutils
  to avoid having development files in the released packages.

* Update RELEASE-NOTES.txt add section ``Release <version>``.

  Consult HISTORY.txt for important changes.

* Change HISTORY.txt title ``Changes Since <previous release>`` to ``Release <version>``.

* Set new version (replace ``<version>`` with the new version indentifier
  and ``<docutils-repository-root>`` with the dir containing
  ``HISTORY.txt`` and ``RELEASE-NOTES.txt``)::

      cd <docutils-repository-root>
      ../sandbox/infrastructure/set_version.sh <version>

  Check what was changed by ``set_version.sh``.

  Run tests ::

    export PYTHONWARNINGS=default
    python3 test/alltests.py

  or use tox.
  In case of errors, clearing ``docutils/__pycache__`` may help.

  ``export PYTHONWARNINGS=default`` prints DeprecationWarnings in python3.

* Generate wheel and source-distribution::

    python3.11 setup.py sdist
    python3.11 setup.py bdist_wheel

* check sdist for html-files in docutils.egg-info.
* Upload wheel and source to test.pypi::

    python3.11 -m twine upload --repository docutils_testpypi dist/docutils-0.20.1*

  *docutils_testpypi* is a repository configured in .pypirc.

  Test in venv. NOTE use --pre for prereleases::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --pre --no-deps docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    As expected HISTORY fails because it is not in the package.

    python -m pip uninstall docutils
    deactivate ; cd .. ; rm -r du3

* Commit changes ... the changed version number.

* tag #.# (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-#.# \
             -m "tagging release #.#"

* Update your source directory.

  Nothing changed.

* Now upload the same files to pypi::

    python3.11 -m twine upload --repository docutils_pypi dist/docutils-0.20.1*

* Remove previous package from local cache::

    find .cache/pip/wheels -name docutils\*whl -exec rm -v -i {} \;

* and test::

    python3.11 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --no-deps docutils
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    python -m pip uninstall docutils
    deactivate ; cd .. ; rm -r du3

* Notify to docutils-developer and user.

* upload source and generated html to sf-htdocs/0.20.1 ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.20.1.tar.gz
    cd docutils-0.20.1/
    python3.11 tools/buildhtml.py .

  check for html-files in docutils.egg-info/ ... None::

    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rm -r docutils.egg-info
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.20.1

* Check web/index.txt for necessary corrections.
* Run sandbox/infrastructure/docutils-update.local to update web-content.
* Release to sourceforge.

  - Upload docutils-#.#.tar.gz and release notes to sourceforge.
  - Select docutils-#.#.tar.gz as default for all OS.

* set_version 0.20.2b.dev
* tox: py3.7 3.8 3.9 3.10 3.11 
* docutils/HISTORY.txt: add title "Changes Since 0.20.1"
* run sandbox/infrastructure/docutils-update.local


Release 0.20 (2023-05-09)
=========================

(follow docs/dev/release.txt)

release (2023-05-09)

* Update RELEASE-NOTES.txt add section ``Release 0.20``.

  Changes were already done on canditate

* Change HISTORY.txt title ``Release 0.20 (2023-05-09)``.

* Set new version (replace ``<version>`` with the new version indentifier
  and ``<docutils-repository-root>`` with the dir containing
  ``HISTORY.txt`` and ``RELEASE-NOTES.txt``)::

      cd <docutils-repository-root>
      ../sandbox/infrastructure/set_version.sh <version>
  
* run tox 3.7 ... 3.11, run python3.12 alltests.py

  all OK.

* Generate wheel and source-distribution::

    python3 setup.py sdist
    python3 setup.py bdist_wheel

* Upload wheel and source to test.pypi::

    python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

  Test in venv. ::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    python -m pip uninstall docutils
    deactivate ; cd .. ; rm -r du3

* Commit changes ... the changed version number.

* tag #.# (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.20 \
             -m "tagging release 0.20"

* Update your source directory.
* Rebuild wheel and source-distribution ::

    python3 setup.py sdist
    python3 setup.py bdist_wheel

* Now upload to pypi::

    python3 -m twine upload  dist/docutils-0.20*

* and test::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --no-deps docutils
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    deactivate ; cd .. ; rm -r du3

* Notify to docutils-developer and user.

* upload source and generated html to sf-htdocs/#.# ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.20.tar.gz
    cd docutils-0.20/
    python3 tools/buildhtml.py .
    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rm -r docutils.egg-info
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.20

* Check web/index.txt for necessary corrections. Nothing changed.
* Run sandbox/infrastructure/docutils-update.local to update web-content.
* Release to sourceforge.

  - Upload docutils-0.20.tar.gz and release notes to sourceforge.
  - Select docutils-0.20.tar.gz as default for all OS.

* set_version 0.20.1b.dev 
* run tox : OK
* docutils/HISTORY.txt: add title "Changes Since 0.20"
* commit
* run sandbox/infrastructure/docutils-update.local

release candidate 1 (2023-05-04)
--------------------------------

* svn update
* run tox : py3.7 to py3.11 : OK
* run tests with 3.12.0a7 : OK

* Update RELEASE-NOTES.txt add section ``Release <version>``.

  Consult HISTORY.txt for important changes.

* Change HISTORY.txt title ``Changes Since <previous release>`` to ``Release <version>``.

* Set new version with ``sandbox/infrastructure/set_version.sh <version>``
 
  run tox (py3.7 to 3.11)
 
  Check docutils/__init__.py __version_info__ tuple. : OK

  Run tests ::

    export PYTHONWARNINGS=default
    python3 test/alltests.py

  OK , no warnings (really no)

* Generate wheel and source-distribution::

    python3 setup.py sdist
    python3 setup.py bdist_wheel

* Upload wheel and source to test.pypi::

    python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

* Test in venv. NOTE use --pre for prereleases::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --pre --no-deps docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  HISTORY.txt is not installed with wheel. 

  Clean up::

    python -m pip uninstall docutils
    deactivate ; cd .. ; rm -r du3

* Commit changes ... the changed version number.

* Now upload to pypi::

    python3 -m twine upload  dist/docutils-0.20*

* Remove previous package from local cache::

    find .cache/pip/wheels -name docutils\*whl -exec rm -v -i {} \;

* and test::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --pre --no-deps docutils
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

  HISTORY.txt is missing.

  Clean up::

    deactivate ; cd .. ; rm -r du3

* Notify to docutils-developer and user.

* update web page.

TODO on release tag the source 

Release 0.19 (2022-07-05)
=========================

(follow docs/dev/release.txt)

* svn update
* run tox
* run tests with py3.6 to 3.11

* Update RELEASE-NOTES.txt add section ``Release <version>``.

  Consult HISTORY.txt for important changes.

* Change HISTORY.txt title ``Changes Since <previous release>`` to ``Release <version>``.

* Set new version with ``sandbox/infrastructure/set_version.sh <version>``

  Check what was changed with version control system by ``set_version.sh``

  Change docutils/__init__.py __version_info__ tuple.

  Run tests ::

    export PYTHONWARNINGS=default
    python3 test/alltests.py

  or use tox.
    
  ``export PYTHONWARNINGS=default`` prints DeprecationWarnings in python3.

* Generate wheel and source-distribution::

    python3 setup.py sdist
    python3 setup.py bdist_wheel

* Upload wheel and source to test.pypi::

    python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

  Test in venv. NOTE use --pre for prereleases::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils

    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    python -m pip uninstall docutils
    deactivate ; cd .. ; rm -r du3

* Commit changes ... the changed version number.

* tag 0.## (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.19 \
             -m "tagging release 0.19"

* Update your source directory. 
* Rebuild wheel and source-distribution ::

    python3 setup.py sdist
    python3 setup.py bdist_wheel

* Now upload to pypi::

    python3 -m twine upload  dist/docutils-0.19*

* Remove previous package from local cache::

    find .cache/pip/wheels -name docutils\*whl -exec rm -v -i {} \;

* and test::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --no-deps docutils
    cp -Lr ../docutils-code/docutils/test .
    python test/alltests.py

    deactivate ; cd .. ; rm -r du3

* Notify to docutils-developer and user.

* upload source and generated html to sf-htdocs/0.19 ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.19.tar.gz
    cd docutils-0.19/
    tools/buildhtml.py .
    find . -name \*.pyc -exec rm -v {} \;
    find . -name __pycache__ -exec rmdir -v {} \;
    rm -r docutils.egg-info
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.19

* Check web/index.txt for necessary corrections.
* Run sandbox/infrastructure/docutils-update.local to update web-content.
* Release to sourceforge.

  - Upload tar.gz and 0.19 release notes to sourceforge.
  - Select docutils-0.19.tar.gz as default for all OS.  

* set_version 0.19.1b.dev
* test with py3
* docutils/HISTORY.txt: add title "Changes Since 0.##"
* svn commit
* run sandbox/infrastructure/docutils-update.local

Problems while releasing 0.19b1
===============================

* If docutils is installed into virtual environment and
  the test directory is copied from development directory
  with ``cp -Lr ...``::

       -L, --dereference
              always follow symbolic links in SOURCE
  
  - finding ``HISTORY.txt`` in test_utils.py fails.
    Create a HISTORY.txt file to avoid.

setup.cfg contained the universal setting that generated py2py3 wheels.

Release 0.19b1 (2022-06-21)
===========================

* run tox: 
* set_version 0.19b1
* tox again
* Generate universal wheel and source-distribution ::

    python3 setup.py sdist
    python3 setup.py bdist_wheel 

* Upload universal wheel and source to test.pypi::

    twine upload --repository-url https://test.pypi.org/legacy/ dist/docutils-0.19b*

* test in venvs: ignore wrong paths and un-embedded images

* upload to pypi::

    twine upload  dist/docutils-0.19*

* and test: python uses the cached download from the previous test.

  - remove from pip cache::

      find .cache/pip/wheels -name docutils\*whl

ERROR (fixed in r9089)

  ``python3 setup.py bdist_wheel`` builds a docutils-0.19b1-py2.py3-none-any.whl
  although python > 3.7 is required ... will this break python2 installations ? 

  This happens with option universal and without and also if using *build*.

  Testing with python2.7 : only 0.18 is installed, even with --pre and -U.

  Locally cached wheels may pose a problem, though.
  Cf. https://github.com/marshmallow-code/marshmallow/issues/1860

* set_version 0.19b2.dev and change __version_info_structure.
* tox
* commit
* Run sandbox/infrastructure/docutils-update.local to update web-content.

Release 0.18.1 (2021-11-23)
===========================

follow docs/dev/release.txt

Release 0.18.1b (2021-11-18)
============================

* run tox: passed 2.7, 3.5 ... 3.11
* set_version 0.18.1b
* tox again
* commit : release 0.18.1b
* Generate universal wheel and source-distribution with py3.8::

    python3 setup.py sdist
    python3 setup.py bdist_wheel --universal

* Upload universal wheel and source to test.pypi::

    python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/docutils-0.18.1b0*

* test in venvs: ignore wrong paths and un-embedded images

* upload to pypi::

    python3.9 -m twine upload  dist/docutils-0.18.1b0*

* and test: Note python3 uses the cached download from the python2 test.

  - remove from pip cache::

      find .cache/pip/wheels -name docutils\*whl

* set_version 0.18.1b1.dev
* tox
* commit
* Run sandbox/infrastructure/docutils-update.local to update web-content.

Release 0.18 (2021-10-26)
=========================

* beta is out for three weeks.
* announce soft freeze on dev-mail.
* run tox: passed 2.7, 3.5 ... 3.11
* set_version 0.18
* tox again
* commit : release 0.18
* Generate universal wheel and source-distribution with py39.
* Upload universal wheel and source to test.pypi.
* test in venvs: ignore wrong paths and un-embedded images
* tag release 0.18

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils 
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.18 
             -m "tagging release 0.18"

* upload to pypi
* and test: Note python3 uses the cached download from the python2 test.


* upload source and generated html to sf-htdocs/0.18

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.18.tar.gz
    cd docutils-0.18/
    tools/buildhtml.py .
    find . -name \*.pyc -exec rm {} \;
    rm -rf docutils.egg-info
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.18

* Check web/index.txt for necessary corrections : None necessary.
* Release to sourceforge.

  - Upload tar.gz and 0.18 release notes to sourceforge.
  - Select docutils-0.18.tar.gz as default for all OS.  

* set_version 0.18.1.dev
* tox
* docutils/HISTORY.txt: add title "Changes Since 0.18"
* Notify docutils-developer and user.
* Run sandbox/infrastructure/docutils-update.local to update web-content.

Release 0.17 aftermath
======================

:2021-04-05: Fixed: on-ASCII characters in docutils/writers/latex2e/docutils.sty

:2021-04-04: Open: [readthedocs/recommonmark] 
             AttributeError: 'Values' object has no attribute 'tab_width' (#220)

             pinning to docutils 0.16 helped ... why ?

Release 0.17.1 (2021-04-16)
===========================

* tox with 2.7 3.5, 3.6, 3.7, 3.8, 3.9, 3.10
* set_version 0.17.1
* tox again
* commit : release 0.17.1
* Generate universal wheel and source-distribution with py38.
* Upload universal wheel and source to test.pypi.
* test in venvs ... and then next problem pypi caches 0.17.1b2.

  --no-cache-dir does not help.

  call twice, second time with ``--upgrade``.

* tag release 0.17.1
* upload to pypi
* and test
* Notify docutils-developer and user.
* upload source and generated html to sf-htdocs/0.17.1
* Check web/index.txt for necessary corrections : None necessary.
* Release to sourceforge.
* set_version 0.17.2b.dev
* tox
* docutils/HISTORY.txt: add title "Changes Since 0.17.1"
* run sandbox/infrastructure/docutils-update.local


Release 0.17.1 (2021-04-12 ...)
===============================

* tox with 2.7 3.5, 3.6, 3.7, 3.8, 3.9, 3.10

  - 3.7 and 3.10 fail. Both with ::

     from _ctypes import Union, Structure, Array
       ModuleNotFoundError: No module named '_ctypes'

  testing against development source passes.

  Test crosstest, see subdirectory

  3.7 and 3.10 require libffi-dev to build local
  then tox passes for 3.5 to 3.10.

Release 0.17.1b1 (2021-04-09)
=============================

* tox with 2.7 3.5, 3.6, 3.8, 3.9

* with LC_ALL=C and PYTHONWARNINGS=default

  python3.6 and python3.10.0a ::

    docutils/utils/smartquotes.py:639: DeprecationWarning: invalid escape sequence \[
        ch_classes = {'open': u'[(\[{]', # opening braces
    docutils/test/test_writers/test_manpage.py:62: DeprecationWarning: invalid escape sequence \-
    ... several of the same

* recommonmark tests::

    python3.6 -m pip install --user recommonmark 
    # 0.7.1
 
    python3.6 test/alltests.py
    FAILED (failures=17, errors=17, skipped=1)

    input:
    b'\nExternal hyperlink [target]s:\n\n[target]: http://www.python.org/\n'
    -: expected
    +: output
      <document source="test data">
          <paragraph>
              External hyperlink
    -         <reference name="target" refuri="http://www.python.org/">
    ?                   --------------
    +         <reference refuri="http://www.python.org/">
                  target
              s:

  only works with recommonmark 0.4

* Generate universal wheel and source-distribution with py38.
* Upload universal wheel and source to test.pypi.
* Test in python3.8 venv 

  Fails.

  Inside virtualenv ::

    >>> import docutils.parsers.recommonmark_wrapper as rw                      
    >>> dir(rw)
    ['Parser', '__builtins__', '__cached__', '__doc__', '__file__', 
     '__loader__', '__name__', '__package__', '__spec__', 
     '_recommonmarkParser', 'docutils', 'nodes', 'with_recommonmark']

  In development directory ::

    >>> import docutils.parsers.recommonmark_wrapper as rw
    >>> dir(rw)
    ['CommonMarkParser', 'Component', 'Parser', '__builtins__',
     '__cached__', '__doc__', '__file__', '__loader__',
     '__name__', '__package__', '__spec__', 'docutils', 'nodes']

  Depending on recommonmark being installed in the running python
  version or not ``CommonMarkParser`` is a class or None.

* Rebuild the wheel after running tox (this cleans caches). 
  Cannot be uploaded to testpypi because it is the same name. 

  Install the whl from dist-directory into py38 venv.
  Ok.

* New version 0.17.1b1.dev because pypi does not allow changing uploads.
* Run tox and python3.10 test.
* Commit new version number
* build sdist and universal wheel with py39.
* Install wheel from dist into venv py39.

  Test passes (embedding fails because images are missing).

* Upload universal wheel and source to test.pypi.
* Install into py39 venv : This time install the --pre release
  tests pass.

* upload to pypi.
* test in py39 venv. Passed
* test in py39 venv with LC_ALL=C. Passed
* test in venv with recommonmark==0.4: fails . requires module html.

* notify docutils-develop and user and sphinx.
* New version 0.17.1b2.dev 

Release 0.17 (2021-04-03 ...)
=============================

* tox with 2.7 3.8, 3.9

  3.10a6 misses _ctypes.

* copy more things from HISTORY to RELEASE-NOTES

* ``set_version.sh 0.17``

* Run tests manually and via tox:

  ======== ======
   pyvers   time
  ======== ======
     2.7    7.3
     3.8    6.5
     3.9    6.5
     3.10   7.2
  ======== ======

* Generate universal wheel and source-distribution.
* Upload universal wheel and source to test.pypi.
  Wait some minutes to test in python2 virtualenv.

  Test in python3.10 venv.

  Stylesheet paths are different and image embedding fails
  because images are not found. 

  After copying ``docs/user/rst/images`` from docutils into the 
  venv-directory/docs/user/rst/images image embedding works.

* Commit changes to version control system.
* tag 0.17 (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.17 \
             -m "tagging release 0.17"

* Generate universal wheel and source-distribution.
  Do it again and check for differences: svn-numbers and sha-fingerprints

* Now upload to pypi (the newly created s- and bdist).
* ... and test in venv and virtualenv.
* Notify to docutils-developer and user.
* Upload source and html to sf-htdocs/0.17

* Check web/index.txt for necessary corrections: nothing to do.

* Run sandbox/infrastructure/docutils-update.local to update web-content.
* Release to sourceforge.

  - Upload tar.gz and 0.17 release notes to sourceforge.
  - Select docutils-0.17.tar.gz as default for all OS.  

* set_version 0.18b.dev
* test with py2 and py3
* docutils/HISTORY.txt: add title "Changes Since 0.17"

* run sandbox/infrastructure/docutils-update.local

BetaRelease 0.17b1 (2021-02-10) to test.pypi
============================================

* tox with 2.7 3.8 and 3.9 ::

    functional/expected/standalone_rst_html5.html

    -<dl class="footnote brackets">
    ...
    -<dd><p>Requires support for attributes to inline
    -roles to make sense.</p>
    -</dd>
    -</dl>

  seams to be a moved chunk in test input but not in expected

* Version numbering

  ``python3 setup.py clean`` tells ::

    setuptools/dist.py:473: UserWarning: Normalizing '0.17b.dev' to '0.17b0.dev0'.

  According to https://peps.python.org/pep-0440/#pre-releases

  set_version 0.17b1

* test ::

    export PYTHONPATH=
    export PYTHONWARNINGS=default
    python2 test/alltests.py

    Ran 1454 tests 
    OK (skipped=3)

    python3 test/alltests.py
    Ran 1442 tests
    OK (skipped=5)

  python has some ResourceWarning::

    docutils/parsers/rst/directives/images.py:145: ResourceWarning: 
      unclosed file <_io.BufferedReader name=b'../docs/user/rst/images/title.png'>
      del img

    docutils/writers/html4css1/__init__.py:578: ResourceWarning: 
      unclosed file <_io.BufferedReader name=b'../docs/user/rst/images/biohazard.png'>
      del img

* Upload universal wheel and source to test.pypi::

    python3 setup.py sdist
    python3 setup.py bdist_wheel --universal
    python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

  Wait some minutes to test in virtualenv ::

    python2 -m virtualenv du2 ; cd du2
    export PYTHONPATH= ; . bin/activate

    python -m pip install --index-url https://test.pypi.org/simple/ --no-deps --pre docutils
    # Successfully installed docutils-0.17b1

    cp -r ~/projects/docutils-code/docutils/test .
    # copy docs too for inlined images to be found
    python2 test/alltests.py
    # IGNORE stylesheet path differences ?

  Test in venv ::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    python3 -m pip install --index-url https://test.pypi.org/simple/ --no-deps --pre docutils
    # Successfully installed docutils-0.17b1

    cp -r ~/projects/docutils-code/docutils/test .
    cp -r ~/projects/docutils-code/docutils/docs .
    python test/alltests.py
    # Python 3.8.5 Linux 5.4.0
    # IGNORE stylesheet path differences ?

* commit 0.17b1 to code.sf

* Now upload to pypi::

    python3 -m twine upload  dist/docutils-0.17b1*

* and test::

    python3 -m venv du3 ; cd du3
    export PYTHONPATH= ; . bin/activate

    pip install --no-deps --pre docutils
    # 0.17b1
    cp -r ~/projects/docutils-code/docutils/test .
    cp -r ~/projects/docutils-code/docutils/docs .
    python test/alltests.py
    # css paths fail

* Notify to docutils-developer and user.

* ON RELEASE then: tag 0.16 (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.16 \
             -m "tagging release 0.16"

* run sandbox/infrastructure/docutils-update.local

* set version 0.17b2.dev
* test with tox: py27 and py38 39
* commit to code.sf
* run sandbox/infrastructure/docutils-update.local

.. note:: final release has some extra steps

Release 0.16 (2020-01-12)
=========================

Set version 0.16

test ::

  export PYTHONWARNINGS=default
  python2 test/alltests.py
  python3 test/alltests.py

Upload universal wheel and source to test.pypi::

  python3 setup.py sdist
  python3 setup.py bdist_wheel --universal
  python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

Wait some minutes to test in virtualenv ::

  python2 -m virtualenv du2 ; cd du2
  export PYTHONPATH= ; . bin/activate

  python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils
  # Successfully installed docutils-0.16

  cp -r ~/projects/docutils-code/docutils/test .
  python2 test/alltests.py
  # IGNORE stylesheet path differences ?

  python -m pip uninstall docutils
  deactivate ; cd .. ; rm -rf du2

Test in venv ::

  python3 -m venv du3 ; cd du3
  export PYTHONPATH= ; . bin/activate

  python3 -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils
  # Successfully installed docutils-0.16

  cp -r ~/projects/docutils-code/docutils/test .
  python test/alltests.py
  # IGNORE stylesheet path differences ?
  # FAIL: test_find_file_in_dirs (test_utils.HelperFunctionTests)
  # FAIL: test_rst/ interpreted directives/code ...
  #  classes="keyword" is "name builtin" in 3.8.0b3 on Darwin 15.6.0

Now upload to pypi::

  python3 -m twine upload  dist/docutils-0.16*

and test::

  python3 -m venv du3 ; cd du3
  export PYTHONPATH= ; . bin/activate

  pip install --no-deps docutils
  # 0.16
  cp -r ~/projects/docutils-code/docutils/test .
  python test/alltests.py
  # css paths fail

  python2 -m virtualenv du2 ; cd du2
  export PYTHONPATH= ; . bin/activate

  pip install --no-deps docutils
  # 0.16
  cp -r ~/projects/docutils-code/docutils/test .
  python test/alltests.py
  # css paths fail

Notify to docutils-developer and user.

* tag 0.16 (Note: only directory docutils is copied)::

    svn copy svn+ssh://grubert@svn.code.sf.net/p/docutils/code/trunk/docutils \
             svn+ssh://grubert@svn.code.sf.net/p/docutils/code/tags/docutils-0.16 \
             -m "tagging release 0.16"

* upload doc/0.16 ::

    mkdir tmp1
    cd tmp1
    tar xzvf ../dist/docutils-0.16.tar.gz
    cd docutils-0.16/
    tools/buildhtml.py .
    find . -name \*.pyc -exec rm {} \;
    rm -rf docutils.egg-info
    rsync -e ssh -r -t ./ web.sourceforge.net:/home/project-web/docutils/htdocs/0.16

* change web index.txt
* run sandbox/infrastructure/docutils-update.local

* set version 0.17b.dev
* test with py2 and py3
* run sandbox/infrastructure/docutils-update.local

* docutils/HISTORY.txt: change title "Changes since 0.15" to "Release 0.16"
  add "Changes since 0.16"
* docutils/RELEASE-NOTES.txt change title "Release 0.16b ..." to Release 0.16 ..."

* Release to sourceforge.

  - Remove test/outputs from tar.gz.
  - Upload tar.gz and 0.16 release notes to sourceforge.
  - Select docutils-0.16.tar.gz as default for all OS.  

Release 0.16rc1
---------------

Set version 0.16rc1

test ::

  export PYTHONWARNINGS=default
  python2 test/alltests.py
  python3 test/alltests.py

Fix: DeprecationWarning: Please use assertEqual in test_nodes.

Upload universal wheel and source to test.pypi::

  python3 setup.py sdist
  python3 setup.py bdist_wheel --universal
  python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/*

Wait some minutes to test in virtualenv ::

  python2 -m virtualenv du2 ; cd du2
  export PYTHONPATH= ; . bin/activate

  python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils
  # Successfully installed docutils-0.15.2
  python -m pip uninstall docutils
  python -m pip install --index-url https://test.pypi.org/simple/ --no-deps --pre docutils
  # Successfully installed docutils-0.16Crc1

  cp -r ~/projects/docutils-code/docutils/test .
  python2 test/alltests.py
  # IGNORE stylesheet path differences ?

  # -<link rel="stylesheet" href="../input/data/html4css1.css" type="text/css" />
  # -<link rel="stylesheet" href="../input/data/math.css" type="text/css" />
  # +<link rel="stylesheet" href="../../html4css1.css" type="text/css" />
  # +<link rel="stylesheet" href="../../math.css" type="text/css" />

  deactivate ; cd .. ; rm -rf du2

Test in venv ::

  python3 -m venv du3 ; cd du3
  export PYTHONPATH= ; . bin/activate

  python3 -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils
  # Successfully installed docutils-0.15.2
  python -m pip uninstall docutils
  python -m pip install --index-url https://test.pypi.org/simple/ --no-deps --pre docutils
  # Successfully installed docutils-0.16b0.dev0
  cp -r ~/projects/docutils-code/docutils/test .
  python test/alltests.py

Seven CSS-path failures ... ignored for now::

  deactivate ; cd .. ; rm -rf du3

Now upload to pypi::

  python3 -m twine upload  dist/docutils-0.16rc1*

and test::

  python3 -m venv du3 ; cd du3
  export PYTHONPATH= ; . bin/activate

  pip install --no-deps --pre docutils
  # 0.16rc1
  cp -r ~/projects/docutils-code/docutils/test .
  python test/alltests.py
  # css paths fail

  python2 -m virtualenv du2 ; cd du2
  export PYTHONPATH= ; . bin/activate

  pip install --no-deps --pre docutils
  # 0.16rc1
  cp -r ~/projects/docutils-code/docutils/test .
  python test/alltests.py
  # css paths fail

Notify to docutils-developer and user.

Release 0.15 (2019-07-24)
=========================

branches/rel-0.15

CAUTION (2019-07-22)
====================

  While releasing 0.15 ::

    python3 setup.py sdist bdist_wheel
    python2 setup.py bdist_wheel

  This will result in the py2 wheel being identical to the py3 one.

  The name ``docutils-0.15-py2-none-any.whl`` cannot be used twice on pypi,
  build the py2-wheel and rename it ``docutils-0.15.post1-py2-none-any.whl``.
  (No code was changed therefore only filename change.)

  Name it ``docutils-0.15-post1-py2-none-any.whl`` then the version in the filename
  corresponds to the directory names in the wheel file.

  Maybe change the the version number in setup.py ?

Release 0.15.2 (2019-07-30)
===========================

Bump the version number to ease tool live.

Bump version ::

  set_version 0.15.2 
  python2 test/alltests.py
  python3 setup.py test3/alltests.py
  # visual inspection
  svn di | grep '^[+-]' | less -p '0.15.[12]'
  svn ci

Build py2 release upload to test.pypi ::

  mkdir py2 ; cd py2
  svn export svn+ssh://grubert@svn.code.sf.net/p/docutils/code/branches/rel-0.15/docutils
  cd docutils
  python2 setup.py sdist bdist_wheel
  python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/docutils-0.15.2-py2-none-any.whl

Test in virtualenv ::

  virtualenv du2 ; cd du2
  export PYTHONPATH= ; . bin/activate

  python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils
  cp -r ~/projects/docutils-rel-0.15/docutils/py2/docutils/test .
  python2 test/alltests.py
  # IGNORE stylesheet path differences  
  
  deactivate ; cd .. ; rm -rf du2

release to pypi from the exported source directory   ::

  python3 -m twine upload  dist/docutils-0.15.2*  

Test in new virtualenv ::

  virtualenv du2 ; cd du2
  export PYTHONPATH= ; . bin/activate

  pip install docutils
  # Successfully installed docutils-0.15.2

  cp -r ~/projects/docutils-rel-0.15/docutils/py2/docutils/test .
  python2 test/alltests.py
  # IGNORE stylesheet path differences  

  deactivate ; cd .. ; rm -rf du2

Build py3 release upload to test.pypi.
In the export/docutils ::

  # py3 and source 
  python3 setup.py bdist_wheel
  python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/docutils-0.15.2-py3-none-any.whl

Test in virtualenv ::

  python3 -m venv du3 ; cd du3
  export PYTHONPATH= ; . bin/activate

  python3 -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils

BUG install 0.15 source. Check test.pypi web interface ... py3-wheel is there.
Retry::

  # Successfully installed docutils-0.15.2
  cp -r ~/projects/docutils-rel-0.15/docutils/py2/docutils/test3 .
  python test3/alltests.py
  # IGNORE upper directory Failure
  
  deactivate ; cd .. ; rm -rf du3 

release to pypi from the exported source directory   ::

  python3 -m twine upload  dist/docutils-0.15.2-py3*  

Wait for wheel to appear on pypi.org.

Test in virtualenv ::

  python3 -m venv du3 ; cd du3
  export PYTHONPATH= ; . bin/activate

  pip install docutils
  # Successfully installed docutils-0.15.2
  
  cp -r ~/projects/docutils-rel-0.15/docutils/py2/docutils/test3 .
  python test3/alltests.py
  # IGNORE upper directory Failure

  deactivate ; cd .. ; rm -rf du3

FINE 0.15.2

Release 0.15.1 (2019-07-24)
===========================

Bug fix release for python2 only.

* set version 0.15.1
* ``python2 setup.py sdist bdist_wheel``
* ``python3 -m twine upload --repository-url https://test.pypi.org/legacy/ dist/docutils-0.15.1-py2-none-any.whl ``
* in a new virtualenv::

    python -m pip install --index-url https://test.pypi.org/simple/ --no-deps docutils

  and then test/alltests.py 

* ``python3 -m twine upload  dist/docutils-0.15.1*``

  upload py2-wheel and source.

* in a new virtualenv::

    python -m pip install docutils

  and then test/alltests.py 

* (2019-07-25) replace source by -post1

  Name it ``docutils-0.15.1-post1.tar.gz`` to make sure

  * It is unchanged library code: *post1*
  * the version is still 0.15.1: therefore separate with ``-``

  * run the tests somewhere, fix and commit.
    Then::

      svn export svn+ssh://@svn.code.sf.net/p/docutils/code/branches/rel-0.15/docutils d
      cd d
      python2 setup.py sdist
    
  * check ``tar tzf dist/docutils-0.15.1.tar.gz`` for remaining files.
    Then::

      mv dist/docutils-0.15.1.tar.gz dist/docutils-0.15.1-post1.tar.gz
      python -m twine upload dist/docutils-0.15.1-post1.tar.gz

  * Build a venv (python3), install docutils, copy test3 from development
    directory and run the test.

    As there is no 0.15.1 wheel for python3 this will be the source package. 

Release 0.15
============

svn revision: 8258 - start of release

* test on linux 2.7 3.7 : passed
* test on macosx python2.6: 

  - 7 failures due to different error messages. see below
  - 4 errors: python2.6 sys.version_info is a tuple no dictionary 
    FIXED: revision 8260

* test on macosx 2.7 3.4 3.6 3.7 : passed
* change version to : 0.15rc1.dev

  - run tests: python 2.7 and 3.7

* extract changes from HISTORY.txt to RELEASE-NOTES.txt
* rename headings in HISTORY.txt and RELEASE-NOTES.txt
* run release.sh stage 2: do a clean checkout and packing
* run local test with python 2.6 2.7 3.4 3.6 3.7
* upload to sourceforge.net. Keep 0.15 as latest, stable.
* Fix: README.txt version number BY HAND
* pypi :

  - docutils is registered
  - check setup.py : add classifier 'Programming Language :: Python :: 3.7'
  - Login to pypi (see https://packaging.python.org/tutorials/packaging-projects/)

    - python3 -m pip install --user --upgrade setuptools wheel

      (warns about not being in PATH. symlink into my/bin)
    - python3 setup.py sdist bdist_wheel ::

        /Library/Frameworks/Python.framework/Versions/3.7/lib/python3.7/distutils/dist.py:274: UserWarning: Unknown distribution option: 'python_requires'
          warnings.warn(msg)

        error: invalid command 'bdist_wheel'

    - add import setuptools to setup.py
    - python3 setup.py sdist bdist_wheel
    - python2 setup.py bdist_wheel
    - python3 -m pip install --user --upgrade twine
    - twine upload dist/*

  check on pypi: 0.15 is there.

* update website docs

  - in directory web edit index.txt
  - in sandbox/infrastructure run ./docutils-update.local

* set version of repository to 0.16b.dev

Release 0.14
============

svn revision: 8145 - start of release - 0.15.0 beta

Prerelease 0.14a0
=================

svn revision: 8082 Prerelease 0.14a0
svn revision: 8078 Prerelease 0.14.0a

* merge changes from HISTORY.txt to RELEASE-NOTES.txt
* extract release number description setting from release.sh
* rename headings in HISTORY.txt and RELEASE-NOTES.txt
* run release.sh stage 2: do a clean checkout and packing
* run local test with python2.7
* upload to sourceforge.net. Keep 0.13 as latest, stable.
* reister on to pypi and upload tgz
* pypi: unset hide old releases


Release 0.13
============

svn revision: 7980

Tests
-----

Tests are run from svn checkout, only few from install.

* windows7 python 2.7.5: OK

* windows7 python 3.4.1: as in previous release

  * test.css path error (filed #256) 
  * test_parsers\test_rst\test_directives\test_include.py::

* macosx 10.10 python 2.7.10: OK
* macosx 10.10 python 3.3.2: OK
* macosx 10.10 python 3.4.1: OK

* ubuntu 12.04 python 2.7.3: OK
* ubuntu 12.04 python 3.2.3: OK

* testing tarball 0.13.1 (build sdist) ::

    +++ functional/output/standalone_rst_html5.html
    @@ -7,9 +7,9 @@
     <meta content="A test document, containing at least one example of each reStructuredText construct." lang="en" name="description" xml:lang="en" />
    -<link rel="stylesheet" href="../input/data/minimal.css" type="text/css" />
    -<link rel="stylesheet" href="../input/data/plain.css" type="text/css" />
    -<link rel="stylesheet" href="../input/data/math.css" type="text/css" />
    +<link rel="stylesheet" href="../../minimal.css" type="text/css" />
    +<link rel="stylesheet" href="../../plain.css" type="text/css" />
    +<link rel="stylesheet" href="../../math.css" type="text/css" />
     </head>

  this is because ``test/functional/input/data/minimal.css`` is a symlink to
  ``../../../../docutils/writers/html5_polyglot/minimal.css`` and release_test.sh 
  removes directory docutils to ensure the test uses the installed docutils
  not the unzipped.

  FIX: remove docutils/__init__.py

Release 0.12
============

svn revision: 7749

Tests
-----

Tests are run from svn checkout, only few from install.

Python3 tests are run ::

  rm -rf build test3
  python3 setup.py build
  PYTHONPATH=build/lib python3 test3/alltests.py

* ubuntu 8.04: python 2.4.5, 2.5.2, 2.6.7, 2.7.2 OK

  python 2.6.4rc1 6 failure due to change in error message: no such file ...  

  python 3.2.3 OK

* macosx 10.6.8: python 2.5.4, 2.7.3

  python 2.6.1 6 failure due to change in error message: no such file ...  

  python 3.2, 3.4.1 OK

* ubuntu 14.04: pyton 2.7.6,  python 3.4.0 OK

* windows7: python 2.7.5 

  Error (filed #256): test_writers/test_html4css1_template.py::

      stylesheet = """\
    - <link rel="stylesheet" href="/test.css" type="text/css" />"""
    + <link rel="stylesheet" href="C:/test.css" type="text/css" />"""
    ?                              ++

  python 3.4.1

  * test.css path error (filed #256) 
  * test_parsers\test_rst\test_directives\test_include.py::

      b'Encoding:\n\n.. include:: test_parsers/test_rst/test_directives/utf-16.csv\n   :encoding: utf-16\n'
        File "test3\alltests.py", line 40, in write
          string = string.encode('raw_unicode_escape').decode('ascii')
      UnicodeDecodeError: 'ascii' codec can't decode byte 0xb0 in position 994: ordinal not in range(128)

   fiddling with alltests.py (uncommitted) ::

      <system_message level="4" line="1" source="test data" type="SEVERE">
        <paragraph>
            Problem with "raw" directive:
            UnicodeDecodeError: \'utf-16-be\' codec can\'t decode bytes in position 90-91: illegal encoding
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: test_parsers/test_rst/test_directives/utf-16.csv
               :encoding: utf-16''' != '''\



Release 0.11
============

Summary
-------

``sandbox/infrastructure/release.sh`` tries running ``test/alltests.py`` after 
installing the new release, this fails because

* tests depends on e.g. ``../docs/user/rst/images/title.png`` or ``../HISTORY.txt``, 
  but thess are neither in the test directory tree nor in the installed software. 

* there is a lot of code trying to handle varying installation targets ``/usr/lib``
  ``/usr/local/lib` and not yet ``/usr/lib/pymodules``.

Change testing to:

1. build and install
2. extract docutils-<release>.tar.gz into tmp
3. remove docutils/docutils directory, just to make shure it is not used.
4. run test/alltest.py in this directory, so all files are where they are
   while development.

Following failure becuase docutils-library directory was removed, therefore 
docutils/writers/html4css1/html4css1.css is not found::

  ======================================================================
  FAIL: test_custom_stylesheet_dir (test_writers.test_html4css1_misc.SettingsTestCase)
  ----------------------------------------------------------------------
  Traceback (most recent call last):
    File "/... 0.11/test/test_writers/test_html4css1_misc.py", line 81, in test_custom_stylesheet_dir
      self.assertIn('docutils/writers/html4css1/html4css1.css', styles)
    File "/... 0.11/test/DocutilsTestSupport.py", line 138, in assertIn
      msg or '%s not in %s' % _format_str(a, b))
  AssertionError: 'docutils/writers/html4css1/html4css1.css' not in u'''\
  <link rel="stylesheet" href="html4css1.css" type="text/css" />
  <link rel="stylesheet" href="data/ham.css" type="text/css" />
  '''


Tests
-----

* ubuntu 8.04 2.4.5, 2.5.2, 2.6.7, 2.7.2 OK

  python 2.6.4rc1 6 failure due to change in error message: no such file ...  

  Python 3.2.3 : OK (roman.py left over from last release)

* macosx 10.6.8: python 2.5.4, 2.7.3

  python 2.6.1 6 failure due to change in error message: no such file ...  

  python 3.2: roman.py for py3 required.

Release.sh
----------

release.sh does not work on MacOSX.

installation on ubuntu 10.04 breaks test script::

  Working directory: /usr/local/lib/python2.6/site-packages/docutils-test
  Docutils package: /usr/local/lib/python2.6/dist-packages/docutils

  ======================================================================
  FAIL: test_find_file_in_dirs (test_utils.HelperFunctionsTests)
  ----------------------------------------------------------------------
  Traceback (most recent call last):
    File "/usr/local/lib/python2.6/site-packages/docutils-test/test_utils.py", line 295, in test_find_file_in_dirs
      '../HISTORY.txt')
  AssertionError: 'HISTORY.txt' != '../HISTORY.txt'

Stopping for now.



Release 0.10
============

* same failures for some python2.6 versions ::

              Problems with "raw" directive path:
       -      InputError: [Errno 2] No such file or directory: 'non-existent.file'.
       +      InputError: (2, 'No such file or directory').

* testing release tarball ::

    ======================================================================
    FAIL: test_dependencies (__main__.RecordDependenciesTests)
    ----------------------------------------------------------------------
    Traceback (most recent call last):
      File "docutils-test/test_dependencies.py", line 61, in test_dependencies
        self.assertEqual(record, expected)
    AssertionError: [u'data/include.txt', u'data/raw.txt'] != [u'../docs/user/rst/images/title.png',
    u'data/include.txt', u'data/raw.txt']
    
    ...

  because ../docs is not there if run from /usr/../pythonx.x/site-packages/docutils-test.

Release 0.9.1
=============

same failures as for 0.9 plus

* python 2.3: twice, ignored ::

  -             [Errno 2] No such file or directory: 'bogus.csv'.
  +             [Errno 2] No such file or directory: u'bogus.csv'.

  python 2.3 ::

    ======================================================================
    ERROR: test_unicode (test_error_reporting.ErrorStringTests)
    ----------------------------------------------------------------------
    Traceback (most recent call last):
      File "/usr/local/lib/python2.3/site-packages/docutils-test/test_error_reporting.py", line 153, in test_unicode
        self.assertEqual(u'ImportError: %s' % SafeString(self.bs),
    UnicodeDecodeError: 'ascii' codec can't decode byte 0xfc in position 0: ordinal not in range(128)

Note: sf takes some considerable time till the downlods are visible for normal users.
  More than 1 hour , the folder files/docutils/0.9.1 exists and the webinterface
  correctly summarizes "Totals: 2 Items     1.6 MB" but nothing is shown.

Release 0.9
===========

* python 2.3 unittest.TestCase has no assertTrue
* python 2.3 keyword dictionaries update method does not support kwargs

Ignored test errors

* python 2.3: unicode problems. 2.3 support is likely to be ended soon.
  Systems with only 2.3 might not even know of unicode.

* PIL ``AttributeError: 'module' object has no attribute 'Image'``

  A problem in PIL ? 
  Tested and failiing on

  - ubuntu8.04, python 2.4, 2.5  
  - ubuntu10.04, python 2.6  
  - ubuntu11.10, python 2.7  

* python 2.6.4rc1 has a different error message format::

              Problems with "raw" directive path:
       -      InputError: [Errno 2] No such file or directory: 'non-existent.file'.
       +      InputError: (2, 'No such file or directory').

  but not in python 2.6.7
