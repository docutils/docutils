===================
docutils cross test
===================

:Date: $Date$

Testing against/with/through several python installations.

tox does this ... somehow ... trying to do it explicit.

* test with PYTHONWARNINGS=default

  this shows deprecation warnings.

* and LC_ALL=C

Log
---

* setup a virtual environment with every python
  include wheel

* build a wheel with every python (in the venv ?)

  before build clean docutils::

    find . -type f -name "*.pyc" -delete

  3.7 and 3.10 fail : _ctypes missing

  3.6 and 3.8 have the same md5sum, others differ.

* missing _ctypes for 3.7 and 3.10

  - install libffi-dev

  python 3.7 works

  but 3.10a6 fails building venv ::

    python3.10 -m venv a
    Error: subprocess not supported for isolated subinterpreters

    find a
    a/lib
    a/lib/python3.10
    a/lib/python3.10/site-packages
    a/lib64
    a/include
    a/bin
    a/bin/python3
    a/bin/python
    a/bin/python3.10
    a/pyvenv.cfg

  For now skip 3.10

* test wheel from 3.7 in every venv
  with

    export LC_ALL=C
    export PYTHONPATH=
    export PYTHONWARNINGS=default

MAYBE

* test every wheel in every venv



