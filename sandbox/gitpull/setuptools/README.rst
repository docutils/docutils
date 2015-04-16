==========
setuptools
==========

Configure docutils to use ``setuptools`` instead of ``distutils``.

The history around python packaging is convuluted [1]_ [2]_ [3]_.

The internals of docutil would be benefit from upgrading the tooling to be
on par with the python ecosystem. This is especially important since many
modern python projects rely on docutils.

* `distutils was merged into setuptools`_ in 2013.
* It is recommended as a tool in the `Python Packaging User Guide`_, which
  is ran by PyPA, who runs the official python package index (PyPI).
  
However, this cannot come at the expense of breaking compatibility with
existing functionality. This sandbox / branch also incorporates 
researching concrete details on the implications setuptools would have to 
the tooling of the project and its' compatibility.

Links
-----

- `setuptools docs`_

.. _distutils was merged into setuptools:
   https://mail.python.org/pipermail/distutils-sig/2013-March/020126.html
.. _Python Packaging User Guide: https://packaging.python.org/en/latest/
.. _setuptools docs: https://pythonhosted.org/setuptools/setuptools.html
.. [1] http://www.aosabook.org/en/packaging.html
.. [2] http://lucumr.pocoo.org/2012/6/22/hate-hate-hate-everywhere/
.. [3] http://stackoverflow.com/a/14753678
