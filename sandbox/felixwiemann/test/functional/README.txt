================
Functional tests
================

:Author: Felix Wiemann
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

(This document should probably be moved to the ``docs/`` directory of
Docutils, as it is real documentation.  Maybe we need a file
``docs/dev/testing.txt``?)

This directory (``functional/``) contains data for functional tests.

Performing functional tests means testing the Docutils system as a
whole, i.e. blackbox testing.

.. contents::

Directory Structure
===================

+ ``functional/`` The main data directory.

  + ``input/`` The input files.

    - ``some_test.txt``, for example.

  + ``output/`` The actual output.

    - ``some_test.html``, for example.

  + ``expected/`` The expected output.

    - ``some_test.html``, for example.

  + ``tests/`` The config files for processing the input files.

    - ``some_test.py``, for example.

    - ``default.py``, the `default configuration file`_.

The Testing Process
===================

When running ``test_functional.py``, all config files in
``functional/some_test.py`` are processed.

An example of what this means:

Provided ``functional/tests/some_test.py`` reads like this::

    # Source and destination file names.
    test_source = "some_test.txt"
    test_destination = "some_test.html"

    # Keyword parameters passed to publish_file.
    reader_name = "standalone"
    parser_name = "rst"
    writer_name = "html"
    settings_overrides['output-encoding': 'utf-8']

The two variables ``test_source`` and ``test_destination`` contain the
input file name (relative to ``functional/input/``) and the output
file name (relative to ``functional/output/`` and
``functional/expected/``).  Note that the file names can be chosen
arbitrarily.  However, the file names in ``functional/output/`` *must*
match the file names in ``functional/expected/``.

All other variables are passed as keyword arguments to
``docutils.core.publish_file``, so you can set reader, parser,
writer and anything else you want to configure.

Note that ``settings_overrides`` is already initialized as an empty
dictionary *before* the execution of the config file.  This is done in
order to allow subsequent assignments to ``settings_overrides`` in the
`default configuration file`_ and in the actual configuration file.

Creating New Tests
==================

In order to create a new test, put the input test file into
``functional/input/``.  Then create a config file in
``functional/tests/`` which sets at least input and output file names,
reader, parser and writer.

Now run ``test_functional.py``.  The test will fail, of course,
because you do not have an expected output yet.

However, an output file will be generated in ``functional/output/``.
Check this output file for validity and correctness.  Then copy the
file to ``functional/expected/``.

If you run ``test_functional.py`` later and the actual output doesn't
match the expected output anymore, the test will fail.

If this is the case and you made an intentional change, check the
actual output for validity and correctness and copy it to
``functional/expected/``, overwriting the old expected output.

.. _default configuration file:

The Default Configuration File
==============================

The file ``functional/tests/default.py`` contains default settings.
It is executed just before the actual configuration files, which has
the same effect as the contents of ``default.py`` were prepended to
every configuration file.
