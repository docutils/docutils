#!/bin/sh
''''exec python3 -u "$0" "$@" #'''

# $Id$
# Author: David Goodger <goodger@python.org>,
#         Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

__doc__ = """\
All modules named 'test_*.py' in the current directory, and recursively in
subdirectories (packages) called 'test_*', are loaded and test suites within
are run.
"""

import time
# Start point for actual elapsed time, including imports
# and setup outside of unittest.
start = time.time()

import atexit               # noqa: E402
import os                   # noqa: E402
from pathlib import Path    # noqa: E402
import platform             # noqa: E402
import sys                  # noqa: E402

# Prepend the "docutils root" to the Python library path
# so we import the local `docutils` package.
# For Python < 3.9, we need `resolve()` to ensure an absolute path.
# https://docs.python.org/3/whatsnew/3.9.html#other-language-changes
DOCUTILS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(DOCUTILS_ROOT))

import docutils             # noqa: E402


class Tee:

    """Write to a file and a stream (default: stdout) simultaneously."""

    def __init__(self, filename, stream=sys.__stdout__):
        self.file = open(filename, 'w', encoding='utf-8',
                         errors='backslashreplace')
        atexit.register(self.close)
        self.stream = stream
        self.encoding = getattr(stream, 'encoding', None)

    def close(self):
        self.file.close()
        self.file = None

    def write(self, string):
        try:
            self.stream.write(string)
        except UnicodeEncodeError:
            bstring = string.encode(self.encoding, errors='backslashreplace')
            self.stream.write(bstring.decode())
        if self.file:
            self.file.write(string)

    def flush(self):
        self.stream.flush()
        if self.file:
            self.file.flush()


# must redirect stderr *before* first import of unittest
sys.stdout = sys.stderr = Tee('alltests.out')

import unittest  # NoQA: E402


class NumbersTestResult(unittest.TextTestResult):
    """Result class that counts subTests."""
    def addSubTest(self, test, subtest, error):
        super().addSubTest(test, subtest, error)
        self.testsRun += 1
        if self.dots:
            self.stream.write('.' if error is None else 'E')
            self.stream.flush()


if __name__ == '__main__':
    suite = unittest.defaultTestLoader.discover(str(DOCUTILS_ROOT / 'test'))
    print(f'Testing Docutils {docutils.__version__} '
          f'with Python {sys.version.split()[0]} '
          f'on {time.strftime("%Y-%m-%d at %H:%M:%S")}')
    print(f'OS: {platform.system()} {platform.release()} {platform.version()} '
          f'({sys.platform}, {platform.platform()})')
    print(f'Working directory: {os.getcwd()}')
    print(f'Docutils package: {os.path.dirname(docutils.__file__)}')
    sys.stdout.flush()
    result = unittest.TextTestRunner(resultclass=NumbersTestResult).run(suite)
    finish = time.time()
    print(f'Elapsed time: {finish - start:.3f} seconds')
    sys.exit(not result.wasSuccessful())
