#!/bin/sh
''''exec python3 -u "$0" "$@" #'''

# $Id$
# Author: David Goodger <goodger@python.org>,
#         Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

from __future__ import annotations

__doc__ = """\
All modules named 'test_*.py' in the current directory, and recursively in
subdirectories (packages) called 'test_*', are loaded and test suites within
are run.
"""  # NoQA: A001 (shadowing builtin `__doc__`)

import atexit
import platform
import time
import sys
from pathlib import Path
from typing import TYPE_CHECKING

# Prepend the "docutils root" to the Python library path
# so we import the local `docutils` package.
DOCUTILS_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(DOCUTILS_ROOT))

import docutils  # NoQA: E402

if TYPE_CHECKING:
    import types
    from typing import TextIO
    from unittest.case import TestCase

    from typing_extensions import TypeAlias

    ErrorTriple: TypeAlias = tuple[
        type[BaseException],
        BaseException,
        types.TracebackType,
        ]

STDOUT = sys.__stdout__


class Tee:
    """Write to a file and stdout simultaneously."""

    def __init__(self, filename: str) -> None:
        self.file: TextIO | None = open(filename, mode='w',
                                        encoding='utf-8',
                                        errors='backslashreplace')
        self.encoding: str = 'utf-8'
        atexit.register(self.close)

    def close(self) -> None:
        if self.file is not None:
            self.file.close()
            self.file = None

    def write(self, string: str) -> None:
        try:
            STDOUT.write(string)
        except UnicodeEncodeError:
            bstring = string.encode(STDOUT.encoding, errors='backslashreplace')
            STDOUT.buffer.write(bstring)
        if self.file is not None:
            self.file.write(string)

    def flush(self) -> None:
        STDOUT.flush()
        if self.file is not None:
            self.file.flush()


# must redirect stderr *before* first import of unittest
sys.stdout = sys.stderr = Tee('alltests.out')

import unittest  # NoQA: E402


class NumbersTestResult(unittest.TextTestResult):
    """Result class that counts subTests."""
    def addSubTest(self,
                   test: TestCase,
                   subtest: TestCase,
                   error: ErrorTriple | None,
                   ) -> None:
        super().addSubTest(test, subtest, error)
        self.testsRun += 1
        if self.dots:
            self.stream.write('.' if error is None else 'E')
            self.stream.flush()


if __name__ == '__main__':
    # Start point for elapsed time, except suite setup.
    start = time.time()
    suite = unittest.defaultTestLoader.discover(str(DOCUTILS_ROOT / 'test'))
    print(f'Testing Docutils {docutils.__version__} '
          f'with Python {sys.version.split()[0]} '
          f'on {time.strftime("%Y-%m-%d at %H:%M:%S")}')
    print(f'OS: {platform.system()} {platform.release()} {platform.version()} '
          f'({sys.platform}, {platform.platform()})')
    print(f'Working directory: {Path.cwd()}')
    print(f'Docutils package: {Path(docutils.__file__).parent}')
    sys.stdout.flush()
    result = unittest.TextTestRunner(resultclass=NumbersTestResult).run(suite)
    finish = time.time()
    print(f'Elapsed time: {finish - start:.3f} seconds')
    sys.exit(not result.wasSuccessful())
