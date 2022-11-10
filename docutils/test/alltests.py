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

import sys                  # noqa: E402
import atexit               # noqa: E402
import glob                 # noqa: E402
import os                   # noqa: E402
import platform             # noqa: E402
import warnings             # noqa: E402
from importlib import import_module  # noqa: E402
import DocutilsTestSupport  # noqa: E402,F401 must be imported before docutils
import docutils             # noqa: E402

warnings.filterwarnings('ignore',
                        message='.*return type of publish_string.*',
                        category=FutureWarning)
warnings.filterwarnings('ignore',
                        message=r".*StringOutput.encode\(\)'s return type.*",
                        category=FutureWarning)

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = os.path.abspath(os.path.dirname(__file__))


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


def loadTestModules(path):
    """
    Return a test suite composed of all the tests from modules in a directory.

    Search for modules in directory `path`, beginning with `name`.
    Then search subdirectories (also beginning with `name`)
    recursively.  Subdirectories must be Python packages; they must contain an
    '__init__.py' module.
    """
    testLoader = unittest.defaultTestLoader
    testSuite = unittest.TestSuite()
    testModules = []
    path = os.path.abspath(path)        # current working dir if `path` empty
    paths = [path]
    while paths:
        p = paths.pop() + '/test_'
        for file_path in glob.glob(p + '*.py'):
            testModules.append(path2mod(os.path.relpath(file_path, path)))
        for file_path in glob.glob(p + '*/__init__.py'):
            paths.append(os.path.dirname(file_path))
    # Import modules and add their tests to the suite.
    sys.path.insert(0, path)
    for mod in testModules:
        try:
            module = import_module(mod)
        except ImportError:
            print(f"ERROR: Can't import {mod}, skipping its tests:",
                  file=sys.stderr)
            sys.excepthook(*sys.exc_info())
        else:
            # if there's a suite defined, incorporate its contents
            try:
                suite = module.suite
            except AttributeError:
                # Look for individual tests
                moduleTests = testLoader.loadTestsFromModule(module)
                # unittest.TestSuite.addTests() doesn't work as advertised,
                # as it can't load tests from another TestSuite, so we have
                # to cheat:
                testSuite.addTest(moduleTests)
            else:
                if not callable(suite):
                    raise AssertionError(f"don't understand suite ({mod})")
                testSuite.addTest(suite())
    sys.path.pop(0)
    return testSuite


def path2mod(path):
    """Convert a file path to a dotted module name."""
    return path[:-3].replace(os.sep, '.')


class NumbersTestResult(unittest.TextTestResult):
    """Result class that counts subTests."""
    def addSubTest(self, test, subtest, error):
        super().addSubTest(test, subtest, error)
        self.testsRun += 1
        if self.dots:
            self.stream.write('.' if error is None else 'E')
            self.stream.flush()


if __name__ == '__main__':
    suite = loadTestModules(TEST_ROOT)
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
