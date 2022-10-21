#! /usr/bin/env python3

# $Id$
# Author: Garth Kidd <garth@deadlybloodyserious.com>
# Copyright: This module has been placed in the public domain.

"""
This module extends unittest.py with `loadTestModules()`, by loading multiple
test modules from a directory.  Optionally, test packages are also loaded,
recursively.
"""

import sys
import os
import glob
import unittest
from importlib import import_module


# So that individual test modules can share a bit of state,
# `package_unittest` acts as an intermediary for the following
# variables:
debug = False
verbosity = 1


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
