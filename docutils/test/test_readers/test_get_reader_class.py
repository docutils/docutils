#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
test get_reader_class
"""

from pathlib import Path
import sys
import unittest

# Prepend the "docutils root" to the Python library path
# so we import the local `docutils` and `test` packages.
# ensure `test` package can be loaded also if not running as __main__
# (required by ``python -m unittest``
DOCUTILS_ROOT = Path(__file__).resolve().parents[2]
if str(DOCUTILS_ROOT) not in sys.path:
    sys.path.insert(0, str(DOCUTILS_ROOT))

from docutils.readers import get_reader_class  # noqa: E402


class GetReaderClassTestCase(unittest.TestCase):

    def test_registered_reader(self):
        get_reader_class('pep')
        # raises ImportError on failure

    def test_bogus_reader(self):
        with self.assertRaises(ImportError):
            get_reader_class('nope')

    def test_local_reader(self):
        # requires local-reader.py in `test` package
        get_reader_class('test.local-reader')
        # raises ImportError on failure


if __name__ == '__main__':
    unittest.main()
