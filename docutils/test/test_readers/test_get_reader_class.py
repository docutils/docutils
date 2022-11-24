#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
test get_reader_class
"""

import unittest

from docutils.readers import get_reader_class


class GetReaderClassTestCase(unittest.TestCase):

    def test_registered_reader(self):
        get_reader_class('pep')
        # raises ImportError on failure

    def test_bogus_reader(self):
        with self.assertRaises(ImportError):
            get_reader_class('nope')

    def test_local_reader(self):
        # requires local-reader.py in test directory (testroot)
        get_reader_class('local-reader')
        # raises ImportError on failure


if __name__ == '__main__':
    unittest.main()
