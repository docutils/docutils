#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
test get_parser_class
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test_parsers import DocutilsTestSupport
from docutils.parsers import get_parser_class


class GetParserClassTestCase(DocutilsTestSupport.StandardTestCase):

    def test_registered_parser(self):
        get_parser_class('rst')
        # raises ImportError on failure

    def test_bogus_parser(self):
        with self.assertRaises(ImportError):
            get_parser_class('nope')

    def test_local_parser(self):
        # requires local-parser.py in test directory (testroot)
        get_parser_class('local-parser')
        # raises ImportError on failure


if __name__ == '__main__':
    import unittest
    unittest.main()
