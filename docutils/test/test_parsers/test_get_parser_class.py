#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
test get_parser_class
"""

import unittest

from docutils.core import publish_string
from docutils.parsers import get_parser_class
try:
    md_parser_class = get_parser_class('recommonmark')
except ImportError:
    md_parser_class = None


class GetParserClassTestCase(unittest.TestCase):

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


@unittest.skipIf(md_parser_class is not None,
                 'Optional "recommonmark" module found.')
class RecommonmarkMissingTests(unittest.TestCase):

    def test_missing_parser_message(self):
        with self.assertRaisesRegex(ImportError,
                                    'requires the package .*recommonmark'):
            publish_string('test data', parser_name='recommonmark')


if __name__ == '__main__':
    import unittest
    unittest.main()
