#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""test `docutils.parsers.get_parser_class()`"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).parents[2]))
    # prepend the "test root", home of the `local_parser`
    sys.path.insert(0, str(Path(__file__).parents[2]/'test'))

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
        # requires local-parser.py in "test root" directory
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
