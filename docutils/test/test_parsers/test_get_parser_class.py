#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""test `docutils.parsers.get_parser_class()`"""

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

from docutils.core import publish_string       # noqa: E402
from docutils.parsers import get_parser_class  # noqa: E402
try:
    md_parser_class = get_parser_class('recommonmark')
except ImportError:
    md_parser_class = None


class GetParserClassTestCase(unittest.TestCase):

    def test_registered_parsers(self):
        get_parser_class('null')
        get_parser_class('rst')
        get_parser_class('docutils_xml')
        # raises ImportError on failure

    def test_registered_parsers_case_folding(self):
        get_parser_class('reStructuredText')
        get_parser_class('XML')
        # raises ImportError on failure

    def test_bogus_parser(self):
        with self.assertRaises(ImportError):
            get_parser_class('nope')

    def test_local_parser(self):
        # requires local-parser.py in "test root" directory
        get_parser_class('test.local-parser')
        # raises ImportError on failure


@unittest.skipIf(md_parser_class is not None,
                 'Optional "recommonmark" module found.')
class RecommonmarkMissingTests(unittest.TestCase):

    def test_missing_parser_message(self):
        # match multiline message (?s) = re.DOTALL "." also matches newline
        with self.assertRaisesRegex(ImportError,
                                    '(?s)requires the.*package .*recommonmark'):
            publish_string('test data', parser_name='recommonmark')


if __name__ == '__main__':
    unittest.main()
