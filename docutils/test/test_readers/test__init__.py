#! /usr/bin/env python3

# $Id$
# Author: grubert abadger1999
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""Test the "docutils.readers" module."""

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

from docutils import parsers, readers  # noqa: E402


class ReaderTests(unittest.TestCase):

    def test__init__(self):
        # Initialization also instantiates a parser, if one is specified.
        reader = readers.Reader()
        self.assertEqual(reader.parser, None)
        # The parser can be specified via its name
        reader = readers.Reader('rst')
        self.assertTrue(isinstance(reader.parser, parsers.rst.Parser),
                        f'should be Parser instance, not {reader.parser!r}')
        # or passed as instance
        parser = parsers.rst.Parser()
        reader = readers.Reader(parser)
        self.assertEqual(reader.parser, parser)
        # # the second argument `parser_name` is deprecated
        with self.assertWarnsRegex(
            DeprecationWarning,
            'Specify parser name in the "parser" argument.'):
            reader = readers.Reader(parser_name='rst')
        self.assertTrue(isinstance(reader.parser, parsers.rst.Parser))
        # if both arguments are specified, `parser` has precedence:
        with self.assertWarns(DeprecationWarning):
            reader = readers.Reader(parser, parser_name='null')
        self.assertEqual(reader.parser, parser)

        # __init__() is inherited or called by the standard readers:
        reader = readers.get_reader_class('standalone')('null')
        self.assertTrue(isinstance(reader, readers.standalone.Reader))
        self.assertTrue(isinstance(reader.parser, parsers.null.Parser))

        reader = readers.get_reader_class('pep')()
        self.assertTrue(isinstance(reader, readers.pep.Reader))
        # the "pep" reader uses the "rst" parser with special settings
        self.assertTrue(isinstance(reader.parser, parsers.rst.Parser))


class GetReaderClassTestCase(unittest.TestCase):

    def test_registered_reader(self):
        reader_class = readers.get_reader_class('pep')
        self.assertEqual(reader_class, readers.pep.Reader)

    def test_bogus_reader(self):
        with self.assertRaises(ImportError):
            readers.get_reader_class('nope')

    def test_local_reader(self):
        # requires local-reader.py in `test` package
        reader_class = readers.get_reader_class('test.local-reader')
        self.assertEqual(reader_class.supported, ('dummy', ))


if __name__ == '__main__':
    unittest.main()
