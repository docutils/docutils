#! /usr/bin/env python3
# $Id$
# Author: Stefan Rank <strank(AT)strank(DOT)info>
# Copyright: This module has been placed in the public domain.

"""
Tests for basic functionality of parser classes.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the local "docutils root" to the Python library path
    sys.path.insert(0, Path(__file__).resolve().parents[2].as_posix())

from docutils import frontend, utils
import docutils.parsers.rst


class RstParserTests(unittest.TestCase):

    def test_inputrestrictions(self):
        # input must be unicode at all times, check for meaningful Exception
        parser = docutils.parsers.rst.Parser()
        document = utils.new_document('test data',
                                      frontend.get_default_settings(parser))
        with self.assertRaises(TypeError):
            parser.parse(b'hol', document)


if __name__ == '__main__':
    unittest.main()
