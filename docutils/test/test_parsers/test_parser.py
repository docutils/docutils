#! /usr/bin/env python3
# $Id$
# Author: Stefan Rank <strank(AT)strank(DOT)info>
# Copyright: This module has been placed in the public domain.

"""
Tests for basic functionality of parser classes.
"""

import unittest

from docutils import parsers, utils, frontend


class RstParserTests(unittest.TestCase):

    def test_inputrestrictions(self):
        parser_class = parsers.get_parser_class('rst')
        parser = parser_class()
        document = utils.new_document('test data',
                                      frontend.get_default_settings(parser))
        # input must be unicode at all times
        self.assertRaises(TypeError, parser.parse, b'hol', document)


if __name__ == '__main__':
    unittest.main()
