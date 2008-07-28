#! /usr/bin/env python

# $Id$
# Author: Stefan Rank <strank(AT)strank(DOT)info>
# Copyright: This module has been placed in the public domain.

"""
Tests for basic functionality of parser classes.
"""

import unittest
import DocutilsTestSupport              # must be imported before docutils
import docutils
from docutils import parsers, utils, frontend



class RstParserTests(unittest.TestCase):

    def test_inputrestrictions(self):
        parser_class = parsers.get_parser_class('rst')
        parser = parser_class()
        document = utils.new_document('test data', frontend.OptionParser(
                    components=(parser, )).get_default_values())

        self.assertRaises(UnicodeError, # UnicodeDecodeError since py2.3
                          parser.parse, 'hol%s' % chr(224), document)


if __name__ == '__main__':
    unittest.main()
