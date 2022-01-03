#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# $Id$
# Author: Günter Milde <milde@users.sourceforge.net>,
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
"""
Test module for `docutils.parsers.rst.directives`.
"""

import unittest


if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport

import docutils
import docutils.parsers.null
from docutils.parsers.rst import directives


class DirectiveOptionConversionTestCase(DocutilsTestSupport.StandardTestCase):

    def test_flag(self):
        # Raise error when there is an argument:
        self.assertEqual(None, directives.flag(''))
        self.assertRaises(ValueError, directives.flag, 'alles')

    def test_unchanged_required(self):
        # Raise error when there is no argument:
        self.assertRaises(ValueError, directives.unchanged_required, None)
        self.assertEqual(3, directives.unchanged_required(3))

    def test_unchanged(self):
        self.assertEqual('', directives.unchanged(''))
        self.assertTrue('something' == directives.unchanged('something'))
        self.assertEqual(3, directives.unchanged(3))
        self.assertEqual([3], directives.unchanged([3]))

    # TODO: 13 more directive option conversion functions.

    def test_parser_name(self):
        self.assertEqual(None, directives.parser_name(None))
        self.assertEqual(docutils.parsers.null.Parser,
                         directives.parser_name('null'))
        self.assertEqual(docutils.parsers.rst.Parser,
                         directives.parser_name('rst'))
        self.assertRaises(ValueError, directives.parser_name, 'fantasy')

    parser_class = DocutilsTestSupport.RecommonmarkParserTestCase.parser_class
    skip_msg = 'optional module "recommonmark" not found'
    @unittest.skipUnless(parser_class, skip_msg)
    def test_external_parser_name(self):
        self.assertEqual(docutils.parsers.recommonmark_wrapper.Parser,
                         directives.parser_name('recommonmark'))


if __name__ == '__main__':
    import unittest
    unittest.main()
