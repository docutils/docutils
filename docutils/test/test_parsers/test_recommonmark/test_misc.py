#!/usr/bin/env python
# -*- coding: utf8 -*-
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""
Various tests for the recommonmark parser.
"""

from __future__ import absolute_import
import sys
import unittest

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport # must be imported before docutils
from docutils import core, utils
from docutils.core import publish_string

sample_with_html = """\
A paragraph:

<p>A HTML block.</p>

Next paragraph.

<script type="text/javascript">
// some dangerous JavaScript

Final paragraph.
"""

parser_class = DocutilsTestSupport.RecommonmarkParserTestCase.parser_class
skip_msg = 'optional module "recommonmark" not found'

class reCommonMarkParserTests(unittest.TestCase):

    @unittest.skipUnless(parser_class, skip_msg)
    def test_raw_disabled(self):
        output = publish_string(sample_with_html, parser_name='recommonmark',
                                settings_overrides={'warning_stream': '',
                                                    'raw_enabled': False})
        self.assertNotIn(b'<raw>', output)
        self.assertIn(b'<system_message', output)
        self.assertIn(b'Raw content disabled.', output)

    @unittest.skipUnless(parser_class, skip_msg)
    def test_raw_disabled_inline(self):
        output = publish_string('foo <a href="uri">', parser_name='recommonmark',
                                settings_overrides={'warning_stream': '',
                                                    'raw_enabled': False,
                                                   })
        self.assertNotIn(b'<raw>', output)
        self.assertIn(b'<system_message', output)
        self.assertIn(b'Raw content disabled.', output)


    @unittest.skipUnless(parser_class, skip_msg)
    def test_raw_disabled(self):
        output = publish_string(sample_with_html, parser_name='recommonmark',
                                settings_overrides={'warning_stream': '',
                                                    'raw_enabled': False,
                                                    'report_level': 3,
                                                   })
        self.assertNotIn(b'<raw>', output)
        self.assertNotIn(b'<system_message', output)

    @unittest.skipIf(parser_class,
                     'recommonmark_wrapper: parser found, fallback not used')
    def test_missing_parser(self):
        try:
            output = publish_string(sample_with_html, 
                                    parser_name='recommonmark')
        except ImportError as err:
            self.assertIn(
                'requires the package https://pypi.org/project/recommonmark', 
                str(err))

if __name__ == '__main__':
    unittest.main()
