#!/usr/bin/env python3
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
Various tests for the `pygments` code highlighter.
"""

from __future__ import absolute_import
import unittest

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport # must be imported before docutils
from docutils import core, utils
from docutils.core import publish_string
from docutils.utils.code_analyzer import with_pygments

unknown_language = """\
Unknown language "S-Lang".

.. code:: s-lang

   % abc.sl
   autoload("abc_mode", "abc");
"""

class_arg = """\
Unknown language "S-Lang".

.. code::
   :class: s-lang

   % abc.sl
   autoload("abc_mode", "abc");
"""

skip_msg = 'optional module "pygments" not found'
settings = {'warning_stream': ''}

class CodeParsingTests(unittest.TestCase):

    @unittest.skipUnless(with_pygments, skip_msg)
    def test_lexer_error(self):
        output = publish_string(unknown_language, settings_overrides=settings)
        self.assertIn(b'<system_message level="2"', output)
        self.assertIn(b'Cannot analyze code. '
                      b'No Pygments lexer found for "s-lang".', output)
        self.assertIn(b'<literal_block xml:space="preserve">', output)

    @unittest.skipUnless(with_pygments, skip_msg)
    def test_lexer_error_workaround(self):
        output = publish_string(class_arg, settings_overrides=settings)
        self.assertNotIn(b'<system_message', output)
        self.assertIn(b'<literal_block classes="code s-lang"', output)
        self.assertIn(b'autoload("abc_mode", "abc");', output)


if __name__ == '__main__':
    unittest.main()
