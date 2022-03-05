#!/usr/bin/env python3
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

import unittest

from docutils.core import publish_string
from docutils.utils.code_analyzer import with_pygments

unknown_language = """\
Unknown language "S-Lang".

.. code:: s-lang

   % abc.sl
   autoload("abc_mode", "abc");
"""

workaround = """\
Workaround to silence warning: disable code parsing with
"syntax highlight" setting or put code language in class argument:

.. code::
   :class: s-lang

   % abc.sl
   autoload("abc_mode", "abc");
"""

settings = {'warning_stream': ''}


@unittest.skipUnless(with_pygments, 'optional module "pygments" not found')
class CodeParsingTests(unittest.TestCase):

    def test_lexer_error(self):
        output = publish_string(unknown_language, settings_overrides=settings)
        self.assertIn(b'<system_message level="2"', output)
        self.assertIn(b'Cannot analyze code. '
                      b'No Pygments lexer found for "s-lang".', output)
        self.assertIn(b'<literal_block xml:space="preserve">', output)

    def test_lexer_error_workaround(self):
        output = publish_string(workaround, settings_overrides=settings)
        self.assertNotIn(b'<system_message', output)
        self.assertIn(b'<literal_block classes="code s-lang"', output)
        self.assertIn(b'autoload("abc_mode", "abc");', output)


if __name__ == '__main__':
    unittest.main()
