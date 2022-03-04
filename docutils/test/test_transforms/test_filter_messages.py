#!/usr/bin/env python3

# $Id$
# :Copyright: © 2021 Günter Milde.
# :Maintainer: docutils-develop@lists.sourceforge.net
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""
Tests for docutils.transforms.universal.FilterMessages.
"""

if __name__ == '__main__':
    import __init__
from test_transforms import DocutilsTestSupport
from docutils.transforms.universal import Messages, FilterMessages
from docutils.transforms.references import Substitutions
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    settings = {'report_level': 5}  # filter all system messages
    s = DocutilsTestSupport.TransformTestSuite(
        parser, suite_settings=settings)
    s.generateTests(totest)
    return s

totest = {}

totest['system_message_sections'] = ((Substitutions, Messages, FilterMessages), [
["""\
.. unknown-directive:: block markup is filtered without trace.
""",
"""\
<document source="test data">
"""],
["""\
Invalid *inline markup is restored to text.
""",
"""\
<document source="test data">
    <paragraph>
        Invalid \n\
        *
        inline markup is restored to text.
"""],
["""\
This |unknown substitution| will generate a system message, thanks to
the "Substitutions" transform. The "Messages" transform will
generate a "System Messages" section and the "FilterMessages" transform
will remove it.
""",
"""\
<document source="test data">
    <paragraph>
        This \n\
        |unknown substitution|
         will generate a system message, thanks to
        the "Substitutions" transform. The "Messages" transform will
        generate a "System Messages" section and the "FilterMessages" transform
        will remove it.
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
