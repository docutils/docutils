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

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.references import Substitutions
from docutils.transforms.universal import Messages, FilterMessages, TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.report_level = 5
        for name, (transforms, cases) in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    # Don't do a ``populate_from_components()`` because that
                    # would enable the Transformer's default transforms.
                    document.transformer.add_transforms(transforms)
                    document.transformer.add_transform(TestMessages)
                    document.transformer.apply_transforms()
                    output = document.pformat()
                    self.assertEqual(output, case_expected)


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
    unittest.main()
