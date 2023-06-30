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
Test for block quotes in CommonMark parsers
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers.recommonmark_wrapper import Parser
from docutils.utils import new_document


class RecommonmarkParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['block_quotes'] = [
["""\
> block quote
> line 2
""",
"""\
<document source="test data">
    <block_quote>
        <paragraph>
            block quote
            line 2
"""],
["""\
Line 1.

  > Indented block quote.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
    <block_quote>
        <paragraph>
            Indented block quote.
"""],
["""\
Line 1.
Line 2.
> Block quote, without blank line before.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Block quote, without blank line before.
"""],
["""\
Line 1.
Line 2.

>Block quote,
continuation line
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Block quote,
            continuation line
"""],
["""\
Here is a paragraph.

>   >  Nested
>
>   block quotes.
""",
"""\
<document source="test data">
    <paragraph>
        Here is a paragraph.
    <block_quote>
        <block_quote>
            <paragraph>
                Nested
        <paragraph>
            block quotes.
"""],
]


if __name__ == '__main__':
    unittest.main()
