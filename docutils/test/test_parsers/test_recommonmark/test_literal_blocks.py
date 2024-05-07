#! /usr/bin/env python3
# $Id$
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
Tests for literal blocks in CommonMark parsers
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

totest['literal_blocks'] = [
["""\
A paragraph:

    A literal block (indented code block).
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
        A literal block (indented code block).
"""],
["""\
A paragraph:
~~~
A literal block (fenced code block).
~~~
Another paragraph:

    Another literal block.
    With two blank lines following.


A final paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
        A literal block (fenced code block).
    <paragraph>
        Another paragraph:
    <literal_block classes="code" xml:space="preserve">
        Another literal block.
        With two blank lines following.
    <paragraph>
        A final paragraph.
"""],
["""\
A paragraph
on more than
one line.
    No literal block
    but paragraph continuation lines.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph
        on more than
        one line.
        No literal block
        but paragraph continuation lines.
"""],
["""\
A paragraph:

    A literal block.
no blank line
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
        A literal block.
    <paragraph>
        no blank line
"""],
["""\
A paragraph:
```
  A fenced code block.
```
no blank lines.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
          A fenced code block.
    <paragraph>
        no blank lines.
"""],
[r"""
A paragraph:

   Not a literal block because only indented 3 spaces.
""",
r"""<document source="test data">
    <paragraph>
        A paragraph:
    <paragraph>
        Not a literal block because only indented 3 spaces.
"""],
[r"""
    A literal block.

   Not a literal block.
""",
r"""<document source="test data">
    <literal_block classes="code" xml:space="preserve">
        A literal block.
    <paragraph>
        Not a literal block.
"""],
["""\
A paragraph:

      A wonky literal block.
    Literal line 2.

      Literal line 3.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
          A wonky literal block.
        Literal line 2.
        \n\
          Literal line 3.
"""],
["""\
A paragraph:
~~~
  A fenced code block.
Literal line 2.

  Literal line 3.
~~~
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code" xml:space="preserve">
          A fenced code block.
        Literal line 2.
        \n\
          Literal line 3.
"""],
["""\
A paragraph:
~~~ ruby
A literal block (fenced code block)
with *info string*.
~~~
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph:
    <literal_block classes="code ruby" xml:space="preserve">
        A literal block (fenced code block)
        with *info string*.
"""],
]


if __name__ == '__main__':
    unittest.main()
