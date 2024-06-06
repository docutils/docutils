#!/usr/bin/env python3
# :Copyright: © 2024 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""Tests for `docutils.parsers.docutils_xml.Parser.parse()`."""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers import docutils_xml
from docutils.utils import new_document

parser = docutils_xml.Parser()


class XmlParserTestCase(unittest.TestCase):
    def test_parse(self):
        settings = get_default_settings(parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings)
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['simple'] = [
["""\
<document>
    <paragraph>A paragraph.</paragraph>
</document>
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph.
"""],
["""\
<document source="test sample">
    <paragraph>A paragraph
        with two lines.</paragraph>
</document>
""",
"""\
<document source="test sample">
    <paragraph>
        A paragraph
        with two lines.
"""],
["""\
<document>
    <paragraph>Paragraph 1.</paragraph>
    <paragraph>Paragraph 2.</paragraph>
</document>
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph 1.
    <paragraph>
        Paragraph 2.
"""],
["""\
<paragraph>Sub-trees are parsed, too.
Line breaks are preserved.</paragraph>
""",
"""\
<document source="test data">
    <paragraph>
        Sub-trees are parsed, too.
        Line breaks are preserved.
"""],
]

totest['inline'] = [
["""\
<paragraph><emphasis>emphatically</emphasis></paragraph>
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphatically
"""],
["""\
<paragraph><strong>strong</strong> week</paragraph>
""",
"""\
<document source="test data">
    <paragraph>
        <strong>
            strong
         week
"""],
["""\
<document source="test data">
    <literal_block xml:space="preserve">  Inline element
with <strong>  space at start,
    in the middle</strong>
  and after end.</literal_block>
</document>
""",
"""\
<document source="test data">
    <literal_block xml:space="preserve">
          Inline element
        with \n\
        <strong>
              space at start,
                in the middle
        \n\
          and after end.
"""],
]

totest['attributes'] = [
["""\
<paragraph classes="top secret">classified text</paragraph>
""",
"""\
<document source="test data">
    <paragraph classes="top secret">
        classified text
"""],
[r"""
<paragraph ids="ref-2 ref-1" names="ref\ 2 ref\ 1">target paragraph</paragraph>
""",
r"""<document source="test data">
    <paragraph ids="ref-2 ref-1" names="ref\ 2 ref\ 1">
        target paragraph
"""],
]

totest['invalid'] = [
["""\
<document>
    <tip>
        spurious text
        <paragraph>A paragraph.</paragraph>
    </tip>
</document>
""",
"""\
<document source="test data">
    <tip>
        spurious text
        <paragraph>
            A paragraph.
"""],
["""\
<document>
    spurious text
    <paragraph>A paragraph.</paragraph>
</document>
""",
"""\
<document source="test data">
    spurious text
    <paragraph>
        A paragraph.
"""],
["""\
<document>
    <tip>
        <paragraph>A paragraph.</paragraph>
        spurious tailing text
    </tip>
</document>
""",
"""\
<document source="test data">
    <tip>
        <paragraph>
            A paragraph.
        spurious tailing text
"""],
["""\
<document>
    <paragraph>A paragraph.</paragraph>
    spurious tailing text
</document>
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph.
    spurious tailing text
"""],
]


if __name__ == '__main__':
    unittest.main()
