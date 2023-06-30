#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['paragraphs'] = [
["""\
A paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph.
"""],
["""\
Paragraph 1.

Paragraph 2.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph 1.
    <paragraph>
        Paragraph 2.
"""],
["""\
Line 1.
Line 2.
Line 3.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
        Line 3.
"""],
["""\
Paragraph 1, Line 1.
Line 2.
Line 3.

Paragraph 2, Line 1.
Line 2.
Line 3.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph 1, Line 1.
        Line 2.
        Line 3.
    <paragraph>
        Paragraph 2, Line 1.
        Line 2.
        Line 3.
"""],
["""\
A. Einstein was a really
smart dude.
""",
"""\
<document source="test data">
    <paragraph>
        A. Einstein was a really
        smart dude.
"""],
]

if __name__ == '__main__':
    unittest.main()
