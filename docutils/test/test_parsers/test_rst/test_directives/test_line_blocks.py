#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the body.py 'line-block' directive.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[4]))

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

totest['line_blocks'] = [
["""\
.. line-block::

   This is a line block.
   Newlines are *preserved*.
       As is initial whitespace.
""",
"""\
<document source="test data">
    <line_block>
        <line>
            This is a line block.
        <line>
            Newlines are \n\
            <emphasis>
                preserved
            .
        <line_block>
            <line>
                As is initial whitespace.
"""],
["""\
.. line-block::
   :class: linear
   :name:  cit:short

   This is a line block with options.
""",
"""\
<document source="test data">
    <line_block classes="linear" ids="cit-short" names="cit:short">
        <line>
            This is a line block with options.
"""],
["""\
.. line-block::

   Inline markup *may not span
       multiple lines* of a line block.
""",
"""\
<document source="test data">
    <line_block>
        <line>
            Inline markup \n\
            <problematic ids="problematic-1" refid="system-message-1">
                *
            may not span
        <line_block>
            <line>
                multiple lines* of a line block.
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
["""\
.. line-block::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "line-block" directive; none found.
        <literal_block xml:space="preserve">
            .. line-block::
"""],
]


if __name__ == '__main__':
    unittest.main()
