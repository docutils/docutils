#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "default-role" directive.
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

totest['default_role'] = [
["""\
.. default-role:: subscript

This is a `subscript`.
""",
"""\
<document source="test data">
    <paragraph>
        This is a \n\
        <subscript>
            subscript
        .
"""],
["""\
Must define a custom role before using it.

.. default-role:: custom
""",
"""\
<document source="test data">
    <paragraph>
        Must define a custom role before using it.
    <system_message level="1" line="3" source="test data" type="INFO">
        <paragraph>
            No role entry for "custom" in module "docutils.parsers.rst.languages.en".
            Trying "custom" as canonical role name.
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Unknown interpreted text role "custom".
        <literal_block xml:space="preserve">
            .. default-role:: custom
"""],
["""\
.. role:: custom
.. default-role:: custom

This text uses the `default role`.

.. default-role::

Returned the `default role` to its standard default.
""",
"""\
<document source="test data">
    <paragraph>
        This text uses the \n\
        <inline classes="custom">
            default role
        .
    <paragraph>
        Returned the \n\
        <title_reference>
            default role
         to its standard default.
"""],
]


if __name__ == '__main__':
    unittest.main()
