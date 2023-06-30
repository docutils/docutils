#! /usr/bin/env python3

# $Id: test_interpreted.py 6424 2010-09-18 10:43:52Z smerten $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for interpreted text in docutils/parsers/rst/states.py.
Test not default/fallback language french.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser, roles
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.language_code = 'fr'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                # Language-specific roles and roles added by the
                # "default-role" and "role" directives are currently stored
                # globally in the roles._roles dictionary.  This workaround
                # empties that dictionary.
                roles._roles = {}
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['basics'] = [
["""\
Simple explicit roles and english fallbacks:
:acronym:`acronym`,
:exp:`superscript`,
:ind:`subscript`,
:titre:`title reference`.
""",
"""\
<document source="test data">
    <paragraph>
        Simple explicit roles and english fallbacks:
        <acronym>
            acronym
        ,
        <superscript>
            superscript
        ,
        <subscript>
            subscript
        ,
        <title_reference>
            title reference
        .
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            No role entry for "acronym" in module "docutils.parsers.rst.languages.fr".
            Using English fallback for role "acronym".
"""],
]

if __name__ == '__main__':
    unittest.main()
