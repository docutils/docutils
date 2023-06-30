#! /usr/bin/env python3

# $Id: test_replace.py 4667 2006-07-12 21:40:56Z wiemann $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "replace" directive.
Test in french (not default/fallback language).
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
        settings.language_code = 'fr'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['replace'] = [
["""\
Test directive containing french role exposant (superscript).

.. |Na+| remplace:: Na\\ :exp:`+`

Le |Na+| est l'ion sodium.
""",
"""\
<document source="test data">
    <paragraph>
        Test directive containing french role exposant (superscript).
    <substitution_definition names="Na+">
        Na
        <superscript>
            +
    <paragraph>
        Le \n\
        <substitution_reference refname="Na+">
            Na+
         est l\'ion sodium.
"""],
["""\
Test directive containing english role superscript.

.. |Na+| remplace:: Na\\ :sup:`+`

Le |Na+| est l'ion sodium.
""",
"""\
<document source="test data">
    <paragraph>
        Test directive containing english role superscript.
    <system_message level="1" line="3" source="test data" type="INFO">
        <paragraph>
            No role entry for "sup" in module "docutils.parsers.rst.languages.fr".
            Using English fallback for role "sup".
    <substitution_definition names="Na+">
        Na
        <superscript>
            +
    <paragraph>
        Le \n\
        <substitution_reference refname="Na+">
            Na+
         est l\'ion sodium.
"""],
]


if __name__ == '__main__':
    unittest.main()
