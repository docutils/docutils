#! /usr/bin/env python3

# $Id$
# Author: Guenter Milde <milde@users.sf.net>
# Copyright: This module has been placed in the public domain.

"""
Tests for the 'math' directive.
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

totest['argument'] = [
["""\
.. math:: y = f(x)
""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        y = f(x)
"""],
]

totest['content'] = [
["""\
.. math::

  1+1=2
""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        1+1=2
"""],
]

totest['options'] = [
["""\
.. math::
  :class: new
  :name: eq:Eulers law

  e^i*2*\\pi = 1
""",
"""\
<document source="test data">
    <math_block classes="new" ids="eq-eulers-law" names="eq:eulers\\ law" xml:space="preserve">
        e^i*2*\\pi = 1
"""],
]

totest['argument_and_content'] = [
["""\
.. math:: y = f(x)

  1+1=2

""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        y = f(x)
    <math_block xml:space="preserve">
        1+1=2
"""],
]

totest['content_with_blank_line'] = [
["""\
.. math::

  1+1=2

  E = mc^2
""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        1+1=2
    <math_block xml:space="preserve">
        E = mc^2
"""],
]


if __name__ == '__main__':
    unittest.main()
