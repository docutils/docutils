#! /usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Tests for the 'class' directive.
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

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
                    self.assertEqual(output, case_expected)


totest = {}

totest['class'] = [
["""\
.. class:: class1  class2
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.misc.ClassAttribute
             .details:
               class: ['class1', 'class2']
               directive: 'class'
"""],
["""\
.. class:: class1  class2

   The classes are applied to this paragraph.

   And this one.
""",
"""\
<document source="test data">
    <paragraph classes="class1 class2">
        The classes are applied to this paragraph.
    <paragraph classes="class1 class2">
        And this one.
"""],
]


if __name__ == '__main__':
    unittest.main()
