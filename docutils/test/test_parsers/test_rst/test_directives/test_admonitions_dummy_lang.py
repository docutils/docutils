#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for admonition directives with local language module.
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
        settings.language_code = 'local-dummy-lang'
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(output, case_expected)


totest = {}

totest['admonitions'] = [
["""\
.. Dummy-Attention:: directive with silly localised name.

.. Attention:: English fallback (an INFO is written).
""",
"""\
<document source="test data">
    <attention>
        <paragraph>
            directive with silly localised name.
    <attention>
        <paragraph>
            English fallback (an INFO is written).
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main()
