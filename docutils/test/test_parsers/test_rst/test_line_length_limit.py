#!/usr/bin/env python3
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
Tests for inline markup in docutils/parsers/rst/states.py.
Interpreted text tests are in a separate module, test_interpreted.py.
"""

import os.path
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

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = os.path.abspath(os.path.join(__file__, '..', '..', '..'))
docutils_conf = os.path.relpath(
    os.path.join(TEST_ROOT, 'docutils.conf'), os.getcwd()).replace('\\', '/')


class ParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.line_length_limit = 80
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['default'] = [
["""\
within the limit
%s
""" % ("x"*80),
"""\
<document source="test data">
    <paragraph>
        within the limit
        %s
""" % ("x"*80)],
["""\
above the limit
%s
""" % ("x"*81),
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Line 2 exceeds the line-length-limit.
"""],
[f"""\
Include Test
============

.. include:: {docutils_conf}

A paragraph.
""",
f"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <system_message level="2" line="4" source="test data" type="WARNING">
            <paragraph>
                "{docutils_conf}": line 5 exceeds the line-length-limit.
            <literal_block xml:space="preserve">
                .. include:: {docutils_conf}
        <paragraph>
            A paragraph.
"""],
[f"""\
Include Test 2

.. include:: {docutils_conf}
   :start-line: 3

A paragraph.
""",
f"""\
<document source="test data">
    <paragraph>
        Include Test 2
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            "{docutils_conf}": line 5 exceeds the line-length-limit.
        <literal_block xml:space="preserve">
            .. include:: {docutils_conf}
               :start-line: 3
    <paragraph>
        A paragraph.
"""],
]


if __name__ == '__main__':
    unittest.main()
