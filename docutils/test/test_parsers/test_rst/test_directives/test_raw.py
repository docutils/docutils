#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "raw" directive.
"""

import os.path
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

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = os.path.abspath(os.path.join(__file__, '..', '..', '..', '..'))


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.halt_level = 5
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


mydir = os.path.join(TEST_ROOT, 'test_parsers/test_rst/test_directives')
raw1 = os.path.relpath(
    os.path.join(mydir, 'raw1.rst'),
    os.getcwd()).replace('\\', '/')
utf_16_file = os.path.relpath(
    os.path.join(TEST_ROOT, 'data/utf-16-le-sig.rst'),
    os.getcwd()).replace('\\', '/')
utf_16_error_str = ("UnicodeDecodeError: 'ascii' codec can't decode byte 0xff "
                    "in position 0: ordinal not in range(128)")

totest = {}

totest['raw'] = [
["""\
.. raw:: html

   <span>This is some plain old raw text.</span>
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        <span>This is some plain old raw text.</span>
"""],
[f"""\
.. raw:: html
   :file: {raw1}
""",
f"""\
<document source="test data">
    <raw format="html" source="{raw1}" xml:space="preserve">
        <p>This file is used by <tt>test_raw.py</tt>.</p>
"""],
["""\
.. raw:: html
   :file: rawfile.html
   :url: http://example.org/
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            The "file" and "url" options may not be simultaneously specified for the "raw" directive.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: rawfile.html
               :url: http://example.org/
"""],
["""\
.. raw:: html
   :file: rawfile.html

   <p>Can't have both content and file attribute.</p>
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            "raw" directive may not both specify an external file and have content.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: rawfile.html
            \n\
               <p>Can't have both content and file attribute.</p>
"""],
[r"""
.. raw:: latex html

   \[ \sum_{n=1}^\infty \frac{1}{n} \text{ etc.} \]
""",
"""\
<document source="test data">
    <raw format="latex html" xml:space="preserve">
        \\[ \\sum_{n=1}^\\infty \\frac{1}{n} \\text{ etc.} \\]
"""],
[f"""\
.. raw:: html
   :file: {utf_16_file}
   :encoding: utf-16
""",
f"""\
<document source="test data">
    <raw format="html" source="{utf_16_file}" xml:space="preserve">
        Grüße
"""],
[f"""\
Raw input file is UTF-16-encoded, and is not valid ASCII.

.. raw:: html
   :file: {utf_16_file}
   :encoding: ascii
""",
f"""\
<document source="test data">
    <paragraph>
        Raw input file is UTF-16-encoded, and is not valid ASCII.
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problem with "raw" directive:
            {utf_16_error_str}
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: {utf_16_file}
               :encoding: ascii
"""],
["""\
.. raw:: html
   :encoding: utf-8

   Should the parser complain becau\xdfe there is no :file:?  BUG?
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        Should the parser complain becau\xdfe there is no :file:?  BUG?
"""],
["""\
.. raw:: html
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "raw" directive; none found.
        <literal_block xml:space="preserve">
            .. raw:: html
"""],
["""\
.. raw:: html
   :file: non-existent.file
""",
"""\
<document source="test data">
    <system_message level="4" line="1" source="test data" type="SEVERE">
        <paragraph>
            Problems with "raw" directive path:
            InputError: [Errno 2] No such file or directory: 'non-existent.file'.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: non-existent.file
"""],
# note that this output is rewritten below for certain python versions
]

if __name__ == '__main__':
    unittest.main()
