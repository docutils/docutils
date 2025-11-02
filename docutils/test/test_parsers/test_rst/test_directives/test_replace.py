#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "replace" directive.
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
    maxDiff = None

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

totest['replace'] = [
["""\
Test the |name| directive.

.. |name| replace:: "**replace**"
""",
"""\
<document source="test data">
    <paragraph>
        Test the \n\
        <substitution_reference refname="name">
            name
         directive.
    <substitution_definition names="name">
        "
        <strong>
            replace
        "
"""],
["""\
.. |name| replace:: paragraph 1

                    paragraph 2
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "replace" directive: may contain a single paragraph only.
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "name" empty or invalid.
        <literal_block xml:space="preserve">
            .. |name| replace:: paragraph 1
            \n\
                                paragraph 2
"""],
["""\
.. |name| replace::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "replace" directive; none found.
        <literal_block xml:space="preserve">
            replace::
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "name" empty or invalid.
        <literal_block xml:space="preserve">
            .. |name| replace::
"""],
["""\
.. |Python| replace:: Python, *the* best language around

.. _Python: http://www.python.org/

I recommend you try |Python|_.
""",
"""\
<document source="test data">
    <substitution_definition names="Python">
        Python, \n\
        <emphasis>
            the
         best language around
    <target ids="python" names="python" refuri="http://www.python.org/">
    <paragraph>
        I recommend you try \n\
        <reference refname="python">
            <substitution_reference refname="Python">
                Python
        .
"""],
["""\
.. |name| replace::  *error in **inline ``markup
""",
"""\
<document source="test data">
    <system_message ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
    <system_message ids="system-message-2" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline strong start-string without end-string.
    <system_message ids="system-message-3" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline literal start-string without end-string.
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Problematic content in substitution definition
        <literal_block xml:space="preserve">
            .. |name| replace::  *error in **inline ``markup
        <block_quote>
            <paragraph>
                <problematic ids="problematic-1" refid="system-message-1">
                    *
                error in \n\
                <problematic ids="problematic-2" refid="system-message-2">
                    **
                inline \n\
                <problematic ids="problematic-3" refid="system-message-3">
                    ``
                markup
"""],
["""\
.. replace:: not valid outside of a substitution definition
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Invalid context: the "replace" directive can only be used within a substitution definition.
        <literal_block xml:space="preserve">
            .. replace:: not valid outside of a substitution definition
"""],
]


if __name__ == '__main__':
    unittest.main()
