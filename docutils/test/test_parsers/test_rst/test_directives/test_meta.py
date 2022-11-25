#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for html meta directives.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).parents[4]))

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

totest['meta'] = [
["""\
.. meta::
   :description: The reStructuredText plaintext markup language
   :keywords: plaintext,markup language
""",
"""\
<document source="test data">
    <meta content="The reStructuredText plaintext markup language" name="description">
    <meta content="plaintext,markup language" name="keywords">
"""],
["""\
.. meta::
   :description lang=en: An amusing story
   :description lang=fr: Un histoire amusant
""",
"""\
<document source="test data">
    <meta content="An amusing story" lang="en" name="description">
    <meta content="Un histoire amusant" lang="fr" name="description">
"""],
["""\
.. meta::
   :http-equiv=Content-Type: text/html; charset=ISO-8859-1
""",
"""\
<document source="test data">
    <meta content="text/html; charset=ISO-8859-1" http-equiv="Content-Type">
"""],
["""\
.. meta::
   :name: content
     over multiple lines
""",
"""\
<document source="test data">
    <meta content="content over multiple lines" name="name">
"""],
["""\
Paragraph

.. meta::
   :name: content
""",
"""\
<document source="test data">
    <meta content="content" name="name">
    <paragraph>
        Paragraph
"""],
["""\
.. meta::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "meta" directive; none found.
        <literal_block xml:space="preserve">
            .. meta::
"""],
["""\
.. meta::
   :empty:
""",
"""\
<document source="test data">
    <system_message level="1" line="2" source="test data" type="INFO">
        <paragraph>
            No content for meta tag "empty".
        <literal_block xml:space="preserve">
            :empty:
"""],
["""\
.. meta::
   not a field list
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Invalid meta directive.
        <literal_block xml:space="preserve">
            .. meta::
               not a field list
"""],
["""\
.. meta::
   :name: content
   not a field
   :name: content
""",
"""\
<document source="test data">
    <meta content="content" name="name">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Invalid meta directive.
        <literal_block xml:space="preserve">
            .. meta::
               :name: content
               not a field
               :name: content
"""],
["""\
.. meta::
   :name: content
   :name: content
   not a field
""",
"""\
<document source="test data">
    <meta content="content" name="name">
    <meta content="content" name="name">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Invalid meta directive.
        <literal_block xml:space="preserve">
            .. meta::
               :name: content
               :name: content
               not a field
"""],
["""\
.. meta::
   :name notattval: content
""",
"""\
<document source="test data">
    <system_message level="3" line="2" source="test data" type="ERROR">
        <paragraph>
            Error parsing meta tag attribute "notattval": missing "=".
        <literal_block xml:space="preserve">
            :name notattval: content
"""],
[r"""
.. meta::
   :name\:with\:colons: escaped line\
                        break
   :unescaped:embedded:colons: content
""",
"""\
<document source="test data">
    <meta content="escaped linebreak" name="name:with:colons">
    <meta content="content" name="unescaped:embedded:colons">
"""],
]


if __name__ == '__main__':
    unittest.main()
