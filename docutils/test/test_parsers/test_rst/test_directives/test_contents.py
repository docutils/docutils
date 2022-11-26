#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for parts.py contents directive.
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
                    self.assertEqual(output, case_expected)


try:
    int('two')
except ValueError as detail:
    invalid_literal = detail.args[0]
else:
    invalid_literal = ''

totest = {}

totest['contents'] = [
["""\
.. contents::
""",
"""\
<document source="test data">
    <topic classes="contents" ids="contents" names="contents">
        <title>
            Contents
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
"""],
["""\
.. contents:: Table of Contents
""",
"""\
<document source="test data">
    <topic classes="contents" ids="table-of-contents" names="table\\ of\\ contents">
        <title>
            Table of Contents
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
"""],
["""\
.. contents::
   Table of Contents
""",
"""\
<document source="test data">
    <topic classes="contents" ids="table-of-contents" names="table\\ of\\ contents">
        <title>
            Table of Contents
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
"""],
["""\
.. contents:: Table
   of
   Contents
""",
"""\
<document source="test data">
    <topic classes="contents" ids="table-of-contents" names="table\\ of\\ contents">
        <title>
            Table
            of
            Contents
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
"""],
["""\
.. contents:: *Table* of ``Contents``
""",
"""\
<document source="test data">
    <topic classes="contents" ids="table-of-contents" names="table\\ of\\ contents">
        <title>
            <emphasis>
                Table
             of \n\
            <literal>
                Contents
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
"""],
["""\
.. contents::
   :depth: 2
   :local:
""",
"""\
<document source="test data">
    <topic classes="contents local" ids="contents" names="contents">
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
                   depth: 2
                   local: None
"""],
["""\
.. contents::
   :local: arg
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive:
            invalid option value: (option: "local"; value: 'arg')
            no argument is allowed; "arg" supplied.
        <literal_block xml:space="preserve">
            .. contents::
               :local: arg
"""],
["""\
.. contents:: Table of Contents
   :local:
   :depth: 2
   :backlinks: none
""",
"""\
<document source="test data">
    <topic classes="contents local" ids="table-of-contents" names="table\\ of\\ contents">
        <title>
            Table of Contents
        <pending>
            .. internal attributes:
                 .transform: docutils.transforms.parts.Contents
                 .details:
                   backlinks: None
                   depth: 2
                   local: None
"""],
["""\
.. contents::
   :depth: two
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive:
            invalid option value: (option: "depth"; value: 'two')
            %s.
        <literal_block xml:space="preserve">
            .. contents::
               :depth: two
""" % invalid_literal],
["""\
.. contents::
   :width: 2
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive:
            unknown option: "width".
        <literal_block xml:space="preserve">
            .. contents::
               :width: 2
"""],
["""\
.. contents::
   :backlinks: no way!
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive:
            invalid option value: (option: "backlinks"; value: 'no way!')
            "no way!" unknown; choose from "top", "entry", or "none".
        <literal_block xml:space="preserve">
            .. contents::
               :backlinks: no way!
"""],
["""\
.. contents::
   :backlinks:
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive:
            invalid option value: (option: "backlinks"; value: None)
            must supply an argument; choose from "top", "entry", or "none".
        <literal_block xml:space="preserve">
            .. contents::
               :backlinks:
"""],
["""\
* .. contents::
""",
"""\
<document source="test data">
    <bullet_list bullet="*">
        <list_item>
            <system_message level="3" line="1" source="test data" type="ERROR">
                <paragraph>
                    The "contents" directive may not be used within topics or body elements.
                <literal_block xml:space="preserve">
                    .. contents::
"""],
["""\
.. sidebar:: containing contents

   .. contents::
""",
"""\
<document source="test data">
    <sidebar>
        <title>
            containing contents
        <topic classes="contents" ids="contents" names="contents">
            <title>
                Contents
            <pending>
                .. internal attributes:
                     .transform: docutils.transforms.parts.Contents
                     .details:
"""],
]


if __name__ == '__main__':
    unittest.main()
