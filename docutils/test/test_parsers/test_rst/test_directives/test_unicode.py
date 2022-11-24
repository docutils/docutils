#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "unicode" directive.
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


try:
    chr(0x111111111111111111)
except OverflowError as unichr_exception:
    unichr_exception_string = f'code too large ({unichr_exception})'
except Exception as unichr_exception:
    unichr_exception_string = str(unichr_exception)
else:
    unichr_exception_string = ''

try:
    chr(0x11111111)
except Exception as detail:
    invalid_char_code = f'{detail.__class__.__name__}: {detail}'
else:
    invalid_char_code = ''

totest = {}

totest['unicode'] = [
["""
Insert an em-dash (|mdash|), a copyright symbol (|copy|), a non-breaking
space (|nbsp|), a backwards-not-equals (|bne|), and a captial omega (|Omega|).

.. |mdash| unicode:: 0x02014
.. |copy| unicode:: \\u00A9
.. |nbsp| unicode:: &#x000A0;
.. |bne| unicode:: U0003D U020E5
.. |Omega| unicode:: U+003A9
""",
"""\
<document source="test data">
    <paragraph>
        Insert an em-dash (
        <substitution_reference refname="mdash">
            mdash
        ), a copyright symbol (
        <substitution_reference refname="copy">
            copy
        ), a non-breaking
        space (
        <substitution_reference refname="nbsp">
            nbsp
        ), a backwards-not-equals (
        <substitution_reference refname="bne">
            bne
        ), and a captial omega (
        <substitution_reference refname="Omega">
            Omega
        ).
    <substitution_definition names="mdash">
        \u2014
    <substitution_definition names="copy">
        \u00A9
    <substitution_definition names="nbsp">
        \u00A0
    <substitution_definition names="bne">
        =
        \u20e5
    <substitution_definition names="Omega">
        \u03a9
"""],
["""
Bad input:

.. |empty| unicode::
.. |empty too| unicode:: .. comment doesn't count as content
.. |not hex| unicode:: 0xHEX
.. |not all hex| unicode:: UABCX
.. unicode:: not in a substitution definition
""",
"""\
<document source="test data">
    <paragraph>
        Bad input:
    <system_message level="3" line="4" source="test data" type="ERROR">
        <paragraph>
            Error in "unicode" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            unicode::
    <system_message level="2" line="4" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "empty" empty or invalid.
        <literal_block xml:space="preserve">
            .. |empty| unicode::
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "empty too" empty or invalid.
        <literal_block xml:space="preserve">
            .. |empty too| unicode:: .. comment doesn't count as content
    <substitution_definition names="not\\ hex">
        0xHEX
    <substitution_definition names="not\\ all\\ hex">
        UABCX
    <system_message level="3" line="8" source="test data" type="ERROR">
        <paragraph>
            Invalid context: the "unicode" directive can only be used within a substitution definition.
        <literal_block xml:space="preserve">
            .. unicode:: not in a substitution definition
"""],
["""
Testing comments and extra text.

Copyright |copy| 2003, |BogusMegaCorp (TM)|.

.. |copy| unicode:: 0xA9 .. copyright sign
.. |BogusMegaCorp (TM)| unicode:: BogusMegaCorp U+2122
   .. with trademark sign
""",
"""\
<document source="test data">
    <paragraph>
        Testing comments and extra text.
    <paragraph>
        Copyright \n\
        <substitution_reference refname="copy">
            copy
         2003, \n\
        <substitution_reference refname="BogusMegaCorp (TM)">
            BogusMegaCorp (TM)
        .
    <substitution_definition names="copy">
        \u00A9
    <substitution_definition names="BogusMegaCorp\\ (TM)">
        BogusMegaCorp
        \u2122
"""],
["""
.. |too big for int| unicode:: 0x111111111111111111
.. |too big for unicode| unicode:: 0x11111111
""",
"""\
<document source="test data">
    <system_message level="3" line="2" source="test data" type="ERROR">
        <paragraph>
            Invalid character code: 0x111111111111111111
            ValueError: %s
        <literal_block xml:space="preserve">
            unicode:: 0x111111111111111111
    <system_message level="2" line="2" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "too big for int" empty or invalid.
        <literal_block xml:space="preserve">
            .. |too big for int| unicode:: 0x111111111111111111
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Invalid character code: 0x11111111
            %s
        <literal_block xml:space="preserve">
            unicode:: 0x11111111
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "too big for unicode" empty or invalid.
        <literal_block xml:space="preserve">
            .. |too big for unicode| unicode:: 0x11111111
""" % (unichr_exception_string, invalid_char_code)]
]


if __name__ == '__main__':
    unittest.main()
