#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for interpreted text in docutils/parsers/rst/states.py.
"""

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
from docutils.utils.code_analyzer import with_pygments


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            if name == 'code_parsing' and not with_pygments:
                self.skipTest('syntax highlight requires pygments')
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['basics'] = [
["""\
`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted
"""],
["""\
:title:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted
"""],
["""\
`interpreted`:title:
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted
"""],
["""\
`interpreted \\`title``
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted `title`
"""],
["""\
:title:`:not-role: interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            :not-role: interpreted
"""],
["""\
`interpreted` but not \\`interpreted` [`] or ({[`] or [`]}) or `
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted
         but not `interpreted` [`] or ({[`] or [`]}) or `
"""],
["""\
`interpreted`-text `interpreted`: text `interpreted`:text `text`'s interpreted
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted
        -text \n\
        <title_reference>
            interpreted
        : text \n\
        <title_reference>
            interpreted
        :text \n\
        <title_reference>
            text
        's interpreted
"""],
["""\
`interpreted without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            `
        interpreted without closing backquote
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
["""\
`interpreted`:not a role if it contains whitespace:
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            interpreted
        :not a role if it contains whitespace:
"""],
["""\
:title:`` (empty interpreted text not recognized)
""",
"""\
<document source="test data">
    <paragraph>
        :title:`` (empty interpreted text not recognized)
"""],
["""\
:title:`\\ ` (interpreted text containing empty string)
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
         (interpreted text containing empty string)
"""],
["""\
`\\ `:title: (interpreted text containing empty string (postfix))
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
         (interpreted text containing empty string (postfix))
"""],
["""\
:title:`\\ non-empty`
""",
"""\
<document source="test data">
    <paragraph>
        <title_reference>
            non-empty
"""],
["""\
:title:`\\  ` (trailing unquoted space)
""",
"""\
<document source="test data">
    <paragraph>
        :title:
        <problematic ids="problematic-1" refid="system-message-1">
            `
         ` (trailing unquoted space)
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
["""\
Explicit roles for standard inline markup:
:emphasis:`emphasis`,
:strong:`strong`,
:literal:`inline literal text`.
""",
"""\
<document source="test data">
    <paragraph>
        Explicit roles for standard inline markup:
        <emphasis>
            emphasis
        ,
        <strong>
            strong
        ,
        <literal>
            inline literal text
        .
"""],
["""\
Simple explicit roles:
:ab:`abbreviation`,
:ac:`acronym`,
:sup:`superscript`,
:sub:`subscript`,
:title:`title reference`.
""",
"""\
<document source="test data">
    <paragraph>
        Simple explicit roles:
        <abbreviation>
            abbreviation
        ,
        <acronym>
            acronym
        ,
        <superscript>
            superscript
        ,
        <subscript>
            subscript
        ,
        <title_reference>
            title reference
        .
"""],
]

totest['code'] = [
["""\
Code role for inline code snippets:
:code:`$\alpha = \\int_0^\\infty f(x) dx$`.
""",
"""\
<document source="test data">
    <paragraph>
        Code role for inline code snippets:
        <literal classes="code">
            $\x07lpha = \\int_0^\\infty f(x) dx$
        .
"""],
]

totest['code_parsing'] = [
["""\
.. role:: tex(code)
   :language: latex

Custom role based on code role:
:tex:`$\alpha = f(x)$`.
""",
"""\
<document source="test data">
    <paragraph>
        Custom role based on code role:
        <literal classes="code tex latex">
            <inline classes="literal string">
                $
            <inline classes="name builtin">
                \x07lpha \n\
            <inline classes="operator">
                =
            <inline classes="name builtin">
                 f
            <inline classes="operator">
                (
            <inline classes="name builtin">
                x
            <inline classes="operator">
                )
            <inline classes="literal string">
                $
        .
"""],
["""\
Custom role based on code role:

.. role:: python(code)
   :language: python3
   :class: testclass

Python code :python:`print("The end")`.
""",
"""\
<document source="test data">
    <paragraph>
        Custom role based on code role:
    <paragraph>
        Python code \n\
        <literal classes="code testclass python3">
            <inline classes="name builtin">
                print
            <inline classes="punctuation">
                (
            <inline classes="literal string double">
                "The end"
            <inline classes="punctuation">
                )
        .
"""],
]

totest['references'] = [
["""\
:PEP:`0`
""",
"""\
<document source="test data">
    <paragraph>
        <reference refuri="https://peps.python.org/pep-0000">
            PEP 0
"""],
["""\
:PEP:`-1`
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            :PEP:`-1`
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            PEP number must be a number from 0 to 9999; "-1" is invalid.
"""],
["""\
:RFC:`2822`
""",
"""\
<document source="test data">
    <paragraph>
        <reference refuri="https://tools.ietf.org/html/rfc2822.html">
            RFC 2822
"""],
["""\
:RFC:`0`
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            :RFC:`0`
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            RFC number must be a number greater than or equal to 1; "0" is invalid.
"""],
["""\
:RFC:`2822#section1`
""",
"""\
<document source="test data">
    <paragraph>
        <reference refuri="https://tools.ietf.org/html/rfc2822.html#section1">
            RFC 2822
"""],
]

totest['unknown_roles'] = [
["""\
:role:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            :role:`interpreted`
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            No role entry for "role" in module "docutils.parsers.rst.languages.en".
            Trying "role" as canonical role name.
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown interpreted text role "role".
"""],
["""\
`interpreted`:role:
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            `interpreted`:role:
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            No role entry for "role" in module "docutils.parsers.rst.languages.en".
            Trying "role" as canonical role name.
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown interpreted text role "role".
"""],
["""\
:role:`interpreted`:role:
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            :role:`interpreted`:role:
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Multiple roles in interpreted text (both prefix and suffix present; only one allowed).
"""],
["""\
:very.long-role_name:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            :very.long-role_name:`interpreted`
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            No role entry for "very.long-role_name" in module "docutils.parsers.rst.languages.en".
            Trying "very.long-role_name" as canonical role name.
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown interpreted text role "very.long-role_name".
"""],
["""\
:restructuredtext-unimplemented-role:`interpreted`
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="problematic-1" refid="system-message-1">
            :restructuredtext-unimplemented-role:`interpreted`
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            No role entry for "restructuredtext-unimplemented-role" in module "docutils.parsers.rst.languages.en".
            Trying "restructuredtext-unimplemented-role" as canonical role name.
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Interpreted text role "restructuredtext-unimplemented-role" not implemented.
"""],
]


if __name__ == '__main__':
    unittest.main()
