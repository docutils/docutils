#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Substitutions.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.references import Substitutions
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    maxDiff = None

    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, (transforms, cases) in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    # Don't do a ``populate_from_components()`` because that
                    # would enable the Transformer's default transforms.
                    document.transformer.add_transforms(transforms)
                    document.transformer.add_transform(TestMessages)
                    document.transformer.apply_transforms()
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['substitutions'] = ((Substitutions,), [
["""\
The |biohazard| symbol is deservedly scary-looking.

.. |biohazard| image:: biohazard.png
""",
"""\
<document source="test data">
    <paragraph>
        The \n\
        <image alt="biohazard" uri="biohazard.png">
         symbol is deservedly scary-looking.
    <substitution_definition names="biohazard">
        <image alt="biohazard" uri="biohazard.png">
"""],
["""\
Here's an |unknown| substitution.
""",
"""\
<document source="test data">
    <paragraph>
        Here's an \n\
        <problematic ids="problematic-1" refid="system-message-1">
            |unknown|
         substitution.
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Undefined substitution referenced: "unknown".
"""],
["""\
Substitutions support case differences:

.. |eacute| replace:: \u00E9
.. |Eacute| replace:: \u00C9

|Eacute|\\t\\ |eacute|, and even |EACUTE|.
""",
"""\
<document source="test data">
    <paragraph>
        Substitutions support case differences:
    <substitution_definition names="eacute">
        \u00E9
    <substitution_definition names="Eacute">
        \u00C9
    <paragraph>
        \u00C9
        t
        \u00E9
        , and even \n\
        \u00C9
        .
"""],
["""\
Indirect substitution definitions with multiple references:

|substitute| my coke for gin
|substitute| you for my mum
at least I'll get my washing done

.. |substitute| replace:: |replace|
.. |replace| replace:: swap
""",
"""\
<document source="test data">
    <paragraph>
        Indirect substitution definitions with multiple references:
    <paragraph>
        swap
         my coke for gin
        swap
         you for my mum
        at least I'll get my washing done
    <substitution_definition names="substitute">
        swap
    <substitution_definition names="replace">
        swap
"""],
["""\
.. |l| unicode:: U+00AB .. left chevron
.. |r| unicode:: U+00BB .. right chevron
.. |.| replace:: |l|\\ ``.``\\ |r|

.. Delete either of the following lines, and there is no error.

Regular expression |.| will match any character

.. Note:: Note that |.| matches *exactly* one character
""",
"""\
<document source="test data">
    <substitution_definition names="l">
        \xab
    <substitution_definition names="r">
        \xbb
    <substitution_definition names=".">
        \xab
        <literal>
            .
        \xbb
    <comment xml:space="preserve">
        Delete either of the following lines, and there is no error.
    <paragraph>
        Regular expression \n\
        \xab
        <literal>
            .
        \xbb
         will match any character
    <note>
        <paragraph>
            Note that \n\
            \xab
            <literal>
                .
            \xbb
             matches \n\
            <emphasis>
                exactly
             one character
"""],
["""\
.. |sub| replace:: |sub|
""",
"""\
<document source="test data">
    <system_message level="3" line="1" names="sub" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |sub| replace:: |sub|
"""],
["""\
.. |sub| replace:: |indirect1|
.. |indirect1| replace:: |indirect2|
.. |indirect2| replace:: |Sub|
""",
"""\
<document source="test data">
    <system_message level="3" line="1" names="sub" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |sub| replace:: |indirect1|
    <system_message level="3" line="2" names="indirect1" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |indirect1| replace:: |indirect2|
    <system_message level="3" line="3" names="indirect2" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |indirect2| replace:: |Sub|
"""],
["""\
.. |indirect1| replace:: |indirect2|
.. |indirect2| replace:: |Sub|
.. |sub| replace:: |indirect1|

Use |sub| and |indirect1| and |sub| again (and |sub| one more time).
""",
"""\
<document source="test data">
    <system_message level="3" line="1" names="indirect1" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |indirect1| replace:: |indirect2|
    <system_message level="3" line="2" names="indirect2" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |indirect2| replace:: |Sub|
    <system_message level="3" line="3" names="sub" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition detected:
        <literal_block xml:space="preserve">
            .. |sub| replace:: |indirect1|
    <paragraph>
        Use \n\
        <problematic ids="problematic-4" refid="system-message-4">
            |Sub|
         and \n\
        <problematic ids="problematic-1" refid="system-message-1">
            |indirect1|
         and \n\
        <problematic ids="problematic-2" refid="system-message-2">
            |sub|
         again (and \n\
        <problematic ids="problematic-3" refid="system-message-3">
            |sub|
         one more time).
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition referenced: "indirect1".
    <system_message backrefs="problematic-2" ids="system-message-2" level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition referenced: "sub".
    <system_message backrefs="problematic-3" ids="system-message-3" level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition referenced: "sub".
    <system_message backrefs="problematic-4" ids="system-message-4" level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Circular substitution definition referenced: "Sub".
"""],
["""\
Substitution reference with |reference-in-content|.

.. |reference-in-content| replace:: text and hyperlink-reference_
""",
"""\
<document source="test data">
    <paragraph>
        Substitution reference with \n\
        text and \n\
        <reference name="hyperlink-reference" refname="hyperlink-reference">
            hyperlink-reference
        .
    <substitution_definition names="reference-in-content">
        text and \n\
        <reference name="hyperlink-reference" refname="hyperlink-reference">
            hyperlink-reference
"""],
])

totest['unicode'] = ((Substitutions,), [
["""\
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
        \u2014
        ), a copyright symbol (
        \u00a9
        ), a non-breaking
        space (
        \u00a0
        ), a backwards-not-equals (
        =
        \u20e5
        ), and a captial omega (
        \u03a9
        ).
    <substitution_definition names="mdash">
        \u2014
    <substitution_definition names="copy">
        \u00a9
    <substitution_definition names="nbsp">
        \u00a0
    <substitution_definition names="bne">
        =
        \u20e5
    <substitution_definition names="Omega">
        \u03a9
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
        \u00a9
         2003, \n\
        BogusMegaCorp
        \u2122
        .
    <substitution_definition names="copy">
        \u00a9
    <substitution_definition names="BogusMegaCorp\\ (TM)">
        BogusMegaCorp
        \u2122
"""],
["""\
Insert an em-dash |---| automatically trimming whitespace.
Some substitutions |TM| only need |rarrow| trimming on one side.

.. |---| unicode:: U+02014
   :trim:
.. |TM| unicode:: U+02122
   :ltrim:
.. |rarrow| unicode:: U+2192
   :rtrim:
""",
"""\
<document source="test data">
    <paragraph>
        Insert an em-dash
        \u2014
        automatically trimming whitespace.
        Some substitutions
        \u2122
         only need \n\
        \u2192
        trimming on one side.
    <substitution_definition ltrim="1" names="---" rtrim="1">
        \u2014
    <substitution_definition ltrim="1" names="TM">
        \u2122
    <substitution_definition names="rarrow" rtrim="1">
        \u2192
"""],
["""\
Substitution definition with an illegal element:

.. |target| replace:: _`target`

Make sure this substitution definition is not registered: |target|
""",
"""\
<document source="test data">
    <paragraph>
        Substitution definition with an illegal element:
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Targets (names and identifiers) are not supported in a substitution definition.
        <literal_block xml:space="preserve">
            .. |target| replace:: _`target`
    <paragraph>
        Make sure this substitution definition is not registered: \n\
        <problematic ids="problematic-1" refid="system-message-1">
            |target|
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Undefined substitution referenced: "target".
"""],
])


if __name__ == '__main__':
    unittest.main()
