#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for transition markers.
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


class ParserTestCase(unittest.TestCase):
    maxDiff = None

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


totest = {}

# Each dictionary key (test type name) maps to a list of tests. Each
# test is a list: input, expected output.
totest['transitions'] = [
["""\
Test transition markers.

--------

Paragraph
""",
"""\
<document source="test data">
    <paragraph>
        Test transition markers.
    <transition>
    <paragraph>
        Paragraph
"""],
["""\
Section 1
=========
First text division of section 1.

--------

Second text division of section 1.

Section 2
---------
Paragraph 2 in section 2.
""",
"""\
<document source="test data">
    <section ids="section-1" names="section\\ 1">
        <title>
            Section 1
        <paragraph>
            First text division of section 1.
        <transition>
        <paragraph>
            Second text division of section 1.
        <section ids="section-2" names="section\\ 2">
            <title>
                Section 2
            <paragraph>
                Paragraph 2 in section 2.
"""],
# TODO: disable this test case, as it endorses non-standard results?
["""\
--------

According to the DTD, a section or document may not begin with a transition.

Note: There is currently no warning, but in future these
DTD violations should be prevented or at least trigger a warning.
Alternatively, the DTD may be relaxed to accomodate for more use cases.

The DTD specifies that two transitions may not
be adjacent:

--------

--------

--------

The DTD also specifies that a section or document
may not end with a transition.

--------
""",
"""\
<document source="test data">
    <transition>
    <paragraph>
        According to the DTD, a section or document may not begin with a transition.
    <paragraph>
        Note: There is currently no warning, but in future these
        DTD violations should be prevented or at least trigger a warning.
        Alternatively, the DTD may be relaxed to accomodate for more use cases.
    <paragraph>
        The DTD specifies that two transitions may not
        be adjacent:
    <transition>
    <transition>
    <transition>
    <paragraph>
        The DTD also specifies that a section or document
        may not end with a transition.
    <transition>
"""],
["""\
Test unexpected transition markers.

    Block quote.

    --------

    Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Test unexpected transition markers.
    <block_quote>
        <paragraph>
            Block quote.
        <system_message level="3" line="5" source="test data" type="ERROR">
            <paragraph>
                Unexpected section title or transition.
            <literal_block xml:space="preserve">
                --------
        <paragraph>
            Paragraph.
"""],
["""\
Short transition marker.

---

Paragraph
""",
"""\
<document source="test data">
    <paragraph>
        Short transition marker.
    <paragraph>
        ---
    <paragraph>
        Paragraph
"""],
["""\
Sections with transitions at beginning and end.

Section 1
=========

----------

The next transition is legal:

----------

Section 2
=========

----------
""",
"""\
<document source="test data">
    <paragraph>
        Sections with transitions at beginning and end.
    <section ids="section-1" names="section\\ 1">
        <title>
            Section 1
        <transition>
        <paragraph>
            The next transition is legal:
        <transition>
    <section ids="section-2" names="section\\ 2">
        <title>
            Section 2
        <transition>
"""],
["""\
A paragraph, two transitions, and a blank line.

----------

----------

""",
"""\
<document source="test data">
    <paragraph>
        A paragraph, two transitions, and a blank line.
    <transition>
    <transition>
"""],
["""\
A paragraph and two transitions.

----------

----------
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph and two transitions.
    <transition>
    <transition>
"""],
["""\
----------

Document beginning with a transition.
""",
"""\
<document source="test data">
    <transition>
    <paragraph>
        Document beginning with a transition.
"""],
["""\
Section 1
=========

Subsection 1
------------

Some text.

----------

Section 2
=========

Some text.
""",
"""\
<document source="test data">
    <section ids="section-1" names="section\\ 1">
        <title>
            Section 1
        <section ids="subsection-1" names="subsection\\ 1">
            <title>
                Subsection 1
            <paragraph>
                Some text.
            <transition>
    <section ids="section-2" names="section\\ 2">
        <title>
            Section 2
        <paragraph>
            Some text.
"""],
["""\
Section 1
=========

----------

----------

----------

Section 2
=========

Some text.
""",
"""\
<document source="test data">
    <section ids="section-1" names="section\\ 1">
        <title>
            Section 1
        <transition>
        <transition>
        <transition>
    <section ids="section-2" names="section\\ 2">
        <title>
            Section 2
        <paragraph>
            Some text.
"""],
["""\
----------

----------

----------
""",
"""\
<document source="test data">
    <transition>
    <transition>
    <transition>
"""],
["""\
A paragraph.

----------

""",
"""\
<document source="test data">
    <paragraph>
        A paragraph.
    <transition>
"""],
]


if __name__ == '__main__':
    unittest.main()
