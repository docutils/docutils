#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for transition markers.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

# See DocutilsTestSupport.ParserTestSuite.generateTests for a
# description of the 'totest' data structure.
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
    <section id="section-1" name="section 1">
        <title>
            Section 1
        <paragraph>
            First text division of section 1.
        <transition>
        <paragraph>
            Second text division of section 1.
        <section id="section-2" name="section 2">
            <title>
                Section 2
            <paragraph>
                Paragraph 2 in section 2.
"""],
["""\
--------

A section or document may not begin with a transition.

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
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Document or section may not begin with a transition.
    <transition>
    <paragraph>
        A section or document may not begin with a transition.
    <paragraph>
        The DTD specifies that two transitions may not
        be adjacent:
    <transition>
    <system_message level="3" line="10" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions not allowed.
    <transition>
    <system_message level="3" line="12" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions not allowed.
    <transition>
    <paragraph>
        The DTD also specifies that a section or document
        may not end with a transition.
    <transition>
    <system_message level="3" line="17" source="test data" type="ERROR">
        <paragraph>
            Document or section may not end with a transition.
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
        <system_message level="4" line="5" source="test data" type="SEVERE">
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

Illegal transitions.

----------

Section 2
=========

----------
""",
"""\
<document source="test data">
    <paragraph>
        Sections with transitions at beginning and end.
    <section id="section-1" name="section 1">
        <title>
            Section 1
        <system_message level="3" line="6" source="test data" type="ERROR">
            <paragraph>
                Section may not begin with a transition.
        <transition>
        <paragraph>
            Illegal transitions.
        <transition>
        <system_message level="3" line="10" source="test data" type="ERROR">
            <paragraph>
                Section may not end with a transition.
    <section id="section-2" name="section 2">
        <title>
            Section 2
        <system_message level="3" line="15" source="test data" type="ERROR">
            <paragraph>
                Section may not begin with a transition.
        <transition>
        <system_message level="3" line="15" source="test data" type="ERROR">
            <paragraph>
                Document or section may not end with a transition.
"""],
["""\
----------

Document beginning with a transition.
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Document or section may not begin with a transition.
    <transition>
    <paragraph>
        Document beginning with a transition.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
