#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.universal.FinalChecks.
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.universal import FinalChecks
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['references'] = ((FinalChecks,), [
["""\
Unknown reference_.
""",
"""\
<document source="test data">
    <paragraph>
        Unknown \n\
        <problematic ids="id2" refid="id1">
            reference_
        .
    <system_message backrefs="id2" ids="id1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown target name: "reference".
"""],
["""\
Duplicate manual footnote labels, with reference ([1]_):

.. [1] Footnote.

.. [1] Footnote.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate manual footnote labels, with reference (
        <problematic ids="id5" refid="id4">
            [1]_
        ):
    <footnote dupnames="1" ids="id2">
        <label>
            1
        <paragraph>
            Footnote.
    <footnote dupnames="1" ids="id3">
        <label>
            1
        <system_message backrefs="id3" level="2" line="5" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "1".
        <paragraph>
            Footnote.
    <system_message backrefs="id5" ids="id4" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Duplicate target name, cannot be used as a unique reference: "1".
"""],
])


# See DocutilsTestSupport.ParserTestSuite.generateTests for a
# description of the 'totest' data structure.
totest['transitions'] = ((FinalChecks,), [
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
    <section ids="section-1" names="section 1">
        <title>
            Section 1
        <section ids="subsection-1" names="subsection 1">
            <title>
                Subsection 1
            <paragraph>
                Some text.
    <transition>
    <section ids="section-2" names="section 2">
        <title>
            Section 2
        <paragraph>
            Some text.
"""],
["""\
A paragraph.

----------

Section 1
=========

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph.
    <transition>
    <section ids="section-1" names="section 1">
        <title>
            Section 1
        <paragraph>
            Paragraph.
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
            At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <system_message level="3" line="12" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <paragraph>
        The DTD also specifies that a section or document
        may not end with a transition.
    <transition>
    <system_message level="3" line="17" source="test data" type="ERROR">
        <paragraph>
            Document may not end with a transition.
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
    <section ids="section-1" names="section 1">
        <title>
            Section 1
        <system_message level="3" line="6" source="test data" type="ERROR">
            <paragraph>
                Document or section may not begin with a transition.
        <transition>
        <paragraph>
            The next transition is legal:
    <transition>
    <section ids="section-2" names="section 2">
        <title>
            Section 2
        <system_message level="3" line="15" source="test data" type="ERROR">
            <paragraph>
                Document or section may not begin with a transition.
        <transition>
        <system_message level="3" line="15" source="test data" type="ERROR">
            <paragraph>
                Document may not end with a transition.
"""],
["""\
A paragraph and two transitions.

----------

----------
""", # the same:
"""\
<document source="test data">
    <paragraph>
        A paragraph and two transitions.
    <transition>
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Document may not end with a transition.
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
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Document may not end with a transition.
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
    <section ids="section-1" names="section 1">
        <title>
            Section 1
        <system_message level="3" line="4" source="test data" type="ERROR">
            <paragraph>
                Document or section may not begin with a transition.
        <transition>
        <system_message level="3" line="6" source="test data" type="ERROR">
            <paragraph>
                At least one body element must separate transitions; adjacent transitions are not allowed.
        <transition>
        <system_message level="3" line="8" source="test data" type="ERROR">
            <paragraph>
                At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <section ids="section-2" names="section 2">
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
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Document or section may not begin with a transition.
    <transition>
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions are not allowed.
    <transition>
    <system_message level="3" line="5" source="test data" type="ERROR">
        <paragraph>
            Document may not end with a transition.
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
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Document may not end with a transition.
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
