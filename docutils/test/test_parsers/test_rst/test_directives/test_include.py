#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "include" directive.
"""

import os.path
from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

mydir = os.path.dirname(suite.func_code.co_filename)
include1 = os.path.join(mydir, 'include1.txt')
include1rel = DocutilsTestSupport.utils.relative_path(None, include1)
include2 = os.path.join(mydir, 'include2.txt')
include3 = os.path.join(mydir, 'include3.txt')
include8 = os.path.join(mydir, 'include8.txt')
include9 = os.path.join(mydir, 'include9.txt')
include9rel = DocutilsTestSupport.utils.relative_path(None, include9)


totest = {}

totest['include'] = [
["""\
Include Test
============

.. include:: %s

A paragraph.
""" % include1,
"""\
<document source="test data">
    <section id="include-test" name="include test">
        <title>
            Include Test
        <section id="inclusion-1" name="inclusion 1">
            <title>
                Inclusion 1
            <paragraph>
                This file is used by \n\
                <literal>
                    test_include.py
                .
            <paragraph>
                A paragraph.
"""],
["""\
Include Test
============

.. include:: %s
   :literal:

A paragraph.
""" % include1,
"""\
<document source="test data">
    <section id="include-test" name="include test">
        <title>
            Include Test
        <literal_block source="%s" xml:space="preserve">
            Inclusion 1
            -----------
            \n\
            This file is used by ``test_include.py``.
        <paragraph>
            A paragraph.
""" % include1rel],
["""\
Let's test the parse context.

    This paragraph is in a block quote.

    .. include:: %s

The included paragraphs should also be in the block quote.
""" % include2,
"""\
<document source="test data">
    <paragraph>
        Let's test the parse context.
    <block_quote>
        <paragraph>
            This paragraph is in a block quote.
        <paragraph>
            Here are some paragraphs
            that can appear at any level.
        <paragraph>
            This file (include2.txt) is used by \n\
            <literal>
                test_include.py
            .
    <paragraph>
        The included paragraphs should also be in the block quote.
"""],
["""\
Include Test
============

.. include:: nonexistent.txt

A paragraph.
""",
"""\
<document source="test data">
    <section id="include-test" name="include test">
        <title>
            Include Test
        <system_message level="4" line="4" source="test data" type="SEVERE">
            <paragraph>
                Problems with "include" directive path:
                [Errno 2] No such file or directory: 'nonexistent.txt'.
            <literal_block xml:space="preserve">
                .. include:: nonexistent.txt
        <paragraph>
            A paragraph.
"""],
["""\
Include Test
============

.. include:: %s

.. include:: %s

A paragraph.
""" % (include1, include1),
"""\
<document source="test data">
    <section id="include-test" name="include test">
        <title>
            Include Test
        <section dupname="inclusion 1" id="inclusion-1">
            <title>
                Inclusion 1
            <paragraph>
                This file is used by 
                <literal>
                    test_include.py
                .
        <section dupname="inclusion 1" id="id1">
            <title>
                Inclusion 1
            <system_message backrefs="id1" level="1" line="2" source="%s" type="INFO">
                <paragraph>
                    Duplicate implicit target name: "inclusion 1".
            <paragraph>
                This file is used by 
                <literal>
                    test_include.py
                .
            <paragraph>
                A paragraph.
""" % include1rel],
["""\
Include Test
============

.. include:: %s

----------

.. include:: %s

A paragraph.
""" % (include1, include1),
"""\
<document source="test data">
    <section id="include-test" name="include test">
        <title>
            Include Test
        <section dupname="inclusion 1" id="inclusion-1">
            <title>
                Inclusion 1
            <paragraph>
                This file is used by 
                <literal>
                    test_include.py
                .
            <transition>
            <system_message level="3" line="12" source="test data" type="ERROR">
                <paragraph>
                    Section may not end with a transition.
        <section dupname="inclusion 1" id="id1">
            <title>
                Inclusion 1
            <system_message backrefs="id1" level="1" line="2" source="%s" type="INFO">
                <paragraph>
                    Duplicate implicit target name: "inclusion 1".
            <paragraph>
                This file is used by 
                <literal>
                    test_include.py
                .
            <paragraph>
                A paragraph.
""" % include1rel],
["""\
In test data

.. include:: %s
""" % include3,
"""\
<document source="test data">
    <paragraph>
        In test data
    <paragraph>
        In include3.txt
    <paragraph>
        In includes/include4.txt
    <paragraph>
        In includes/include5.txt
    <paragraph>
        In includes/more/include6.txt
    <paragraph>
        In includes/sibling/include7.txt
"""],
["""\
In test data

Section
=======

(Section contents in nested parse; slice of input_lines ViewList.)

.. include:: %s
""" % include3,
"""\
<document source="test data">
    <paragraph>
        In test data
    <section id="section" name="section">
        <title>
            Section
        <paragraph>
            (Section contents in nested parse; slice of input_lines ViewList.)
        <paragraph>
            In include3.txt
        <paragraph>
            In includes/include4.txt
        <paragraph>
            In includes/include5.txt
        <paragraph>
            In includes/more/include6.txt
        <paragraph>
            In includes/sibling/include7.txt
"""],
["""\
Testing relative includes:

.. include:: %s
""" % include8,
"""\
<document source="test data">
    <paragraph>
        Testing relative includes:
    <paragraph>
        In include8.txt
    <paragraph>
        In ../includes/include9.txt.
    <paragraph>
        Here are some paragraphs
        that can appear at any level.
    <paragraph>
        This file (include2.txt) is used by 
        <literal>
            test_include.py
        .
"""],
# @@@ BUG with errors reported with incorrect "source" & "line":
# ["""\
# Testing bad charent includes:
#
# .. include:: %s
# """ % include9,
# """\
# <document source="test data">
#     <paragraph>
#         Testing bad charent includes:
#     <system_message level="3" line="1" source="%s" type="ERROR">
#         <paragraph>
#             Invalid character code: 0xFFFFFFFFF
#             int() literal too large: FFFFFFFFF
#         <literal_block xml:space="preserve">
#             unicode:: 0xFFFFFFFFF
#     <system_message level="2" line="1" source="%s" type="WARNING">
#         <paragraph>
#             Substitution definition "bad" empty or invalid.
#         <literal_block xml:space="preserve">
#             .. |bad| unicode:: 0xFFFFFFFFF
# """ % (include9rel, include9rel)],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
