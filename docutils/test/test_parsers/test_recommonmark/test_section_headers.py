#!/usr/bin/env python3
# -*- coding: utf8 -*-
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause
"""
Test for section headings in CommonMark parsers.
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""

from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.RecommonmarkParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['section_headers'] = [
["""\
The Title
=========
Paragraph.
""",
"""\
<document source="test data">
    <section ids="the-title" names="the\ title">
        <title>
            The Title
        <paragraph>
            Paragraph.
"""],
["""\
Another Section Title
=====================
Paragraph (no blank line required).
""",
"""\
<document source="test data">
    <section ids="another-section-title" names="another\ section\ title">
        <title>
            Another Section Title
        <paragraph>
            Paragraph (no blank line required).
"""],
["""\
Paragraph.

Title
=====

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <section ids="title" names="title">
        <title>
            Title
        <paragraph>
            Paragraph.
"""],
# ["""\
# Test unexpected section titles.
# 
# * Title
#   =====
#   
#   Paragraph.
# """,
# """\
# <document source="test data">
#     <paragraph>
#         Test unexpected section titles.
#     <system_message level="3" source="test data" type="ERROR">
#         <paragraph>
#             Parsing with "recommonmark" returned the error:
#             'NoneType' object has no attribute 'note_implicit_target'
# """],
["""\
Title
==

A short underline is no problem in CommonMark
""",
"""\
<document source="test data">
    <section ids="title" names="title">
        <title>
            Title
        <paragraph>
            A short underline is no problem in CommonMark
"""],
["""\
Test return to existing, highest-level section (Title 3).

Title 1
=======
Paragraph 1.

Title 2
-------
Paragraph 2.

Title 3
=======
Paragraph 3.

Title 4
-------
Paragraph 4.
""",
"""\
<document source="test data">
    <paragraph>
        Test return to existing, highest-level section (Title 3).
    <section ids="title-1" names="title\\ 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section ids="title-2" names="title\\ 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
    <section ids="title-3" names="title\\ 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
        <section ids="title-4" names="title\\ 4">
            <title>
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
Test bad subsection order.

## Title 1

## Title 2

# Title 3

#### Title 4

### Title 5
""",
"""\
<document source="test data">
    <paragraph>
        Test bad subsection order.
    <section ids="title-1" names="title\\ 1">
        <title>
            Title 1
        <system_message level="2" source="test data" type="WARNING">
            <paragraph>
                Title level inconsistent. Changing from 2 to 1.
            <literal_block xml:space="preserve">
                Title 1
    <section ids="title-2" names="title\\ 2">
        <title>
            Title 2
        <system_message level="2" source="test data" type="WARNING">
            <paragraph>
                Title level inconsistent. Changing from 2 to 1.
            <literal_block xml:space="preserve">
                Title 2
    <section ids="title-3" names="title\\ 3">
        <title>
            Title 3
        <section ids="title-4" names="title\ 4">
            <title>
                Title 4
            <system_message level="2" source="test data" type="WARNING">
                <paragraph>
                    Title level inconsistent. Changing from 4 to 2.
                <literal_block xml:space="preserve">
                    Title 4
        <section ids="title-5" names="title\ 5">
            <title>
                Title 5
            <system_message level="2" source="test data" type="WARNING">
                <paragraph>
                    Title level inconsistent. Changing from 3 to 2.
                <literal_block xml:space="preserve">
                    Title 5
"""],
["""\
Title containing *inline* ``markup``
====================================

Paragraph.
""",
"""\
<document source="test data">
    <section ids="title-containing-inline-markup" names="title\\ containing\\ inline\\ markup">
        <title>
            Title containing \n\
            <emphasis>
                inline
             \n\
            <literal classes="code">
                markup
        <paragraph>
            Paragraph.
"""],
["""\
1. Not a numbered title but an enumerated list
==============================================

Paragraph.
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
            <paragraph>
                Not a numbered title but an enumerated list
                ==============================================
    <paragraph>
        Paragraph.
"""],
["""\
ABC
===

Short title.
""",
"""\
<document source="test data">
    <section ids="abc" names="abc">
        <title>
            ABC
        <paragraph>
            Short title.
"""],
["""\
Empty Section
=============
""",
"""\
<document source="test data">
    <section ids="empty-section" names="empty\\ section">
        <title>
            Empty Section
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
