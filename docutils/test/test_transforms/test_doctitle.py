#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for docutils.transforms.frontmatter.DocTitle.
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.frontmatter import DocTitle
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['section_headers'] = ((DocTitle,), [
["""\
.. test title promotion

Title
=====

Paragraph.
""",
"""\
<document id="title" name="title">
    <title>
        Title
    <comment>
        test title promotion
    <paragraph>
        Paragraph.
"""],
["""\
Title
=====
Paragraph (no blank line).
""",
"""\
<document id="title" name="title">
    <title>
        Title
    <paragraph>
        Paragraph (no blank line).
"""],
["""\
Paragraph.

Title
=====

Paragraph.
""",
"""\
<document>
    <paragraph>
        Paragraph.
    <section id="title" name="title">
        <title>
            Title
        <paragraph>
            Paragraph.
"""],
["""\
Title
=====

Subtitle
--------

Test title & subtitle.
""",
"""\
<document id="title" name="title">
    <title>
        Title
    <subtitle id="subtitle" name="subtitle">
        Subtitle
    <paragraph>
        Test title & subtitle.
"""],
["""\
Title
====

Test short underline.
""",
"""\
<document id="title" name="title">
    <title>
        Title
    <system_message level="1" type="INFO">
        <paragraph>
            Title underline too short at line 2.
        <literal_block>
            Title
            ====
    <paragraph>
        Test short underline.
"""],
["""\
=======
 Long    Title
=======

Test long title and space normalization.
The system_message should move after the document title
(it was before the beginning of the section).
""",
"""\
<document id="long-title" name="long title">
    <title>
        Long    Title
    <system_message level="1" type="INFO">
        <paragraph>
            Title overline too short at line 1.
        <literal_block>
            =======
             Long    Title
            =======
    <paragraph>
        Test long title and space normalization.
        The system_message should move after the document title
        (it was before the beginning of the section).
"""],
["""\
.. Test multiple second-level titles.

Title 1
=======
Paragraph 1.

Title 2
-------
Paragraph 2.

Title 3
-------
Paragraph 3.
""",
"""\
<document id="title-1" name="title 1">
    <title>
        Title 1
    <comment>
        Test multiple second-level titles.
    <paragraph>
        Paragraph 1.
    <section id="title-2" name="title 2">
        <title>
            Title 2
        <paragraph>
            Paragraph 2.
    <section id="title-3" name="title 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
"""],
])

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
