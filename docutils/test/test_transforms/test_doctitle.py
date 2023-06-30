#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.frontmatter.DocTitle.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser, Directive
from docutils.parsers.rst.directives import register_directive
from docutils.transforms.frontmatter import DocTitle, SectionSubTitle
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


# dummy directive to test attribute merging:
class AddNameToDocumentTitle(Directive):
    required_arguments = 0
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}
    has_content = False

    def run(self):
        document = self.state_machine.document
        document['names'].append('Name')
        return []


class TransformTestCase(unittest.TestCase):

    maxDiff = None

    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        register_directive('add-name-to-title', AddNameToDocumentTitle)
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

totest['section_headers'] = ((DocTitle, SectionSubTitle), [
["""\
.. test title promotion

Title
=====

Paragraph.
""",
"""\
<document ids="title" names="title" source="test data" title="Title">
    <title>
        Title
    <comment xml:space="preserve">
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
<document ids="title" names="title" source="test data" title="Title">
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
<document source="test data">
    <paragraph>
        Paragraph.
    <section ids="title" names="title">
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

.. title:: Another Title

Test title, subtitle, and title metadata.
""",
"""\
<document ids="title" names="title" source="test data" title="Another Title">
    <title>
        Title
    <subtitle ids="subtitle" names="subtitle">
        Subtitle
    <paragraph>
        Test title, subtitle, and title metadata.
"""],
["""\
Title
====

Test short underline.
""",
"""\
<document ids="title" names="title" source="test data" title="Title">
    <title>
        Title
    <system_message level="2" line="2" source="test data" type="WARNING">
        <paragraph>
            Title underline too short.
        <literal_block xml:space="preserve">
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
<document ids="long-title" names="long\\ title" source="test data" title="Long    Title">
    <title>
        Long    Title
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Title overline too short.
        <literal_block xml:space="preserve">
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
<document ids="title-1" names="title\\ 1" source="test data" title="Title 1">
    <title>
        Title 1
    <comment xml:space="preserve">
        Test multiple second-level titles.
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
"""],
["""\
.. |foo| replace:: bar

.. _invisible target:

Title
=====
This title should be the document title despite the
substitution_definition.
""",
"""\
<document ids="title" names="title" source="test data" title="Title">
    <title>
        Title
    <substitution_definition names="foo">
        bar
    <target ids="invisible-target" names="invisible\\ target">
    <paragraph>
        This title should be the document title despite the
        substitution_definition.
"""],
["""\
(Because of this paragraph, the following is not a doc title.)

===============
 Section Title
===============

Subtitle
========

-----------------
 Another Section
-----------------

Another Subtitle
----------------

""",
"""\
<document source="test data">
    <paragraph>
        (Because of this paragraph, the following is not a doc title.)
    <section ids="section-title" names="section\\ title">
        <title>
            Section Title
        <subtitle ids="subtitle" names="subtitle">
            Subtitle
        <section ids="another-section" names="another\\ section">
            <title>
                Another Section
            <subtitle ids="another-subtitle" names="another\\ subtitle">
                Another Subtitle
"""],
["""\
-----
Title
-----

This is a document, it flows nicely, so the attributes of it are at the
bottom.

.. add-name-to-title::

""",
"""\
<document ids="title" names="Name title" source="test data" title="Title">
    <title>
        Title
    <paragraph>
        This is a document, it flows nicely, so the attributes of it are at the
        bottom.
"""]
])


if __name__ == '__main__':
    unittest.main()
