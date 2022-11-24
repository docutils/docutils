#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for `docutils.transforms.misc.ClassAttribute`.
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
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
                    self.assertEqual(output, case_expected)


totest = {}

totest['class'] = ((), [
["""\
.. class:: one

paragraph
""",
"""\
<document source="test data">
    <paragraph classes="one">
        paragraph
"""],
["""\
.. class:: two
..

    Block quote
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
    <block_quote classes="two">
        <paragraph>
            Block quote
"""],
["""\
    Block quote

    .. class:: three

Paragraph
""",
"""\
<document source="test data">
    <block_quote>
        <paragraph>
            Block quote
    <paragraph classes="three">
        Paragraph
"""],
["""\
.. class:: four

Section Title
=============

Paragraph
""",
"""\
<document source="test data">
    <section classes="four" ids="section-title" names="section\\ title">
        <title>
            Section Title
        <paragraph>
            Paragraph
"""],
["""\
.. class:: multiple

   paragraph 1

   paragraph 2
""",
"""\
<document source="test data">
    <paragraph classes="multiple">
        paragraph 1
    <paragraph classes="multiple">
        paragraph 2
"""],
["""\
.. class:: multiple

   .. Just a comment.  It's silly, but possible
""",
"""\
<document source="test data">
    <comment classes="multiple" xml:space="preserve">
        Just a comment.  It's silly, but possible
"""],
["""\
.. class::

.. class:: 99
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "class" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            .. class::
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Invalid class attribute value for "class" directive: "99".
        <literal_block xml:space="preserve">
            .. class:: 99
"""],
["""\
.. class:: one
.. class:: two

multiple class values may be assigned to one element
""",
"""\
<document source="test data">
    <paragraph classes="one two">
        multiple class values may be assigned to one element
"""],
["""\
.. class:: one two

multiple class values may be assigned to one element
""",
"""\
<document source="test data">
    <paragraph classes="one two">
        multiple class values may be assigned to one element
"""],
["""\
.. class:: fancy

2. List starts at 2.
3. Class should apply to list, not to system message.
""",
"""\
<document source="test data">
    <enumerated_list classes="fancy" enumtype="arabic" prefix="" start="2" suffix=".">
        <list_item>
            <paragraph>
                List starts at 2.
        <list_item>
            <paragraph>
                Class should apply to list, not to system message.
    <system_message level="1" line="3" source="test data" type="INFO">
        <paragraph>
            Enumerated list start value not ordinal-1: "2" (ordinal 2)
"""],
["""\
2. List starts at 2.
3. Class should apply to next paragraph, not to system message.

   .. class:: fancy

A paragraph.
""",
"""\
<document source="test data">
    <enumerated_list enumtype="arabic" prefix="" start="2" suffix=".">
        <list_item>
            <paragraph>
                List starts at 2.
        <list_item>
            <paragraph>
                Class should apply to next paragraph, not to system message.
    <system_message level="1" line="1" source="test data" type="INFO">
        <paragraph>
            Enumerated list start value not ordinal-1: "2" (ordinal 2)
    <paragraph classes="fancy">
        A paragraph.
"""],
])


if __name__ == '__main__':
    unittest.main()
