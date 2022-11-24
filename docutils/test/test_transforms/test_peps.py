#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.peps.
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.peps import TargetNotes
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

totest['target_notes'] = ((TargetNotes,), [
["""\
No references or targets exist, therefore
no "References" section should be generated.
""",
"""\
<document source="test data">
    <paragraph>
        No references or targets exist, therefore
        no "References" section should be generated.
"""],
["""\
A target exists, here's the reference_.
A "References" section should be generated.

.. _reference: http://www.example.org
""",
"""\
<document source="test data">
    <paragraph>
        A target exists, here's the \n\
        <reference name="reference" refname="reference">
            reference
         \n\
        <footnote_reference auto="1" ids="footnote-reference-1" refname="TARGET_NOTE: footnote-1">
        .
        A "References" section should be generated.
    <target ids="reference" names="reference" refuri="http://www.example.org">
    <section ids="section-1">
        <title>
            References
        <footnote auto="1" ids="footnote-1" names="TARGET_NOTE:\\ footnote-1">
            <paragraph>
                <reference refuri="http://www.example.org">
                    http://www.example.org
"""],
])


if __name__ == '__main__':
    unittest.main()
