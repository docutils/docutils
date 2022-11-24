#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for `docutils.transforms.references.TargetNotes` (via
`docutils.transforms.universal.LastReaderPending`).
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.references import (PropagateTargets, AnonymousHyperlinks,
                                            IndirectHyperlinks, ExternalTargets,
                                            InternalTargets, DanglingReferences)
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

totest['tables_of_contents'] = ((PropagateTargets, AnonymousHyperlinks,
                                 IndirectHyperlinks,
                                 ExternalTargets, InternalTargets,
                                 DanglingReferences,
    ), [
["""\
.. _target: http://example.org

A reference to a target_.

.. target-notes::
""",
"""\
<document source="test data">
    <target ids="target" names="target" refuri="http://example.org">
    <paragraph>
        A reference to a \n\
        <reference name="target" refuri="http://example.org">
            target
         \n\
        <footnote_reference auto="1" ids="footnote-reference-1" refid="footnote-1">
        .
    <footnote auto="1" ids="footnote-1" names="TARGET_NOTE:\\ footnote-1">
        <paragraph>
            <reference refuri="http://example.org">
                http://example.org
"""],
["""\
.. _target: http://example.org

A reference to a target_.

.. target-notes:: :class: custom
""",
"""\
<document source="test data">
    <target ids="target" names="target" refuri="http://example.org">
    <paragraph>
        A reference to a \n\
        <reference name="target" refuri="http://example.org">
            target
        <inline classes="custom">
             \n\
        <footnote_reference auto="1" classes="custom" ids="footnote-reference-1" refid="footnote-1">
        .
    <footnote auto="1" ids="footnote-1" names="TARGET_NOTE:\\ footnote-1">
        <paragraph>
            <reference refuri="http://example.org">
                http://example.org
"""],
])


if __name__ == '__main__':
    unittest.main()
