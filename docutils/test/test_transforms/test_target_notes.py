#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for `docutils.transforms.references.TargetNotes` (via
`docutils.transforms.universal.LastReaderPending`).
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test_transforms import DocutilsTestSupport
from docutils.transforms.references import (PropagateTargets, AnonymousHyperlinks,
                                            IndirectHyperlinks, ExternalTargets,
                                            InternalTargets, DanglingReferences)
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s


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
    import unittest
    unittest.main(defaultTest='suite')
