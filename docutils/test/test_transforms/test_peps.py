#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.peps.
"""

if __name__ == '__main__':
    import __init__
from test_transforms import DocutilsTestSupport
from docutils.transforms.peps import TargetNotes
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

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
    import unittest
    unittest.main(defaultTest='suite')
