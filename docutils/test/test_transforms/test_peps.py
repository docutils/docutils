#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.peps.
"""

from __init__ import DocutilsTestSupport
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
        <footnote_reference auto="1" id="id3" refname="target_note: id2">
        .
        A "References" section should be generated.
    <target id="reference" name="reference" refuri="http://www.example.org">
    <section id="id1">
        <title>
            References
        <footnote auto="1" id="id2" name="target_note: id2">
            <paragraph>
                <reference refuri="http://www.example.org">
                    http://www.example.org
"""],
])



if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
