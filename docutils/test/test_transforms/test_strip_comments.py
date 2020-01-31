#! /usr/bin/env python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.universal.StripComments.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_transforms import DocutilsTestSupport
from docutils.transforms.universal import StripComments
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(
        parser, suite_settings={'strip_comments': 1})
    s.generateTests(totest)
    return s

totest = {}

totest['strip_comments'] = ((StripComments,), [
["""\
.. this is a comment

Title
=====

Paragraph.

.. second comment
""",
"""\
<document source="test data">
    <section ids="title" names="title">
        <title>
            Title
        <paragraph>
            Paragraph.
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
