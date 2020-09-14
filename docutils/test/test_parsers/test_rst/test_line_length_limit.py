#! /usr/bin/env python
# -*- coding: utf-8 -*-

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for inline markup in docutils/parsers/rst/states.py.
Interpreted text tests are in a separate module, test_interpreted.py.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite(
            suite_settings={'line_length_limit': 80})
    s.generateTests(totest)
    return s

totest = {}

totest['default'] = [
["""\
within the limit
%s
""" % ("x"*80),
"""\
<document source="test data">
    <paragraph>
        within the limit
        %s
""" % ("x"*80)],
["""\
above the limit
%s
""" % ("x"*81),
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Line 2 exceeds the line-length-limit.
"""],
["""\
Include Test
============

.. include:: docutils.conf
   :literal:

A paragraph.
""",
"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <system_message level="2" line="4" source="test data" type="WARNING">
            <paragraph>
                "docutils.conf": line 5 exceeds the line-length-limit.
            <literal_block xml:space="preserve">
                .. include:: docutils.conf
                   :literal:
        <paragraph>
            A paragraph.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
