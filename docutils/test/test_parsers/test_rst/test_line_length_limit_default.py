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
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['default'] = [
["""\
within the limit
%s
""" % ("x"*10000),
"""\
<document source="test data">
    <paragraph>
        within the limit
        %s
""" % ("x"*10000)],
["""\
above the limit
%s
""" % ("x"*10001),
"""\
<document source="test data">
    <system_message level="3" source="test data" type="ERROR">
        <paragraph>
            Line 2 exceeds the line-length-limit.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
