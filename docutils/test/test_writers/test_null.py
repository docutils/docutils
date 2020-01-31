#!/usr/bin/env python

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test for Null writer.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_writers import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.PublishTestSuite('null')
    s.generateTests(totest)
    return s

totest = {}

totest['basic'] = [
["""\
This is a paragraph.
""",
None]
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
