#!/usr/bin/env python

# $Id$
# Author: Felix Wiemann <Felix.Wiemann@ososo.de>
# Copyright: This module has been placed in the public domain.

"""
Test for Null writer.
"""

from __init__ import DocutilsTestSupport

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
