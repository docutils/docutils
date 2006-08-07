#! /usr/bin/env python

# $Id$
# Author: Felix Wiemann <Felix.Wiemann@ososo.de>
# Copyright: This module has been placed in the public domain.

"""
Tests for the 'title' directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['title'] = [
["""\
.. title:: This is the document title.
""",
"""\
<document source="test data" title="This is the document title.">
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
