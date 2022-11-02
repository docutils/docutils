#!/usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test for Null writer.
"""

from test import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.PublishTestSuite(
        'null', suite_settings={'output_encoding': 'utf-8'})
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
