#! /usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test module for universal.ExposeInternals transform.
"""


from docutils.transforms.universal import ExposeInternals
from __init__ import DocutilsTestSupport
from docutils.parsers.rst import Parser

def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s


totest = {}

totest['transitions'] = ((ExposeInternals,), [
["""\
This is a test.
""",
"""\
[Test disabled at the moment.  How do we activate the expose_internals
setting for this test suite?]
""",
0],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
