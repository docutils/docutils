#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision: 1.1 $
# Date: $Date: 2006-01-26 10:53:09 -0600 (Thu, 26 Jan 2006) $
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.components.Filter.
"""

from __init__ import DocutilsTestSupport
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['meta'] = ((), [
["""\
.. meta::
   :description: The reStructuredText plaintext markup language
   :keywords: plaintext,markup language
""",
"""\
<document source="test data">
    <meta content="The reStructuredText plaintext markup language" name="description">
    <meta content="plaintext,markup language" name="keywords">
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
