#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision: 1.1 $
# Date: $Date: 2005-10-06 10:35:16 -0500 (Thu, 06 Oct 2005) $
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.universal.FinalChecks.
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.universal import FinalChecks
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['final_checks'] = ((FinalChecks,), [
["""\
Unknown reference_.
""",
"""\
<document source="test data">
    <paragraph>
        Unknown \n\
        <problematic ids="id2" refid="id1">
            reference_
        .
    <system_message backrefs="id2" ids="id1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Unknown target name: "reference".
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
