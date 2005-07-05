#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
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

totest['references'] = ((FinalChecks,), [
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
["""\
Duplicate manual footnote labels, with reference ([1]_):

.. [1] Footnote.

.. [1] Footnote.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate manual footnote labels, with reference (
        <problematic ids="id5" refid="id4">
            [1]_
        ):
    <footnote dupnames="1" ids="id2">
        <label>
            1
        <paragraph>
            Footnote.
    <footnote dupnames="1" ids="id3">
        <label>
            1
        <system_message backrefs="id3" level="2" line="5" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "1".
        <paragraph>
            Footnote.
    <system_message backrefs="id5" ids="id4" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Duplicate target name, cannot be used as a unique reference: "1".
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
