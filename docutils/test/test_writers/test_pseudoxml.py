#!/usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test for pseudo-XML writer.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.PublishTestSuite('pseudoxml')
    s.generateTests(totest)
    return s

totest = {}

totest['basic'] = [
# input
["""\
This is the title
=================

This is a paragraph.

----------

This is another paragraph.

A subsection
------------

Foo.
""",
# output
"""\
<document id="this-is-the-title" name="this is the title" source="<string>">
    <title>
        This is the title
    <paragraph>
        This is a paragraph.
    <transition>
    <paragraph>
        This is another paragraph.
    <section id="a-subsection" name="a subsection">
        <title>
            A subsection
        <paragraph>
            Foo.
"""]
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
