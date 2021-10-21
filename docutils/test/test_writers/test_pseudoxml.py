#!/usr/bin/env python

# $Id: test_pseudoxml.py 8481 2020-01-31 08:17:24Z milde $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test for pseudo-XML writer.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_writers import DocutilsTestSupport


def suite():
    # Settings dictionary must not be empty for later changes to work.
    settings = {'expose_internals': []} # default
    s = DocutilsTestSupport.PublishTestSuite('pseudoxml',
                                             suite_settings=settings)
    s.generateTests(totest)
    settings['detailed'] = True
    s.generateTests(totest_detailed)
    return s

totest = {}
totest_detailed = {}

totest['basic'] = [
# input
[r"""
This is a paragraph.

----------

This is a paragraph 
with \escaped \characters.

A Section
---------

Foo.
""",
# output
"""\
<document source="<string>">
    <paragraph>
        This is a paragraph.
    <transition>
    <paragraph>
        This is a paragraph
        with escaped characters.
    <section ids="a-section" names="a\\ section">
        <title>
            A Section
        <paragraph>
            Foo.
"""]
]

totest_detailed['basic'] = [
# input                             
[totest['basic'][0][0],
# output 
"""\
<document source="<string>">
    <paragraph>
        <#text>
            'This is a paragraph.'
    <transition>
    <paragraph>
        <#text>
            'This is a paragraph\\n'
            'with \\x00escaped \\x00characters.'
    <section ids="a-section" names="a\\ section">
        <title>
            <#text>
                'A Section'
        <paragraph>
            <#text>
                'Foo.'
"""]
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
