#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@python.org
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for body.py table directives.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['tables'] = [
["""\
.. table:: Truth table for "not"
   :class: custom

   =====  =====
     A    not A
   =====  =====
   False  True
   True   False
   =====  =====
""",
"""\
<document source="test data">
    <table class="custom">
        <title>
            Truth table for "not"
        <tgroup cols="2">
            <colspec colwidth="5">
            <colspec colwidth="5">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            A
                    <entry>
                        <paragraph>
                            not A
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            False
                    <entry>
                        <paragraph>
                            True
                <row>
                    <entry>
                        <paragraph>
                            True
                    <entry>
                        <paragraph>
                            False
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
