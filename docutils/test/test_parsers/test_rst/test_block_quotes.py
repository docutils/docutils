#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['block_quotes'] = [
["""\
Line 1.
Line 2.

   Indented.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Indented.
"""],
["""\
Line 1.
Line 2.

   Indented 1.

      Indented 2.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Indented 1.
        <block_quote>
            <paragraph>
                Indented 2.
"""],
["""\
Line 1.
Line 2.
    Unexpectedly indented.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Unexpected indentation.
    <block_quote>
        <paragraph>
            Unexpectedly indented.
"""],
["""\
Line 1.
Line 2.

   Indented.
no blank line
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Indented.
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Block quote ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
"""],
["""\
Here is a paragraph.

        Indent 8 spaces.

    Indent 4 spaces.

Is this correct? Should it generate a warning?
Yes, it is correct, no warning necessary.
""",
"""\
<document source="test data">
    <paragraph>
        Here is a paragraph.
    <block_quote>
        <block_quote>
            <paragraph>
                Indent 8 spaces.
        <paragraph>
            Indent 4 spaces.
    <paragraph>
        Is this correct? Should it generate a warning?
        Yes, it is correct, no warning necessary.
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
