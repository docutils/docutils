#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for the "topic" directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['topics'] = [
["""\
.. topic::
""",
"""\
<document source="test data">
"""],
["""\
.. topic:: Title
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
"""],
["""\
.. topic:: Title

   Body.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <paragraph>
            Body.
"""],
["""\
.. topic::

   Title

   Body.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <paragraph>
            Body.
"""],
["""\
.. topic:: Title
   Body.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <system_message level="2" source="test data" type="WARNING">
            <paragraph>
                The second line of a topic block must be blank (line 2).
        <paragraph>
            Body.
"""],
["""\
.. topic::

   Title
   Body.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <system_message level="2" source="test data" type="WARNING">
            <paragraph>
                The second line of a topic block must be blank (line 4).
        <paragraph>
            Body.
"""],
["""\
.. topic:: Title

   .. topic:: Nested

      Body.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <system_message level="3" source="test data" type="ERROR">
            <paragraph>
                Topics may not be nested within body elements (line 2).
            <literal_block xml:space="1">
                .. topic:: Nested
                \n\
                   Body.
"""],
["""\
.. topic:: Title

   .. topic:: Nested

      Body.
   More.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <system_message level="3" source="test data" type="ERROR">
            <paragraph>
                Topics may not be nested within body elements (line 2).
            <literal_block xml:space="1">
                .. topic:: Nested
                \n\
                   Body.
        <system_message level="2" source="test data" type="WARNING">
            <paragraph>
                Explicit markup ends without a blank line; unexpected unindent at line 5.
        <paragraph>
            More.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
