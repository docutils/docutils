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
        <system_message level="2" line="2" source="test data" type="WARNING">
            <paragraph>
                The second line of a topic block must be blank.
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
        <system_message level="2" line="4" source="test data" type="WARNING">
            <paragraph>
                The second line of a topic block must be blank.
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
        <system_message level="3" line="3" source="test data" type="ERROR">
            <paragraph>
                Topics may not be nested within body elements.
            <literal_block xml:space="preserve">
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
        <system_message level="3" line="3" source="test data" type="ERROR">
            <paragraph>
                Topics may not be nested within body elements.
            <literal_block xml:space="preserve">
                .. topic:: Nested
                \n\
                   Body.
        <system_message level="2" line="6" source="test data" type="WARNING">
            <paragraph>
                Explicit markup ends without a blank line; unexpected unindent.
        <paragraph>
            More.
"""],
["""\
.. topic:: Title

   .. topic::

      Nested

      Body.

   More.

More.
""",
"""\
<document source="test data">
    <topic>
        <title>
            Title
        <system_message level="3" line="3" source="test data" type="ERROR">
            <paragraph>
                Topics may not be nested within body elements.
            <literal_block xml:space="preserve">
                .. topic::
                \n\
                   Nested
                \n\
                   Body.
        <paragraph>
            More.
    <paragraph>
        More.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
