#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for states.py.
"""

import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['substitution_definitions'] = [
["""\
Here's an image substitution definition:

.. |symbol| image:: symbol.png
""",
"""\
<document>
    <paragraph>
        Here's an image substitution definition:
    <substitution_definition name="symbol">
        <image alt="symbol" uri="symbol.png">
"""],
["""\
Embedded directive starts on the next line:

.. |symbol|
   image:: symbol.png
""",
"""\
<document>
    <paragraph>
        Embedded directive starts on the next line:
    <substitution_definition name="symbol">
        <image alt="symbol" uri="symbol.png">
"""],
["""\
Here's a series of substitution definitions:

.. |symbol 1| image:: symbol1.png
.. |SYMBOL 2| image:: symbol2.png
   :height: 50
   :width: 100
.. |symbol 3| image:: symbol3.png
""",
"""\
<document>
    <paragraph>
        Here's a series of substitution definitions:
    <substitution_definition name="symbol 1">
        <image alt="symbol 1" uri="symbol1.png">
    <substitution_definition name="symbol 2">
        <image alt="SYMBOL 2" height="50" uri="symbol2.png" width="100">
    <substitution_definition name="symbol 3">
        <image alt="symbol 3" uri="symbol3.png">
"""],
["""\
.. |very long substitution text,
   split across lines| image:: symbol.png
""",
"""\
<document>
    <substitution_definition name="very long substitution text, split across lines">
        <image alt="very long substitution text, split across lines" uri="symbol.png">
"""],
["""\
.. |symbol 1| image:: symbol.png

Followed by a paragraph.

.. |symbol 2| image:: symbol.png

    Followed by a block quote.
""",
"""\
<document>
    <substitution_definition name="symbol 1">
        <image alt="symbol 1" uri="symbol.png">
    <paragraph>
        Followed by a paragraph.
    <substitution_definition name="symbol 2">
        <image alt="symbol 2" uri="symbol.png">
    <block_quote>
        <paragraph>
            Followed by a block quote.
"""],
["""\
Here are some duplicate substitution definitions:

.. |symbol| image:: symbol.png
.. |symbol| image:: symbol.png
""",
"""\
<document>
    <paragraph>
        Here are some duplicate substitution definitions:
    <substitution_definition dupname="symbol">
        <image alt="symbol" uri="symbol.png">
    <system_message level="3" type="ERROR">
        <paragraph>
            Duplicate substitution definition name: "symbol".
    <substitution_definition name="symbol">
        <image alt="symbol" uri="symbol.png">
"""],
["""\
Here are some bad cases:

.. |symbol| image:: symbol.png
No blank line after.

.. |empty|

.. |unknown| directive:: symbol.png

.. |invalid 1| there's no directive here
.. |invalid 2| there's no directive here
   With some block quote text, line 1.
   And some more, line 2.

.. |invalid 3| there's no directive here

.. | bad name | bad data
""",
"""\
<document>
    <paragraph>
        Here are some bad cases:
    <substitution_definition name="symbol">
        <image alt="symbol" uri="symbol.png">
    <system_message level="2" type="WARNING">
        <paragraph>
            Unindent without blank line at line 4.
    <paragraph>
        No blank line after.
    <system_message level="2" type="WARNING">
        <paragraph>
            Substitution definition "empty" missing contents at line 6.
        <literal_block>
            .. |empty|
    <system_message level="3" type="ERROR">
        <paragraph>
            Unknown directive type "directive" at line 8.
        <literal_block>
            directive:: symbol.png
    <system_message level="2" type="WARNING">
        <paragraph>
            Substitution definition "unknown" empty or invalid at line 8.
        <literal_block>
            .. |unknown| directive:: symbol.png
    <system_message level="2" type="WARNING">
        <paragraph>
            Substitution definition "invalid 1" empty or invalid at line 10.
        <literal_block>
            .. |invalid 1| there's no directive here
    <system_message level="2" type="WARNING">
        <paragraph>
            Substitution definition "invalid 2" empty or invalid at line 11.
        <literal_block>
            .. |invalid 2| there's no directive here
               With some block quote text, line 1.
               And some more, line 2.
    <system_message level="2" type="WARNING">
        <paragraph>
            Unindent without blank line at line 12.
    <block_quote>
        <paragraph>
            With some block quote text, line 1.
            And some more, line 2.
    <system_message level="2" type="WARNING">
        <paragraph>
            Substitution definition "invalid 3" empty or invalid at line 15.
        <literal_block>
            .. |invalid 3| there's no directive here
    <comment>
        | bad name | bad data
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
