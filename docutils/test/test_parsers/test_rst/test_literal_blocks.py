#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for states.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['literal_blocks'] = [
["""\
A paragraph::

    A literal block.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
        A literal block.
"""],
["""\
A paragraph::

    A literal block.

Another paragraph::

    Another literal block.
    With two blank lines following.


A final paragraph.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
        A literal block.
    <paragraph>
        Another paragraph:
    <literal_block>
        Another literal block.
        With two blank lines following.
    <paragraph>
        A final paragraph.
"""],
["""\
A paragraph
on more than
one line::

    A literal block.
""",
"""\
<document>
    <paragraph>
        A paragraph
        on more than
        one line:
    <literal_block>
        A literal block.
"""],
["""\
A paragraph
on more than
one line::
    A literal block
    with no blank line above.
""",
"""\
<document>
    <paragraph>
        A paragraph
        on more than
        one line:
    <system_message level="3" type="ERROR">
        <paragraph>
            Unexpected indentation at line 4.
    <literal_block>
        A literal block
        with no blank line above.
"""],
["""\
A paragraph::

    A literal block.
no blank line
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
        A literal block.
    <system_message level="2" type="WARNING">
        <paragraph>
            Literal block ends without a blank line; unexpected unindent at line 4.
    <paragraph>
        no blank line
"""],
["""\
A paragraph: ::

    A literal block.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
        A literal block.
"""],
["""\
A paragraph:

::

    A literal block.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
        A literal block.
"""],
["""\
A paragraph:
::

    A literal block.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
        A literal block.
"""],
["""\
A paragraph::

Not a literal block.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <system_message level="2" type="WARNING">
        <paragraph>
            Literal block expected at line 2; none found.
    <paragraph>
        Not a literal block.
"""],
["""\
A paragraph::

    A wonky literal block.
  Literal line 2.

    Literal line 3.
""",
"""\
<document>
    <paragraph>
        A paragraph:
    <literal_block>
          A wonky literal block.
        Literal line 2.
        \n\
          Literal line 3.
"""],
["""\
EOF, even though a literal block is indicated::
""",
"""\
<document>
    <paragraph>
        EOF, even though a literal block is indicated:
    <system_message level="2" type="WARNING">
        <paragraph>
            Literal block expected at line 2; none found.
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
