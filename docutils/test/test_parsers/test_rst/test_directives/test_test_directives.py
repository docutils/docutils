#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for misc.py test directives.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['test_directives'] = [
["""\
.. reStructuredText-test-directive::

Paragraph.
""",
"""\
<document>
    <system_message level="1" type="INFO">
        <paragraph>
            Directive processed. Type="reStructuredText-test-directive", data="", directive block: None
    <paragraph>
        Paragraph.
"""],
["""\
.. reStructuredText-test-directive:: argument

Paragraph.
""",
"""\
<document>
    <system_message level="1" type="INFO">
        <paragraph>
            Directive processed. Type="reStructuredText-test-directive", data="argument", directive block: None
    <paragraph>
        Paragraph.
"""],
["""\
.. reStructuredText-test-directive::

   Directive block contains one paragraph, with a blank line before.

Paragraph.
""",
"""\
<document>
    <system_message level="1" type="INFO">
        <paragraph>
            Directive processed. Type="reStructuredText-test-directive", data="", directive block:
        <literal_block>
            Directive block contains one paragraph, with a blank line before.
    <paragraph>
        Paragraph.
"""],
["""\
.. reStructuredText-test-directive::
   Directive block contains one paragraph, no blank line before.

Paragraph.
""",
"""\
<document>
    <system_message level="1" type="INFO">
        <paragraph>
            Directive processed. Type="reStructuredText-test-directive", data="", directive block:
        <literal_block>
            Directive block contains one paragraph, no blank line before.
    <paragraph>
        Paragraph.
"""],
["""\
.. reStructuredText-test-directive::
   block
no blank line.

Paragraph.
""",
"""\
<document>
    <system_message level="1" type="INFO">
        <paragraph>
            Directive processed. Type="reStructuredText-test-directive", data="", directive block:
        <literal_block>
            block
    <system_message level="2" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent at line 3.
    <paragraph>
        no blank line.
    <paragraph>
        Paragraph.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
