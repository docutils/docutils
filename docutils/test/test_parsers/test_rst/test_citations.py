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

totest['citations'] = [
["""\
.. [citation] This is a citation.
""",
"""\
<document>
    <citation id="citation" name="citation">
        <label>
            citation
        <paragraph>
            This is a citation.
"""],
["""\
.. [citation1234] This is a citation with year.
""",
"""\
<document>
    <citation id="citation1234" name="citation1234">
        <label>
            citation1234
        <paragraph>
            This is a citation with year.
"""],
["""\
.. [citation] This is a citation
   on multiple lines.
""",
"""\
<document>
    <citation id="citation" name="citation">
        <label>
            citation
        <paragraph>
            This is a citation
            on multiple lines.
"""],
["""\
.. [citation1] This is a citation
     on multiple lines with more space.

.. [citation2] This is a citation
  on multiple lines with less space.
""",
"""\
<document>
    <citation id="citation1" name="citation1">
        <label>
            citation1
        <paragraph>
            This is a citation
            on multiple lines with more space.
    <citation id="citation2" name="citation2">
        <label>
            citation2
        <paragraph>
            This is a citation
            on multiple lines with less space.
"""],
["""\
.. [citation]
   This is a citation on multiple lines
   whose block starts on line 2.
""",
"""\
<document>
    <citation id="citation" name="citation">
        <label>
            citation
        <paragraph>
            This is a citation on multiple lines
            whose block starts on line 2.
"""],
["""\
.. [citation]

That was an empty citation.
""",
"""\
<document>
    <citation id="citation" name="citation">
        <label>
            citation
    <paragraph>
        That was an empty citation.
"""],
["""\
.. [citation]
No blank line.
""",
"""\
<document>
    <citation id="citation" name="citation">
        <label>
            citation
    <system_message level="2" type="WARNING">
        <paragraph>
            Unindent without blank line at line 2.
    <paragraph>
        No blank line.
"""],
["""\
.. [citation label with spaces] this isn't a citation

.. [*citationlabelwithmarkup*] this isn't a citation
""",
"""\
<document>
    <comment>
        [citation label with spaces] this isn't a citation
    <comment>
        [*citationlabelwithmarkup*] this isn't a citation
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
