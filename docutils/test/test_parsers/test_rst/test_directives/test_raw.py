#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "raw" directive.
"""

import os.path
from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

mydir = os.path.dirname(suite.func_code.co_filename)
raw1 = os.path.join(mydir, 'raw1.txt')

totest = {}

totest['raw'] = [
["""\
.. raw:: html

   <span>This is some plain old raw text.</span>
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        <span>This is some plain old raw text.</span>
"""],
["""\
.. raw:: html
   :file: %s
""" % raw1,
"""\
<document source="test data">
    <raw format="html" source="%s" xml:space="preserve">
        <p>This file is used by <tt>test_raw.py</tt>.</p>
""" % DocutilsTestSupport.utils.relative_path(None, raw1)],
["""\
.. raw:: html
   :file: rawfile.html
   :url: http://example.org/
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            The "file" and "url" options may not be simultaneously specified for the "raw" directive.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: rawfile.html
               :url: http://example.org/
"""],
["""\
.. raw:: html
   :file: rawfile.html

   <p>Can't have both content and file attribute.</p>
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            "raw" directive may not both specify an external file and have content.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: rawfile.html
            
               <p>Can't have both content and file attribute.</p>
"""],
[r"""
.. raw:: latex html
 
   \[ \sum_{n=1}^\infty \frac{1}{n} \text{ etc.} \]
""",
"""\
<document source="test data">
    <raw format="latex html" xml:space="preserve">
        \\[ \\sum_{n=1}^\\infty \\frac{1}{n} \\text{ etc.} \\]
"""],
["""\
.. raw:: html
   :file: utf-16.csv
   :encoding: utf-16
""",
"""\
<document source="test data">
    <raw format="html" source="utf-16.csv" xml:space="preserve">
        "Treat", "Quantity", "Description"
        "Albatr\xb0\xdf", 2.99, "\xa1On a \\u03c3\\u03c4\\u03b9\\u03ba!"
        "Crunchy Frog", 1.49, "If we took the b\xf6nes out, it wouldn\\u2019t be
        crunchy, now would it?"
        "Gannet Ripple", 1.99, "\xbfOn a \\u03c3\\u03c4\\u03b9\\u03ba?"
"""],
["""\
UTF-16 is no valid ASCII, but the parser doesn't complain.  BUG?

.. raw:: html
   :file: utf-16.csv
   :encoding: ascii
""",
"""\
<document source="test data">
    <paragraph>
        UTF-16 is no valid ASCII, but the parser doesn't complain.  BUG?
    <raw format="html" source="utf-16.csv" xml:space="preserve">
        \xfe\xff\x00"\x00T\x00r\x00e\x00a\x00t\x00"\x00,\x00 \x00"\x00Q\x00u\x00a\x00n\x00t\x00i\x00t\x00y\x00"\x00,\x00 \x00"\x00D\x00e\x00s\x00c\x00r\x00i\x00p\x00t\x00i\x00o\x00n\x00"\x00
        \x00"\x00A\x00l\x00b\x00a\x00t\x00r\x00\xb0\x00\xdf\x00"\x00,\x00 \x002\x00.\x009\x009\x00,\x00 \x00"\x00\xa1\x00O\x00n\x00 \x00a\x00 \x03\xc3\x03\xc4\x03\xb9\x03\xba\x00!\x00"\x00
        \x00"\x00C\x00r\x00u\x00n\x00c\x00h\x00y\x00 \x00F\x00r\x00o\x00g\x00"\x00,\x00 \x001\x00.\x004\x009\x00,\x00 \x00"\x00I\x00f\x00 \x00w\x00e\x00 \x00t\x00o\x00o\x00k\x00 \x00t\x00h\x00e\x00 \x00b\x00\xf6\x00n\x00e\x00s\x00 \x00o\x00u\x00t\x00,\x00 \x00i\x00t\x00 \x00w\x00o\x00u\x00l\x00d\x00n \x19\x00t\x00 \x00b\x00e\x00
        \x00c\x00r\x00u\x00n\x00c\x00h\x00y\x00,\x00 \x00n\x00o\x00w\x00 \x00w\x00o\x00u\x00l\x00d\x00 \x00i\x00t\x00?\x00"\x00
        \x00"\x00G\x00a\x00n\x00n\x00e\x00t\x00 \x00R\x00i\x00p\x00p\x00l\x00e\x00"\x00,\x00 \x001\x00.\x009\x009\x00,\x00 \x00"\x00\xbf\x00O\x00n\x00 \x00a\x00 \x03\xc3\x03\xc4\x03\xb9\x03\xba\x00?\x00"\x00
"""],
["""\
.. raw:: html
   :encoding: utf-8

   Should the parser complain becau\xdfe there is no :file:?  BUG?
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        Should the parser complain becau\xdfe there is no :file:?  BUG?
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
