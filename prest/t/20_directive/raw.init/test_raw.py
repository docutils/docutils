#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision: 1.1 $
# Date: $Date: 2006-01-26 10:53:09 -0600 (Thu, 26 Jan 2006) $
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
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
