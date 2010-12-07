#! /usr/bin/env python
# .. coding: utf8
# $Id$
# Author: Günter Milde <milde@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test module for the command line.
"""

import os.path
import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils.core
import docutils.utils

try:
    import locale
    argv_encoding = locale.getpreferredencoding()
except:
    argv_encoding = None

testoutput = """\
<document source="<stdin>" title="Dornröschen">
    <decoration>
        <footer>
            <paragraph>
"""

class CommandLineEncodingTests(unittest.TestCase):
    
    # This does not work, as there is no "encoding" argument!
    # def test_argv_encoding(self):
    #     if argv_encoding is None:
    #         # failure to load "locale" module
    #         return
    #     if sys.argv:
    #         self.assertEqual(sys.argv[0].encoding,
    #                          locale.getpreferredencoding())
    
    def test_argv_decoding(self):
        if argv_encoding is None:
            # failure to load "locale" module
            return # nothing to test
        cmd_str = (u'../tools/rst2pseudoxml.py --no-generator '
                   u'--no-datestamp --title=Dornröschen')
        # os.popen2 deprecated in Python 2.6 
        # but the subprocess module is new in Python 2.4
        (child_in, child_out) = os.popen2(cmd_str.encode(argv_encoding))
        child_in.close()
        output = child_out.read()
        child_out.close()
        self.assertEqual(output, testoutput)

if __name__ == '__main__':
    unittest.main()
