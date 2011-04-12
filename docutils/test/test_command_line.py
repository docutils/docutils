#! /usr/bin/env python
# .. coding: utf8
# $Id$
# Author: Günter Milde <milde@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test module for the command line.
"""

import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils.core

try:
    import subprocess  # new in 2.4
except ImportError:
    argv_encoding = None
try:
    import locale
    argv_encoding = locale.getpreferredencoding()
except:
    argv_encoding = None

class CommandLineEncodingTests(unittest.TestCase):
    
    def test_sys_argv_decoding(self):
        if argv_encoding is None: # cannot test
            return
        sys.argv.append(u'--title=Dornröschen'.encode(argv_encoding))
        publisher = docutils.core.Publisher()
        publisher.process_command_line()
        self.assertEqual(publisher.settings.title, u'Dornröschen')
        sys.argv.pop()

if __name__ == '__main__':
    unittest.main()
