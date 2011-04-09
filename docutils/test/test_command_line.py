#! /usr/bin/env python
# .. coding: utf8
# $Id$
# Author: Günter Milde <milde@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test module for the command line.
"""

from os.path import normpath, dirname
import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils.core
import docutils.utils

try:
    import subprocess  # new in 2.4
except ImportError:
    argv_encoding = None
try:
    import locale
    argv_encoding = locale.getpreferredencoding()
except:
    argv_encoding = None

testoutput = u'<document source="<stdin>" title="Dornröschen">'

class CommandLineEncodingTests(unittest.TestCase):

    def test_argv_decoding(self):
        if argv_encoding is None:
            # failure to load "locale" or "subprocess" module
            return # nothing to test

        # This does not work, as there is no "encoding" argument!
        # if sys.argv:
        #     self.assertEqual(sys.argv[0].encoding,
        #                      locale.getpreferredencoding())
        # so instead, we check if a command line with non-ASCII char works
        cmd_str = (u'%s %s/mini_frontend.py --title=Dornröschen' %
                   (sys.executable, normpath(dirname(__file__) + '')))
        p = subprocess.Popen([cmd_str.encode(argv_encoding)], shell=True,
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        p.stdin.close()
        output = p.stdout.readlines()
        p.stdout.close()
        self.assertEqual(output[0].decode('utf8').strip(), testoutput)


if __name__ == '__main__':
    unittest.main()
