#! /usr/bin/env python3
# $Id$
# Author: Günter Milde <milde@users.sf.net>
# Copyright: This module has been placed in the public domain.

"""
Test module for the command line.
"""

import codecs
import locale
import sys
import unittest

import DocutilsTestSupport  # must be imported before docutils
import docutils.core

# determine/guess the encoding of the standard input:
try:
    locale_encoding = locale.getlocale()[1] or locale.getdefaultlocale()[1]
except ImportError:
    locale_encoding = None

argv_encoding = locale_encoding or 'ascii'
try:
    codecs.lookup(argv_encoding)
except LookupError:
    argv_encoding = 'ascii'


class CommandLineEncodingTests(unittest.TestCase):

    def test_sys_argv_decoding(self):
        if argv_encoding == 'ascii':  # cannot test
            return
        sys.argv.append('--source-url=test.txt')  # pure ASCII argument
        sys.argv.append('--title=Dornröschen')
        publisher = docutils.core.Publisher()
        publisher.process_command_line()
        self.assertEqual(publisher.settings.source_url, 'test.txt')
        self.assertEqual(publisher.settings.title, 'Dornröschen')
        sys.argv.pop()  # --title
        sys.argv.pop()  # --source-url


if __name__ == '__main__':
    unittest.main()
