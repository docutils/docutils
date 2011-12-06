#! /usr/bin/env python

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for io.py.
"""

import unittest, sys
import DocutilsTestSupport              # must be imported before docutils
from docutils import io
from docutils._compat import b, bytes
from docutils.error_reporting import locale_encoding

class InputTests(unittest.TestCase):

    def test_bom(self):
        input = io.StringInput(source=b('\xef\xbb\xbf foo \xef\xbb\xbf bar'),
                               encoding='utf8')
        # Assert BOMs are gone.
        self.assertEqual(input.read(), u' foo  bar')
        # With unicode input:
        input = io.StringInput(source=u'\ufeff foo \ufeff bar')
        # Assert BOMs are still there.
        self.assertEqual(input.read(), u'\ufeff foo \ufeff bar')

    def test_coding_slug(self):
        input = io.StringInput(source=b("""\
.. -*- coding: ascii -*-
data
blah
"""))
        data = input.read()
        self.assertEqual(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b("""\
#! python
# -*- coding: ascii -*-
print "hello world"
"""))
        data = input.read()
        self.assertEqual(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b("""\
#! python
# extraneous comment; prevents coding slug from being read
# -*- coding: ascii -*-
print "hello world"
"""))
        data = input.read()
        self.assertNotEqual(input.successful_encoding, 'ascii')

    def test_bom_detection(self):
        source = u'\ufeffdata\nblah\n'
        input = io.StringInput(source=source.encode('utf-16-be'))
        data = input.read()
        self.assertEqual(input.successful_encoding, 'utf-16-be')
        input = io.StringInput(source=source.encode('utf-16-le'))
        data = input.read()
        self.assertEqual(input.successful_encoding, 'utf-16-le')
        input = io.StringInput(source=source.encode('utf-8'))
        data = input.read()
        self.assertEqual(input.successful_encoding, 'utf-8')

    def test_readlines(self):
        input = io.FileInput(source_path='data/include.txt')
        data = input.readlines()
        self.assertEqual(data, [u'Some include text.\n'])

    def test_heuristics_utf8(self):
        # if no encoding is given, try decoding with utf8:
        input = io.FileInput(source_path='functional/input/cyrillic.txt')
        data = input.read()
        if sys.version_info < (3,0): 
            # in Py3k, the locale encoding is used without --input-encoding
            # skipping the heuristic
            self.assertEqual(input.successful_encoding, 'utf-8')

    def test_heuristics_no_utf8(self):
        # if no encoding is given and decoding with utf8 fails, 
        # use either the locale encoding (if specified) or latin1:
        input = io.FileInput(source_path='data/latin1.txt')
        data = input.read()
        self.assertTrue(input.successful_encoding in (locale_encoding,
                                                      'latin-1'))
        if input.successful_encoding == 'latin-1':
            self.assertEqual(data, u'Gr\xfc\xdfe\n')


if __name__ == '__main__':
    unittest.main()
