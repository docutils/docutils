#! /usr/bin/env python

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for io.py.
"""

import unittest, sys
try:
    from io import StringIO, BytesIO
except ImportError:    # io is new in Python 2.6
    from StringIO import StringIO
    BytesIO = StringIO

import DocutilsTestSupport              # must be imported before docutils
from docutils import io
from docutils._compat import b


class InputTests(unittest.TestCase):

    def test_bom(self):
        input = io.StringInput(source=b('\xef\xbb\xbf foo \xef\xbb\xbf bar'),
                               encoding='utf8')
        # Assert BOMs are gone.
        self.assertEquals(input.read(), u' foo  bar')
        # With unicode input:
        input = io.StringInput(source=u'\ufeff foo \ufeff bar')
        # Assert BOMs are still there.
        self.assertEquals(input.read(), u'\ufeff foo \ufeff bar')

    def test_coding_slug(self):
        input = io.StringInput(source=b("""\
.. -*- coding: ascii -*-
data
blah
"""))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b("""\
#! python
# -*- coding: ascii -*-
print "hello world"
"""))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b("""\
#! python
# extraneous comment; prevents coding slug from being read
# -*- coding: ascii -*-
print "hello world"
"""))
        data = input.read()
        self.assertNotEquals(input.successful_encoding, 'ascii')

    def test_bom_detection(self):
        source = u'\ufeffdata\nblah\n'
        input = io.StringInput(source=source.encode('utf-16-be'))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'utf-16-be')
        input = io.StringInput(source=source.encode('utf-16-le'))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'utf-16-le')
        input = io.StringInput(source=source.encode('utf-8'))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'utf-8')


# ErrorOutput tests
# -----------------

# Stub: Buffer with 'strict' auto-conversion of input to byte string:
class BBuf(BytesIO, object):
    def write(self, data):
        if type(data) == unicode:
            data.encode('ascii', 'strict')
        super(BBuf, self).write(data)

# Stub: Buffer expecting unicode string:
class UBuf(StringIO, object):
    def write(self, data):
        # emulate Python 3 handling of stdout, stderr
        if type(data) == b:
            raise TypeError('must be unicode, not bytes')
        super(UBuf, self).write(data)

class ErrorOutputTests(unittest.TestCase):
    def test_defaults(self):
        e = io.ErrorOutput()
        self.assertEquals(e.stream, sys.stderr)

    def test_bbuf(self):
        buf = BBuf() # buffer storing byte string
        e = io.ErrorOutput(buf, encoding='ascii')
        # write byte-string as-is
        e.write(b('b\xfc'))
        self.assertEquals(buf.getvalue(), b('b\xfc'))
        # encode unicode data with backslashescape fallback replacement:
        e.write(u' u\xfc')
        self.assertEquals(buf.getvalue(), b('b\xfc u\\xfc'))
        # handle Exceptions with Unicode string args
        # unicode(Exception(u'e\xfc')) # fails in Python < 2.6
        e.write(AttributeError(u' e\xfc'))
        self.assertEquals(buf.getvalue(), b('b\xfc u\\xfc e\\xfc'))
        # encode with `encoding` attribute
        e.encoding = 'utf8'
        e.write(u' u\xfc')
        self.assertEquals(buf.getvalue(), b('b\xfc u\\xfc e\\xfc u\xc3\xbc'))

    def test_ubuf(self):
        buf = UBuf() # buffer only accepting unicode string
        # decode of binary strings
        e = io.ErrorOutput(buf, encoding='ascii')
        e.write(b('b\xfc'))
        self.assertEquals(buf.getvalue(), u'b\ufffd') # use REPLACEMENT CHARACTER
        # write Unicode string and Exceptions with Unicode args
        e.write(u' u\xfc')
        self.assertEquals(buf.getvalue(), u'b\ufffd u\xfc')
        e.write(AttributeError(u' e\xfc'))
        self.assertEquals(buf.getvalue(), u'b\ufffd u\xfc e\xfc')
        # decode with `encoding` attribute
        e.encoding = 'latin1'
        e.write(b(' b\xfc'))
        self.assertEquals(buf.getvalue(), u'b\ufffd u\xfc e\xfc b\xfc')


# class FileInputTests(unittest.TestCase):
#     def test_io_error_reporting(self):
#         # it seems like IOError and SystemExit are not catched by assertRaises:
#         self.assertRaises(IOError, open('foo'))
#         self.assertRaises(IOError,
#                           io.FileInput(source_path=u'u\xfc', handle_io_errors=False))
#         self.assertRaises(IOError,
#                           io.FileInput(source_path=u'u\xfc', handle_io_errors=True))


if __name__ == '__main__':
    unittest.main()
