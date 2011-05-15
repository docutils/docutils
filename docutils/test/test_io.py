#! /usr/bin/env python

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for io.py.
"""

try:
    import locale
except ImportError:
    locale = None

import unittest, sys
try: # from standard library module `io`
    from io import StringIO, BytesIO
except ImportError: # new in Python 2.6
    from StringIO import StringIO
    BytesIO = StringIO

import DocutilsTestSupport              # must be imported before docutils
from docutils import io
from docutils._compat import b, bytes


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


class ErrorStringTests(unittest.TestCase):
    # test data:
    bs = b('\xfc')     # unicode(bs) fails, str(bs) in Python 3 return repr()
    us = u'\xfc'       # bytes(us) fails; str(us) fails in Python 2
    be = Exception(bs) # unicode(be) fails
    ue = Exception(us) # bytes(ue) fails, str(ue) fails in Python 2;
                       # unicode(ue) fails in Python < 2.6 (issue2517_)
    # the error message in IOError comes from the OS, and in some locales
    # (e.g. ru_RU), contains high bit chars:
    if locale:
        try:
            locale.setlocale(locale.LC_ALL, '')
        except locale.Error:
            print 'cannot test locale'
    try:
        open(u'\xfc')
    except IOError, uioe:
        pass
    except UnicodeEncodeError:
        try:
            open(u'\xfc'.encode(sys.getfilesystemencoding(), 'replace'))
        except IOError, uioe:
            pass
    try:
        open(b('\xfc'))
    except IOError, bioe:
        pass
    if locale:
        locale.setlocale(locale.LC_ALL, 'C') # reset
    # wrapped test data:
    wbs = io.ErrorString(bs)
    wus = io.ErrorString(us)
    wbe = io.ErrorString(be)
    wue = io.ErrorString(ue)
    wbioe = io.ErrorString(bioe)
    wuioe = io.ErrorString(uioe)

    def test_7bit(self):
        # wrapping (not required with 7-bit chars) must not change the
        # result of conversions:
        bs = b('foo')
        us = u'foo'
        be = Exception(bs)
        ue = Exception(us)
        self.assertEqual(str(7), str(io.ErrorString(7)))
        self.assertEqual(str(bs), str(io.ErrorString(bs)))
        self.assertEqual(str(us), str(io.ErrorString(us)))
        self.assertEqual(str(be), str(io.ErrorString(be)))
        self.assertEqual(str(ue), str(io.ErrorString(ue)))
        self.assertEqual(unicode(7), unicode(io.ErrorString(7)))
        self.assertEqual(unicode(bs), unicode(io.ErrorString(bs)))
        self.assertEqual(unicode(us), unicode(io.ErrorString(us)))
        self.assertEqual(unicode(be), unicode(io.ErrorString(be)))
        self.assertEqual(unicode(ue), unicode(io.ErrorString(ue)))

    def test_ustr(self):
        """Test conversion to a unicode-string."""
        # unicode(self.bs) fails
        self.assertEqual(unicode, type(unicode(self.wbs)))
        self.assertEqual(unicode(self.us), unicode(self.wus))
        # unicode(self.be) fails
        self.assertEqual(unicode, type(unicode(self.wbe)))
        # unicode(ue) fails in Python < 2.6 (issue2517_)
        self.assertEqual(unicode, type(unicode(self.wue)))
        self.assertEqual(self.us, unicode(self.wue))
        # unicode(bioe) fails with e.g. 'ru_RU.utf8' locale
        self.assertEqual(unicode, type(unicode(self.wbioe)))
        self.assertEqual(unicode, type(unicode(self.wuioe)))

    def test_str(self):
        """Test conversion to a string (bytes in Python 2, unicode in Python 3)."""
        self.assertEqual(str(self.bs), str(self.wbs))
        self.assertEqual(str(self.be), str(self.be))
        # str(us) fails in Python 2
        self.assertEqual(str, type(str(self.wus)))
        # str(ue) fails in Python 2
        self.assertEqual(str, type(str(self.wue)))
        self.assertEqual(str(self.bioe), str(self.wbioe))
        self.assertEqual(str(self.uioe), str(self.wuioe))


# .. _issue2517: http://bugs.python.org/issue2517


# ErrorOutput tests
# -----------------

# Stub: Buffer with 'strict' auto-conversion of input to byte string:
class BBuf(BytesIO, object):
    def write(self, data):
        if isinstance(data, unicode):
            data.encode('ascii', 'strict')
        super(BBuf, self).write(data)

# Stub: Buffer expecting unicode string:
class UBuf(StringIO, object):
    def write(self, data):
        # emulate Python 3 handling of stdout, stderr
        if isinstance(data, bytes):
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


if __name__ == '__main__':
    unittest.main()
