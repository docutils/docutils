#! /usr/bin/env python3
# $Id$
# Author: Günter Milde <milde@users.sf.net>
# Copyright: This module has been placed in the public domain.

"""
Test `EnvironmentError` reporting.

In some locales, the `errstr` argument of IOError and OSError contains
non-ASCII chars.

In Python 2, converting an exception instance to `str` or `unicode`
might fail, with non-ASCII chars in arguments and the default encoding
and errors ('ascii', 'strict').

Therefore, Docutils must not use string interpolation with exception
instances like, e.g., ::

  try:
    something
  except IOError as error:
    print('Found %s' % error)

unless the minimal required Python version has this problem fixed.
"""

from io import StringIO, BytesIO
import os
import sys
import unittest
import warnings
warnings.filterwarnings('ignore', category=DeprecationWarning,
                        message=r'.*utils\.error_reporting')

import DocutilsTestSupport              # must be imported before docutils
from docutils import core, parsers, frontend, utils
from docutils.utils.error_reporting import SafeString, ErrorString, ErrorOutput


class SafeStringTests(unittest.TestCase):

    # test data:
    bs = b'\xc3\xbc'   # str(bs) returns repr(bs)
    us = u'\xfc'       # bytes(us) fails (requires encoding argument)
    be = Exception(bs)
    ue = Exception(us) # bytes(ue) fails
    # wrapped test data:
    wbs = SafeString(bs)
    wus = SafeString(us)
    wbe = SafeString(be)
    wue = SafeString(ue)

    def test_7bit(self):
        # wrapping (not required with 7-bit chars) must not change the
        # result of conversions:
        bs7 = b'foo'
        us7 = u'foo'
        be7 = Exception(bs7)
        ue7 = Exception(us7)
        self.assertEqual(str(bs7), str(SafeString(bs7)))
        self.assertEqual(str(us7), str(SafeString(us7)))
        self.assertEqual(str(be7), str(SafeString(be7)))
        self.assertEqual(str(ue7), str(SafeString(ue7)))

    def test_ustr(self):
        """Test conversion to a unicode-string."""
        # unicode(self.bs) fails
        self.assertEqual(str, type(str(self.wbs)))
        self.assertEqual(str(self.us), str(self.wus))
        # unicode(self.be) fails
        self.assertEqual(str, type(str(self.wbe)))
        self.assertEqual(str, type(str(self.ue)))
        self.assertEqual(str, type(str(self.wue)))
        self.assertEqual(self.us, str(self.wue))

    def test_str(self):
        """Test conversion to a string (bytes in Python 2, unicode in Python 3)."""
        self.assertEqual(str(self.bs), str(self.wbs))
        self.assertEqual(str(self.be), str(self.wbe))
        self.assertEqual(str(self.us), str(self.wus))
        self.assertEqual(str(self.ue), str(self.wue))


class ErrorStringTests(unittest.TestCase):
    bs = b'\xc3\xbc' # unicode(bs) fails, str(bs) in Python 3 return repr()
    us = u'\xfc'     # bytes(us) fails; str(us) fails in Python 2

    def test_str(self):
        self.assertEqual('Exception: spam',
                         str(ErrorString(Exception('spam'))))
        self.assertEqual('IndexError: '+str(self.bs),
                         str(ErrorString(IndexError(self.bs))))
        self.assertEqual('ImportError: %s' % SafeString(self.us),
                         str(ErrorString(ImportError(self.us))))

    def test_unicode(self):
        self.assertEqual(u'Exception: spam',
                         str(ErrorString(Exception(u'spam'))))
        self.assertEqual(u'IndexError: '+self.us,
                         str(ErrorString(IndexError(self.us))))
        self.assertEqual(u'ImportError: %s' % SafeString(self.bs),
                         str(ErrorString(ImportError(self.bs))))


# ErrorOutput tests
# -----------------

# Stub: Buffer with 'strict' auto-conversion of input to byte string:
class BBuf(BytesIO):
    def write(self, data):
        if isinstance(data, str):
            data.encode('ascii', 'strict')
        super(BBuf, self).write(data)

# Stub: Buffer expecting unicode string:
class UBuf(StringIO):
    def write(self, data):
        # emulate Python 3 handling of stdout, stderr
        if isinstance(data, bytes):
            raise TypeError('must be unicode, not bytes')
        super(UBuf, self).write(data)

class ErrorOutputTests(unittest.TestCase):
    def test_defaults(self):
        e = ErrorOutput()
        self.assertEqual(e.stream, sys.stderr)

    def test_bbuf(self):
        buf = BBuf() # buffer storing byte string
        e = ErrorOutput(buf, encoding='ascii')
        # write byte-string as-is
        e.write(b'b\xfc')
        self.assertEqual(buf.getvalue(), b'b\xfc')
        # encode unicode data with backslashescape fallback replacement:
        e.write(u' u\xfc')
        self.assertEqual(buf.getvalue(), b'b\xfc u\\xfc')
        # handle Exceptions with Unicode string args
        # unicode(Exception(u'e\xfc')) # fails in Python < 2.6
        e.write(AttributeError(u' e\xfc'))
        self.assertEqual(buf.getvalue(), b'b\xfc u\\xfc e\\xfc')
        # encode with `encoding` attribute
        e.encoding = 'utf8'
        e.write(u' u\xfc')
        self.assertEqual(buf.getvalue(), b'b\xfc u\\xfc e\\xfc u\xc3\xbc')

    def test_ubuf(self):
        buf = UBuf() # buffer only accepting unicode string
        # decode of binary strings
        e = ErrorOutput(buf, encoding='ascii')
        e.write(b'b\xfc')
        self.assertEqual(buf.getvalue(), u'b\ufffd') # use REPLACEMENT CHARACTER
        # write Unicode string and Exceptions with Unicode args
        e.write(u' u\xfc')
        self.assertEqual(buf.getvalue(), u'b\ufffd u\xfc')
        e.write(AttributeError(u' e\xfc'))
        self.assertEqual(buf.getvalue(), u'b\ufffd u\xfc e\xfc')
        # decode with `encoding` attribute
        e.encoding = 'latin1'
        e.write(b' b\xfc')
        self.assertEqual(buf.getvalue(), u'b\ufffd u\xfc e\xfc b\xfc')


class SafeStringTests_locale(unittest.TestCase):
    """
    Test docutils.SafeString with 'problematic' locales.

    The error message in `EnvironmentError` instances comes from the OS
    and in some locales (e.g. ru_RU), contains high bit chars.
    """
    # test data:
    bs = b'\xc3\xbc'
    us = u'\xfc'
    try:
        open(b'\xc3\xbc')
    except IOError as e: # in Python 3 the name for the exception instance
        bioe = e       # is local to the except clause
    try:
        open(u'\xfc')
    except IOError as e:
        uioe = e
    except UnicodeEncodeError:
        try:
            open(u'\xfc'.encode(sys.getfilesystemencoding(), 'replace'))
        except IOError as e:
            uioe = e
    try:
        os.chdir(b'\xc3\xbc')
    except OSError as e:
        bose = e
    try:
        os.chdir(u'\xfc')
    except OSError as e:
        uose = e
    except UnicodeEncodeError:
        try:
            os.chdir(u'\xfc'.encode(sys.getfilesystemencoding(), 'replace'))
        except OSError as e:
            uose = e
    # wrapped test data:
    wbioe = SafeString(bioe)
    wuioe = SafeString(uioe)
    wbose = SafeString(bose)
    wuose = SafeString(uose)

    def test_ustr(self):
        """Test conversion to a unicode-string."""
        # unicode(bioe) fails with e.g. 'ru_RU.utf8' locale
        self.assertEqual(str, type(str(self.wbioe)))
        self.assertEqual(str, type(str(self.wuioe)))
        self.assertEqual(str, type(str(self.wbose)))
        self.assertEqual(str, type(str(self.wuose)))

    def test_str(self):
        """Test conversion to a string (bytes in Python 2, unicode in Python 3)."""
        self.assertEqual(str(self.bioe), str(self.wbioe))
        self.assertEqual(str(self.uioe), str(self.wuioe))
        self.assertEqual(str(self.bose), str(self.wbose))
        self.assertEqual(str(self.uose), str(self.wuose))



class ErrorReportingTests(unittest.TestCase):
    """
    Test cases where error reporting can go wrong.

    Do not test the exact output (as this varies with the locale), just
    ensure that the correct exception is thrown.
    """

    # These tests fail with a 'problematic locale',
    # Docutils revision < 7035, and Python 2:

    parser = parsers.rst.Parser()
    """Parser shared by all ParserTestCases."""

    option_parser = frontend.OptionParser(components=(parsers.rst.Parser,))
    settings = option_parser.get_default_values()
    settings.report_level = 1
    settings.halt_level = 1
    settings.warning_stream = ''
    document = utils.new_document('test data', settings)

    def test_include(self):
        source = ('.. include:: bogus.txt')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

    def test_raw_file(self):
        source = ('.. raw:: html\n'
                  '   :file: bogus.html\n')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

    def test_csv_table(self):
        source = ('.. csv-table:: external file\n'
                  '   :file: bogus.csv\n')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)


if __name__ == '__main__':
    unittest.main()
