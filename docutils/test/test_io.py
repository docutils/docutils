#! /usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for io.py.
"""
import os.path

from io import StringIO, BytesIO
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from docutils import io

# DATA_ROOT is ./test/data/ from the docutils root
DATA_ROOT = os.path.join(os.path.abspath(os.path.dirname(__file__)), 'data')


# Stub: Buffer with 'strict' auto-conversion of input to byte string:
class BBuf(BytesIO):
    def write(self, data):
        if isinstance(data, str):
            data.encode('ascii', 'strict')
        super().write(data)


# Stub: Buffer expecting unicode string:
class UBuf(StringIO):
    def write(self, data):
        # emulate Python 3 handling of stdout, stderr
        if isinstance(data, bytes):
            raise TypeError('must be unicode, not bytes')
        super().write(data)


class mock_stdout(UBuf):
    encoding = 'utf-8'

    def __init__(self):
        self.buffer = BBuf()
        super().__init__()


class HelperTests(unittest.TestCase):

    def test_check_encoding_true(self):
        """Return `True` if lookup returns the same codec"""
        self.assertEqual(io.check_encoding(mock_stdout, 'utf-8'), True)
        self.assertEqual(io.check_encoding(mock_stdout, 'utf_8'), True)
        self.assertEqual(io.check_encoding(mock_stdout, 'utf8'), True)
        self.assertEqual(io.check_encoding(mock_stdout, 'UTF-8'), True)

    def test_check_encoding_false(self):
        """Return `False` if lookup returns different codecs"""
        self.assertEqual(io.check_encoding(mock_stdout, 'ascii'), False)
        self.assertEqual(io.check_encoding(mock_stdout, 'latin-1'), False)

    def test_check_encoding_none(self):
        """Cases where the comparison fails."""
        # stream.encoding is None:
        self.assertEqual(io.check_encoding(io.FileInput(), 'ascii'), None)
        # stream.encoding does not exist:
        self.assertEqual(io.check_encoding(BBuf, 'ascii'), None)
        # encoding is None or empty string:
        self.assertEqual(io.check_encoding(mock_stdout, None), None)
        self.assertEqual(io.check_encoding(mock_stdout, ''), None)
        # encoding is invalid
        self.assertEqual(io.check_encoding(mock_stdout, 'UTF-9'), None)

    def test_error_string(self):
        us = '\xfc'       # bytes(us) fails
        bs = b'\xc3\xbc'  # str(bs) returns repr(bs)

        self.assertEqual('Exception: spam',
                         io.error_string(Exception('spam')))
        self.assertEqual('IndexError: ' + str(bs),
                         io.error_string(IndexError(bs)))
        self.assertEqual('ImportError: %s' % us,
                         io.error_string(ImportError(us)))


class InputTests(unittest.TestCase):

    def test_bom(self):
        # Provisional:
        input = io.StringInput(source=b'\xef\xbb\xbf foo \xef\xbb\xbf bar')
        # Assert BOM is gone.
        # TODO: only remove BOM (ZWNBSP at start of data)
        self.assertEqual(input.read(), ' foo  bar')
        # Unicode input is left unchanged:
        input = io.StringInput(source='\ufeff foo \ufeff bar')
        # Assert ZWNBSPs are still there.
        self.assertEqual(input.read(), '\ufeff foo \ufeff bar')

    def test_coding_slug(self):
        input = io.StringInput(source=b"""\
.. -*- coding: ascii -*-
data
blah
""")
        data = input.read()  # noqa: F841
        self.assertEqual(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b"""\
#! python
# -*- coding: ascii -*-
print("hello world")
""")
        data = input.read()  # noqa: F841
        self.assertEqual(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b"""\
#! python
# extraneous comment; prevents coding slug from being read
# -*- coding: ascii -*-
print("hello world")
""")
        self.assertNotEqual(input.successful_encoding, 'ascii')

    def test_bom_detection(self):
        source = '\ufeffdata\nblah\n'
        expected = 'data\nblah\n'
        input = io.StringInput(source=source.encode('utf-16-be'))
        self.assertEqual(input.read(), expected)
        input = io.StringInput(source=source.encode('utf-16-le'))
        self.assertEqual(input.read(), expected)
        input = io.StringInput(source=source.encode('utf-8'))
        self.assertEqual(input.read(), expected)

    def test_readlines(self):
        input = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'include.txt'))
        data = input.readlines()
        self.assertEqual(data, ['Some include text.\n'])

    def test_heuristics_no_utf8(self):
        # if no encoding is given and decoding with 'utf-8' fails,
        # use either the locale encoding (if specified) or 'latin-1':
        # Provisional: the second fallback 'latin-1' will be dropped
        probed_encodings = (io._locale_encoding, 'latin-1')  # noqa
        input = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'latin1.txt'))
        data = input.read()
        if input.successful_encoding not in probed_encodings:
            raise AssertionError(
                "guessed encoding '%s' differs from probed encodings %r"
                % (input.successful_encoding, probed_encodings))
        if input.successful_encoding == 'latin-1':
            self.assertEqual(data, 'Gr\xfc\xdfe\n')

    def test_decode_unicode(self):
        # With the special value "unicode" or "Unicode":
        uniinput = io.Input(encoding='unicode')
        # keep unicode instances as-is
        self.assertEqual(uniinput.decode('ja'), 'ja')
        # raise AssertionError if data is not an unicode string
        self.assertRaises(AssertionError, uniinput.decode, b'ja')


class OutputTests(unittest.TestCase):

    bdata = b'\xfc'
    udata = '\xfc'

    def setUp(self):
        self.bdrain = BBuf()
        """Buffer accepting binary strings (bytes)"""
        self.udrain = UBuf()
        """Buffer accepting unicode strings"""
        self.mock_stdout = mock_stdout()
        """Stub of sys.stdout under Python 3"""

    def test_write_unicode(self):
        fo = io.FileOutput(destination=self.udrain, encoding='unicode',
                           autoclose=False)
        fo.write(self.udata)
        self.assertEqual(self.udrain.getvalue(), self.udata)

    def test_write_utf8(self):
        fo = io.FileOutput(destination=self.udrain, encoding='utf-8',
                           autoclose=False)
        fo.write(self.udata)
        self.assertEqual(self.udrain.getvalue(), self.udata)

    def test_FileOutput_hande_io_errors_deprection_warning(self):
        with self.assertWarnsRegex(DeprecationWarning,
                                   '"handle_io_errors" is ignored'):
            io.FileOutput(handle_io_errors=True)

    # With destination in binary mode, data must be binary string
    # and is written as-is:
    def test_write_bytes(self):
        fo = io.FileOutput(destination=self.bdrain, encoding='utf-8',
                           mode='wb', autoclose=False)
        fo.write(self.bdata)
        self.assertEqual(self.bdrain.getvalue(), self.bdata)

    def test_write_bytes_to_stdout(self):
        # try writing data to `destination.buffer`, if data is
        # instance of `bytes` and writing to `destination` fails:
        fo = io.FileOutput(destination=self.mock_stdout)
        fo.write(self.bdata)
        self.assertEqual(self.mock_stdout.buffer.getvalue(),
                         self.bdata)

    def test_encoding_clash_resolved(self):
        fo = io.FileOutput(destination=self.mock_stdout,
                           encoding='latin1', autoclose=False)
        fo.write(self.udata)
        self.assertEqual(self.mock_stdout.buffer.getvalue(),
                         self.udata.encode('latin1'))

    def test_encoding_clash_nonresolvable(self):
        del self.mock_stdout.buffer
        fo = io.FileOutput(destination=self.mock_stdout,
                           encoding='latin1', autoclose=False)
        self.assertRaises(ValueError, fo.write, self.udata)


class ErrorOutputTests(unittest.TestCase):
    def test_defaults(self):
        e = io.ErrorOutput()
        self.assertEqual(e.destination, sys.stderr)

    def test_bbuf(self):
        buf = BBuf()  # buffer storing byte string
        e = io.ErrorOutput(buf, encoding='ascii')
        # write byte-string as-is
        e.write(b'b\xfc')
        self.assertEqual(buf.getvalue(), b'b\xfc')
        # encode unicode data with backslashescape fallback replacement:
        e.write(' u\xfc')
        self.assertEqual(buf.getvalue(), b'b\xfc u\\xfc')
        # handle Exceptions with Unicode string args
        # unicode(Exception('e\xfc')) # fails in Python < 2.6
        e.write(AttributeError(' e\xfc'))
        self.assertEqual(buf.getvalue(), b'b\xfc u\\xfc e\\xfc')
        # encode with `encoding` attribute
        e.encoding = 'utf-8'
        e.write(' u\xfc')
        self.assertEqual(buf.getvalue(), b'b\xfc u\\xfc e\\xfc u\xc3\xbc')

    def test_ubuf(self):
        buf = UBuf()  # buffer only accepting unicode string
        # decode of binary strings
        e = io.ErrorOutput(buf, encoding='ascii')
        e.write(b'b\xfc')
        # use REPLACEMENT CHARACTER
        self.assertEqual(buf.getvalue(), 'b\ufffd')
        # write Unicode string and Exceptions with Unicode args
        e.write(' u\xfc')
        self.assertEqual(buf.getvalue(), 'b\ufffd u\xfc')
        e.write(AttributeError(' e\xfc'))
        self.assertEqual(buf.getvalue(), 'b\ufffd u\xfc e\xfc')
        # decode with `encoding` attribute
        e.encoding = 'latin1'
        e.write(b' b\xfc')
        self.assertEqual(buf.getvalue(), 'b\ufffd u\xfc e\xfc b\xfc')


class FileInputTests(unittest.TestCase):

    # test input encoding auto-detection:
    #
    # Up to Docutils 0.18, auto-detection was not used under Python 3
    # unless reading a file with Python's default encoding failed

    def test_bom_utf_8(self):
        """Drop optional BOM from utf-8 encoded files.
        """
        source = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'utf-8-sig.txt'))
        self.assertTrue(source.read().startswith('Grüße'))

    def test_bom_utf_16(self):
        """Drop BOM from utf-16 encoded files, use correct encoding.
        """
        # Assert correct decoding, BOM is gone.
        source = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'utf-16-le-sig.txt'))
        self.assertTrue(source.read().startswith('Grüße'))

    def test_coding_slug(self):
        """Use self-declared encoding.
        """
        source = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'latin2.txt'))
        self.assertTrue(source.read().endswith('škoda\n'))

    def test_fallback_utf8(self):
        """Try 'utf-8', if encoding is not specified in the source."""
        source = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'utf8.txt'))
        self.assertEqual(source.read(), 'Grüße\n')

    @unittest.skipIf(io._locale_encoding in (None, 'utf-8', 'utf8'),
                     'locale encoding not set or UTF-8')
    def test_fallback_no_utf8(self):
        # if decoding with 'utf-8' fails, use the locale encoding
        # (if not None) or 'latin-1'.
        # provisional: behaviour details will change in future
        # TODO: don't fall back to latin1
        # TODO: use `locale.getpreferredlocale()` (honour UTF-8 mode)?
        probed_encodings = (io._locale_encoding, 'latin-1')  # noqa
        source = io.FileInput(
            source_path=os.path.join(DATA_ROOT, 'latin1.txt'))
        data = source.read()
        self.assertTrue(source.successful_encoding in probed_encodings)
        if source.successful_encoding in ('latin-1', 'iso8859-1'):
            self.assertEqual(data, 'Grüße\n')


if __name__ == '__main__':
    unittest.main()
