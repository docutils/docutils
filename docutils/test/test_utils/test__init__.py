#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for utils/__init__.py.
"""

from io import StringIO
import os
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils import nodes, utils

TEST_ROOT = Path(__file__).parents[1]  # ./test/ from the docutils root


class ReporterTests(unittest.TestCase):

    stream = StringIO()
    reporter = utils.Reporter('test data', 2, 4, stream, 1)

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()

    def test_level0(self):
        sw = self.reporter.system_message(0, 'debug output')
        self.assertEqual(sw.pformat(), """\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        debug output
""")
        self.assertEqual(self.stream.getvalue(),
                         'test data:: (DEBUG/0) debug output\n')

    def test_level1(self):
        sw = self.reporter.system_message(1, 'a little reminder')
        self.assertEqual(sw.pformat(), """\
<system_message level="1" source="test data" type="INFO">
    <paragraph>
        a little reminder
""")
        self.assertEqual(self.stream.getvalue(), '')

    def test_level2(self):
        sw = self.reporter.system_message(2, 'a warning')
        self.assertEqual(sw.pformat(), """\
<system_message level="2" source="test data" type="WARNING">
    <paragraph>
        a warning
""")
        self.assertEqual(self.stream.getvalue(),
                         'test data:: (WARNING/2) a warning\n')

    def test_level3(self):
        sw = self.reporter.system_message(3, 'an error')
        self.assertEqual(sw.pformat(), """\
<system_message level="3" source="test data" type="ERROR">
    <paragraph>
        an error
""")
        self.assertEqual(self.stream.getvalue(),
                         'test data:: (ERROR/3) an error\n')

    def test_level4(self):
        with self.assertRaises(utils.SystemMessage):
            self.reporter.system_message(
                4, 'a severe error, raises an exception')
        self.assertEqual(self.stream.getvalue(), 'test data:: (SEVERE/4) '
                         'a severe error, raises an exception\n')

    def test_unicode_message(self):
        sw = self.reporter.system_message(0, 'mesidʒ')
        self.assertEqual(sw.pformat(), """\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        mesidʒ
""")

    def test_unicode_message_from_exception(self):
        """Workaround for Python < 2.6 bug:
        unicode(<exception instance>) uses __str__
        and hence fails with unicode message"""
        try:
            raise Exception('mesidʒ')
        except Exception as err:
            sw = self.reporter.system_message(0, err)
            self.assertEqual(sw.pformat(), """\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        mesidʒ
""")


class QuietReporterTests(unittest.TestCase):

    stream = StringIO()
    reporter = utils.Reporter('test data', 5, 5, stream, 0)

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()

    def test_debug(self):
        sw = self.reporter.debug('a debug message')
        # None because debug is disabled.
        self.assertEqual(sw, None)
        self.assertEqual(self.stream.getvalue(), '')

    def test_info(self):
        sw = self.reporter.info('an informational message')
        self.assertEqual(sw.pformat(), """\
<system_message level="1" source="test data" type="INFO">
    <paragraph>
        an informational message
""")
        self.assertEqual(self.stream.getvalue(), '')

    def test_warning(self):
        sw = self.reporter.warning('a warning')
        self.assertEqual(sw.pformat(), """\
<system_message level="2" source="test data" type="WARNING">
    <paragraph>
        a warning
""")
        self.assertEqual(self.stream.getvalue(), '')

    def test_error(self):
        sw = self.reporter.error('an error')
        self.assertEqual(sw.pformat(), """\
<system_message level="3" source="test data" type="ERROR">
    <paragraph>
        an error
""")
        self.assertEqual(self.stream.getvalue(), '')

    def test_severe(self):
        sw = self.reporter.severe('a severe error')
        self.assertEqual(sw.pformat(), """\
<system_message level="4" source="test data" type="SEVERE">
    <paragraph>
        a severe error
""")
        self.assertEqual(self.stream.getvalue(), '')


class NameValueTests(unittest.TestCase):

    def test_extract_name_value(self):
        with self.assertRaises(utils.NameValueError):
            utils.extract_name_value('hello')
        with self.assertRaises(utils.NameValueError):
            utils.extract_name_value('=hello')
        with self.assertRaises(utils.NameValueError):
            utils.extract_name_value('hello=')
        with self.assertRaises(utils.NameValueError):
            utils.extract_name_value('hello="')
        with self.assertRaises(utils.NameValueError):
            utils.extract_name_value('hello="something')
        with self.assertRaises(utils.NameValueError):
            utils.extract_name_value('hello="something"else')
        output = utils.extract_name_value(
              """att1=val1 att2=val2 att3="value number '3'" att4=val4""")
        self.assertEqual(output, [('att1', 'val1'), ('att2', 'val2'),
                                  ('att3', "value number '3'"),
                                  ('att4', 'val4')])


class ExtensionOptionTests(unittest.TestCase):

    optionspec = {'a': int, 'bbb': float, 'cdef': (lambda x: x),
                  'empty': (lambda x: x)}

    def test_assemble_option_dict(self):
        input_ = utils.extract_name_value('a=1 bbb=2.0 cdef=hol%s' % chr(224))
        self.assertEqual(
              utils.assemble_option_dict(input_, self.optionspec),
              {'a': 1, 'bbb': 2.0, 'cdef': ('hol%s' % chr(224))})
        input_ = utils.extract_name_value('a=1 b=2.0 c=hol%s' % chr(224))
        with self.assertRaises(KeyError):
            utils.assemble_option_dict(input_, self.optionspec)
        input_ = utils.extract_name_value('a=1 bbb=two cdef=hol%s' % chr(224))
        with self.assertRaises(ValueError):
            utils.assemble_option_dict(input_, self.optionspec)

    def test_extract_extension_options(self):
        field_list = nodes.field_list()
        field_list += nodes.field(
              '', nodes.field_name('', 'a'),
              nodes.field_body('', nodes.paragraph('', '1')))
        field_list += nodes.field(
              '', nodes.field_name('', 'bbb'),
              nodes.field_body('', nodes.paragraph('', '2.0')))
        field_list += nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'hol\u00e0')))
        field_list += nodes.field(
              '', nodes.field_name('', 'empty'), nodes.field_body())
        self.assertEqual(
              utils.extract_extension_options(field_list, self.optionspec),
              {'a': 1, 'bbb': 2.0,
               'cdef': 'hol\u00e0',
               'empty': None})
        with self.assertRaises(KeyError):
            utils.extract_extension_options(field_list, {})
        field_list += nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'one'),
                               nodes.paragraph('', 'two')))
        with self.assertRaises(utils.BadOptionDataError):
            utils.extract_extension_options(field_list, self.optionspec)
        field_list[-1] = nodes.field(
              '', nodes.field_name('', 'cdef bad'),
              nodes.field_body('', nodes.paragraph('', 'no arguments')))
        with self.assertRaises(utils.BadOptionError):
            utils.extract_extension_options(field_list, self.optionspec)
        field_list[-1] = nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'duplicate')))
        with self.assertRaises(utils.DuplicateOptionError):
            utils.extract_extension_options(field_list, self.optionspec)
        field_list[-2] = nodes.field(
              '', nodes.field_name('', 'unkown'),
              nodes.field_body('', nodes.paragraph('', 'unknown')))
        with self.assertRaises(KeyError):
            utils.extract_extension_options(field_list, self.optionspec)


class HelperFunctionTests(unittest.TestCase):
    def test_normalize_language_tag(self):
        self.assertEqual(utils.normalize_language_tag('de'), ['de'])
        self.assertEqual(utils.normalize_language_tag('de-AT'),
                         ['de-at', 'de'])
        self.assertEqual(utils.normalize_language_tag('de-AT-1901'),
                         ['de-at-1901', 'de-at', 'de-1901', 'de'])
        self.assertEqual(utils.normalize_language_tag('de-AT-1901-Latf'),
                         ['de-at-1901-latf', 'de-at-1901', 'de-at-latf',
                          'de-1901-latf', 'de-at', 'de-1901', 'de-latf', 'de'])
        self.assertEqual(utils.normalize_language_tag('grc-ibycus-x-altquot'),
                         ['grc-ibycus-x-altquot', 'grc-ibycus',
                          'grc-x-altquot', 'grc'])

    def test_xml_declaration(self):
        # default is no encoding declaration
        self.assertEqual(utils.xml_declaration(), '<?xml version="1.0"?>\n')
        # if an encoding is passed, declare it
        self.assertEqual(utils.xml_declaration('ISO-8859-2'),
                         '<?xml version="1.0" encoding="ISO-8859-2"?>\n')
        # ignore pseudo encoding name "unicode" introduced by
        # `docutils.io.Output.encode()`
        self.assertEqual(utils.xml_declaration('Unicode'),
                         '<?xml version="1.0"?>\n')
        # ... non-regarding case
        self.assertEqual(utils.xml_declaration('UNICODE'),
                         '<?xml version="1.0"?>\n')
        # allow %s for later interpolation
        # (used for part 'html_prolog', cf. docs/api/publisher.html)
        self.assertEqual(utils.xml_declaration('%s'),
                         '<?xml version="1.0" encoding="%s"?>\n')

    def test_column_width(self):
        self.assertEqual(utils.column_width('de'), 2)
        self.assertEqual(utils.column_width('dâ'), 2)  # pre-composed
        self.assertEqual(utils.column_width('dâ'), 2)  # combining

    def test_decode_path(self):
        try:
            bytes_filename = 'späm'.encode(sys.getfilesystemencoding())
        except UnicodeEncodeError:
            bytes_filename = b'spam'
        bytespath = utils.decode_path(bytes_filename)
        unipath = utils.decode_path('späm')
        defaultpath = utils.decode_path(None)
        if bytes_filename != b'spam':  # skip if ä cannot be encoded
            self.assertEqual(bytespath, 'späm')
        self.assertEqual(unipath, 'späm')
        self.assertEqual(defaultpath, '')
        self.assertTrue(isinstance(bytespath, str))
        self.assertTrue(isinstance(unipath, str))
        self.assertTrue(isinstance(defaultpath, str))
        self.assertRaises(ValueError, utils.decode_path, 13)

    def test_relative_path(self):
        # Build and return a path to `target`, relative to `source`:
        # Use '/' as path sep in result.
        self.assertEqual(utils.relative_path('spam', 'spam'), '')
        source = os.path.join('häm', 'spam', 'fileA')
        target = os.path.join('häm', 'spam', 'fileB')
        self.assertEqual(utils.relative_path(source, target), 'fileB')
        source = os.path.join('häm', 'spam', 'fileA')
        target = os.path.join('häm', 'fileB')
        self.assertEqual(utils.relative_path(source, target), '../fileB')
        source = os.path.join('häm', 'fileA')
        target = os.path.join('..', 'spam', 'fileB')
        self.assertEqual(utils.relative_path(source, target),
                         '../../spam/fileB')
        # if source is None, default to the cwd:
        target = os.path.join('eggs', 'fileB')
        self.assertEqual(utils.relative_path(None, target), 'eggs/fileB')
        # If there is no common prefix, return the absolute path to `target`:
        if os.sep == '/':
            source = '/foo/bar/fileA'
        else:
            source = r'C:\foo\bar\fileA'
        target = os.path.join('eggs', 'fileB')
        self.assertEqual(utils.relative_path(source, target),
                         os.path.abspath('eggs/fileB').replace('\\', '/'))
        # Correctly process characters outside the ASCII range:
        self.assertEqual(utils.relative_path('spam', 'spam'), '')
        source = os.path.join('häm', 'spam', 'fileA')
        target = os.path.join('häm', 'spam', 'fileB')
        self.assertEqual(utils.relative_path(source, target), 'fileB')
        source = os.path.join('häm', 'spam', 'fileA')
        target = os.path.join('häm', 'fileB')
        self.assertEqual(utils.relative_path(source, target), '../fileB')
        # if source is None, default to the cwd:
        target = os.path.join('eggs', 'fileB')
        self.assertEqual(utils.relative_path(None, target), 'eggs/fileB')

    def test_find_file_in_dirs(self):
        # Search for file `path` in the sequence of directories `dirs`.
        # Return the first expansion that matches an existing file.
        dirs = (os.path.join(TEST_ROOT, 'nonex'),
                TEST_ROOT,
                os.path.join(TEST_ROOT, '..'))
        result = utils.find_file_in_dirs('alltests.py', dirs)
        expected = os.path.join(TEST_ROOT, 'alltests.py').replace('\\', '/')
        self.assertEqual(expected, result)
        result = utils.find_file_in_dirs('HISTORY.rst', dirs)
        expected = (TEST_ROOT / '..' / 'HISTORY.rst').as_posix()
        self.assertEqual(expected, result)
        # normalize for second check
        self.assertTrue(os.path.relpath(result, TEST_ROOT).startswith('..'),
                        'HISTORY.rst not found in "..".')
        # Return `path` if the file exists in the cwd or if there is no match
        self.assertEqual(utils.find_file_in_dirs('gibts/nicht.rst', dirs),
                         'gibts/nicht.rst')

    # samples for the (un)escaping tests:
    escaped = r'escapes: \*one, \\*two, \\\*three in\side no\ space' + '\\'
    nulled = ('escapes: \x00*one, \x00\\*two, \x00\\\x00*three'
              + ' in\x00side no\x00 space\x00')
    unescaped = r'escapes: *one, \*two, \*three inside nospace'

    def test_escape2null(self):
        nulled = utils.escape2null(self.escaped)
        self.assertEqual(nulled, self.nulled)

    def test_unescape(self):
        unescaped = utils.unescape(self.nulled)
        self.assertEqual(unescaped, self.unescaped)
        restored = utils.unescape(self.nulled, restore_backslashes=True)
        self.assertEqual(restored, self.escaped)


class StylesheetFunctionTests(unittest.TestCase):

    stylesheet_dirs = [TEST_ROOT, os.path.join(TEST_ROOT, 'data')]

    def test_get_stylesheet_list_stylesheet_path(self):
        # look for stylesheets in stylesheet_dirs
        self.stylesheet = None
        self.stylesheet_path = 'ham.css, missing.css'

        ham_css = os.path.join(TEST_ROOT, 'data', 'ham.css').replace('\\', '/')
        self.assertEqual(utils.get_stylesheet_list(self),
                         [ham_css, 'missing.css'])

    def test_get_stylesheet_list_stylesheet(self):
        # use stylesheet paths verbatim
        self.stylesheet = 'ham.css, missing.css'
        self.stylesheet_path = None

        self.assertEqual(utils.get_stylesheet_list(self),
                         ['ham.css', 'missing.css'])

    def test_get_stylesheet_list_conflict(self):
        # settings "stylesheet_path" and "stylesheet"
        # must not be used together
        self.stylesheet = 'ham.css, missing.css'
        self.stylesheet_path = 'man.css, miss2.css'
        with self.assertRaises(AssertionError):
            utils.get_stylesheet_list(self)


if __name__ == '__main__':
    unittest.main()
