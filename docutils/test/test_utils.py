#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test module for utils.py.
"""

import unittest
import StringIO
import sys
from DocutilsTestSupport import utils, nodes


class ReporterTests(unittest.TestCase):

    stream = StringIO.StringIO()
    reporter = utils.Reporter('test data', 2, 4, stream, 1)

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()

    def test_level0(self):
        sw = self.reporter.system_message(0, 'debug output')
        self.assertEquals(sw.pformat(), """\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        debug output
""")
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (DEBUG/0) debug output\n')

    def test_level1(self):
        sw = self.reporter.system_message(1, 'a little reminder')
        self.assertEquals(sw.pformat(), """\
<system_message level="1" source="test data" type="INFO">
    <paragraph>
        a little reminder
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_level2(self):
        sw = self.reporter.system_message(2, 'a warning')
        self.assertEquals(sw.pformat(), """\
<system_message level="2" source="test data" type="WARNING">
    <paragraph>
        a warning
""")
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (WARNING/2) a warning\n')

    def test_level3(self):
        sw = self.reporter.system_message(3, 'an error')
        self.assertEquals(sw.pformat(), """\
<system_message level="3" source="test data" type="ERROR">
    <paragraph>
        an error
""")
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (ERROR/3) an error\n')

    def test_level4(self):
        self.assertRaises(utils.SystemMessage, self.reporter.system_message, 4,
                          'a severe error, raises an exception')
        self.assertEquals(self.stream.getvalue(), 'test data:: (SEVERE/4) '
                          'a severe error, raises an exception\n')


class QuietReporterTests(unittest.TestCase):

    stream = StringIO.StringIO()
    reporter = utils.Reporter('test data', 5, 5, stream, 0)

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()

    def test_debug(self):
        sw = self.reporter.debug('a debug message')
        self.assertEquals(sw.pformat(), """\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        a debug message
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_info(self):
        sw = self.reporter.info('an informational message')
        self.assertEquals(sw.pformat(), """\
<system_message level="1" source="test data" type="INFO">
    <paragraph>
        an informational message
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_warning(self):
        sw = self.reporter.warning('a warning')
        self.assertEquals(sw.pformat(), """\
<system_message level="2" source="test data" type="WARNING">
    <paragraph>
        a warning
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_error(self):
        sw = self.reporter.error('an error')
        self.assertEquals(sw.pformat(), """\
<system_message level="3" source="test data" type="ERROR">
    <paragraph>
        an error
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_severe(self):
        sw = self.reporter.severe('a severe error')
        self.assertEquals(sw.pformat(), """\
<system_message level="4" source="test data" type="SEVERE">
    <paragraph>
        a severe error
""")
        self.assertEquals(self.stream.getvalue(), '')


class ReporterCategoryTests(unittest.TestCase):

    stream = StringIO.StringIO()

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()
        self.reporter = utils.Reporter('test data', 2, 4, self.stream, 1)
        self.reporter.set_conditions('lemon', 1, 3, self.stream, 0)

    def test_getset(self):
        self.reporter.set_conditions('test', 5, 5, None, 0)
        self.assertEquals(self.reporter.get_conditions('other').astuple(),
                          (1, 2, 4, self.stream))
        self.assertEquals(self.reporter.get_conditions('test').astuple(),
                          (0, 5, 5, sys.stderr))
        self.assertEquals(self.reporter.get_conditions('test.dummy').astuple(),
                          (0, 5, 5, sys.stderr))
        self.reporter.set_conditions('test.dummy.spam', 1, 2, self.stream, 1)
        self.assertEquals(
              self.reporter.get_conditions('test.dummy.spam').astuple(),
              (1, 1, 2, self.stream))
        self.assertEquals(self.reporter.get_conditions('test.dummy').astuple(),
                          (0, 5, 5, sys.stderr))
        self.assertEquals(
              self.reporter.get_conditions('test.dummy.spam.eggs').astuple(),
              (1, 1, 2, self.stream))
        self.reporter.unset_conditions('test.dummy.spam')
        self.assertEquals(
              self.reporter.get_conditions('test.dummy.spam.eggs').astuple(),
              (0, 5, 5, sys.stderr))

    def test_debug(self):
        sw = self.reporter.debug('debug output', category='lemon.curry')
        self.assertEquals(self.stream.getvalue(), '')
        sw = self.reporter.debug('debug output')
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (DEBUG/0) debug output\n')

    def test_info(self):
        sw = self.reporter.info('some info')
        self.assertEquals(self.stream.getvalue(), '')
        sw = self.reporter.info('some info', category='lemon.curry')
        self.assertEquals(
              self.stream.getvalue(),
              'test data:: (INFO/1) some info [lemon.curry]\n')

    def test_warning(self):
        sw = self.reporter.warning('a warning')
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (WARNING/2) a warning\n')
        sw = self.reporter.warning('a warning', category='lemon.curry')
        self.assertEquals(self.stream.getvalue(), """\
test data:: (WARNING/2) a warning
test data:: (WARNING/2) a warning [lemon.curry]
""")

    def test_error(self):
        sw = self.reporter.error('an error')
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (ERROR/3) an error\n')
        self.assertRaises(utils.SystemMessage, self.reporter.error,
                          'an error', category='lemon.curry')
        self.assertEquals(self.stream.getvalue(), """\
test data:: (ERROR/3) an error
test data:: (ERROR/3) an error [lemon.curry]
""")

    def test_severe(self):
        self.assertRaises(utils.SystemMessage, self.reporter.severe,
                          'a severe error')
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (SEVERE/4) a severe error\n')
        self.assertRaises(utils.SystemMessage, self.reporter.severe,
                          'a severe error', category='lemon.curry')
        self.assertEquals(self.stream.getvalue(), """\
test data:: (SEVERE/4) a severe error
test data:: (SEVERE/4) a severe error [lemon.curry]
""")


class NameValueTests(unittest.TestCase):

    def test_extract_name_value(self):
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          '=hello')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello=')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello="')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello="something')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello="something"else')
        output = utils.extract_name_value(
              """att1=val1 att2=val2 att3="value number '3'" att4=val4""")
        self.assertEquals(output, [('att1', 'val1'), ('att2', 'val2'),
                                   ('att3', "value number '3'"),
                                   ('att4', 'val4')])


class ExtensionOptionTests(unittest.TestCase):

    optionspec = {'a': int, 'bbb': float, 'cdef': (lambda x: x),
                  'empty': (lambda x: x)}

    def test_assemble_option_dict(self):
        input = utils.extract_name_value('a=1 bbb=2.0 cdef=hol%s' % chr(224))
        self.assertEquals(
              utils.assemble_option_dict(input, self.optionspec),
              {'a': 1, 'bbb': 2.0, 'cdef': ('hol%s' % chr(224))})
        input = utils.extract_name_value('a=1 b=2.0 c=hol%s' % chr(224))
        self.assertRaises(KeyError, utils.assemble_option_dict,
                          input, self.optionspec)
        input = utils.extract_name_value('a=1 bbb=two cdef=hol%s' % chr(224))
        self.assertRaises(ValueError, utils.assemble_option_dict,
                          input, self.optionspec)

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
              nodes.field_body('', nodes.paragraph('', 'hol%s' % chr(224))))
        field_list += nodes.field(
              '', nodes.field_name('', 'empty'), nodes.field_body())
        self.assertEquals(
              utils.extract_extension_options(field_list, self.optionspec),
              {'a': 1, 'bbb': 2.0, 'cdef': ('hol%s' % chr(224)),
               'empty': None})
        self.assertRaises(KeyError, utils.extract_extension_options,
                          field_list, {})
        field_list += nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'one'),
                               nodes.paragraph('', 'two')))
        self.assertRaises(utils.BadOptionDataError,
                          utils.extract_extension_options,
                          field_list, self.optionspec)
        field_list[-1] = nodes.field(
              '', nodes.field_name('', 'cdef bad'),
              nodes.field_body('', nodes.paragraph('', 'no arguments')))
        self.assertRaises(utils.BadOptionError,
                          utils.extract_extension_options,
                          field_list, self.optionspec)
        field_list[-1] = nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'duplicate')))
        self.assertRaises(utils.DuplicateOptionError,
                          utils.extract_extension_options,
                          field_list, self.optionspec)
        field_list[-2] = nodes.field(
              '', nodes.field_name('', 'unkown'),
              nodes.field_body('', nodes.paragraph('', 'unknown')))
        self.assertRaises(KeyError, utils.extract_extension_options,
                          field_list, self.optionspec)


class MiscFunctionTests(unittest.TestCase):

    names = [('a', 'a'), ('A', 'a'), ('A a A', 'a a a'),
             ('A  a  A  a', 'a a a a'),
             ('  AaA\n\r\naAa\tAaA\t\t', 'aaa aaa aaa')]

    def test_normalize_name(self):
        for input, output in self.names:
            normed = utils.normalize_name(input)
            self.assertEquals(normed, output)


if __name__ == '__main__':
    unittest.main()
