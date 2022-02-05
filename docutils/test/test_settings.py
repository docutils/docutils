#!/usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests of runtime settings.
"""

import sys
import os
import difflib
import pprint
import warnings
import unittest
import DocutilsTestSupport              # must be imported before docutils
from docutils import frontend, utils
from docutils.writers import html4css1, pep_html, html5_polyglot
from docutils.parsers import rst

def fixpath(path):
    return os.path.abspath(os.path.join(*(path.split('/'))))

class ConfigFileTests(unittest.TestCase):

    config_files = {'old': fixpath('data/config_old.txt'),
                    'one': fixpath('data/config_1.txt'),
                    'two': fixpath('data/config_2.txt'),
                    'list': fixpath('data/config_list.txt'),
                    'list2': fixpath('data/config_list_2.txt'),
                    'error': fixpath('data/config_error_handler.txt'),
                    'error2': fixpath('data/config_error_handler_2.txt')}

    # expected settings after parsing the equally named config_file:
    settings = {
        'old': {'datestamp': '%Y-%m-%d %H:%M UTC',
                'generator': True,
                'no_random': True,
                'python_home': 'http://www.python.org',
                'source_link': True,
                'stylesheet': None,
                'stylesheet_path': ['stylesheets/pep.css'],
                'template': fixpath('data/pep-html-template'),
                },
        'one': {'datestamp': '%Y-%m-%d %H:%M UTC',
                'generator': True,
                'no_random': True,
                'python_home': 'http://www.python.org',
                'raw_enabled': False,
                'record_dependencies': utils.DependencyList(),
                'source_link': True,
                'stylesheet': None,
                'stylesheet_path': ['stylesheets/pep.css'],
                'tab_width': 8,
                'template': fixpath('data/pep-html-template'),
                'trim_footnote_reference_space': True,
                'output_encoding': 'ascii',
                'output_encoding_error_handler': 'xmlcharrefreplace',
                },
        'two': {'footnote_references': 'superscript',
                'generator': False,
                'record_dependencies': utils.DependencyList(),
                'stylesheet': None,
                'stylesheet_path': ['test.css'],
                'trim_footnote_reference_space': None,
                'output_encoding_error_handler': 'namereplace',
                },
        'two (html5)': {
                # use defaults from html5_polyglot writer component
                # ignore settings in [html4css1 writer] section,
                'generator': True,
                'raw_enabled': False,
                'record_dependencies': utils.DependencyList(),
                'source_link': False,
                'tab_width': 8,
                'trim_footnote_reference_space': True,
                'output_encoding_error_handler': 'namereplace',
                },
        'list': {'expose_internals': ['a', 'b', 'c', 'd', 'e'],
                 'smartquotes_locales': [('de', '«»‹›')],
                 'strip_classes': ['spam', 'pan', 'fun', 'parrot'],
                 'strip_elements_with_classes': ['sugar', 'flour', 'milk',
                                                 'safran']
                 },
        'list2': {'expose_internals': ['a', 'b', 'c', 'd', 'e', 'f'],
                  'smartquotes_locales': [('de', '«»‹›'),
                                          ('nl', '„”’’'),
                                          ('cs', '»«›‹'),
                                          ('fr', ['« ', ' »', '‹ ', ' ›'])
                                          ],
                  'strip_classes': ['spam', 'pan', 'fun', 'parrot',
                                    'ham', 'eggs'],
                  'strip_elements_with_classes': ['sugar', 'flour', 'milk',
                                                  'safran', 'eggs', 'salt'],
                  'stylesheet': ['style2.css', 'style3.css'],
                  'stylesheet_path': None,
                  },
        'error': {'error_encoding': 'ascii',
                  'error_encoding_error_handler': 'strict'},
        'error2': {'error_encoding': 'latin1'},
        }

    compare = difflib.Differ().compare
    """Comparison method shared by all tests."""

    def setUp(self):
        warnings.filterwarnings(action='ignore',
                                category=frontend.ConfigDeprecationWarning)
        self.option_parser = frontend.OptionParser(
            components=(pep_html.Writer, rst.Parser), read_config_files=None)

    def files_settings(self, *names):
        settings = frontend.Values()
        for name in names:
            cfs = self.option_parser.get_config_file_settings(
                                                    self.config_files[name])
            settings.update(cfs, self.option_parser)
        return settings.__dict__

    def expected_settings(self, *names):
        expected = {}
        for name in names:
            expected.update(self.settings[name])
        return expected

    def compare_output(self, result, expected):
        """`result` and `expected` should both be dicts."""
        self.assertTrue('record_dependencies' in result)
        if 'record_dependencies' not in expected:
            # Delete it if we don't want to test it.
            del result['record_dependencies']
        result = pprint.pformat(result) + '\n'
        expected = pprint.pformat(expected) + '\n'
        try:
            self.assertEqual(result, expected)
        except AssertionError:
            print('\n%s\n' % (self,), file=sys.stderr)
            print('-: expected\n+: result', file=sys.stderr)
            print(''.join(self.compare(expected.splitlines(True),
                                       result.splitlines(True))), file=sys.stderr)
            raise

    def test_nofiles(self):
        self.compare_output(self.files_settings(),
                            self.expected_settings())

    def test_old(self):
        with warnings.catch_warnings(record=True) as wngs:
            warnings.simplefilter("always") # check also for deprecation warning
            self.compare_output(self.files_settings('old'),
                                self.expected_settings('old'))
            warnings.filterwarnings(action='ignore',
                                    category=frontend.ConfigDeprecationWarning)
            self.assertTrue(len(wngs) > 0, "Expected a FutureWarning.")
            assert any(issubclass(wng.category, FutureWarning)
                       for wng in wngs)

    def test_one(self):
        self.compare_output(self.files_settings('one'),
                            self.expected_settings('one'))

    def test_multiple(self):
        self.compare_output(self.files_settings('one', 'two'),
                            self.expected_settings('one', 'two'))

    def test_multiple_with_html5_writer(self):
        # initialize option parser with different component set
        self.option_parser = frontend.OptionParser(
            components=(html5_polyglot.Writer, rst.Parser), read_config_files=None)
        # generator setting not changed by "config_2.txt":
        self.compare_output(self.files_settings('one', 'two'),
                            self.expected_settings('two (html5)'))

    def test_old_and_new(self):
        self.compare_output(self.files_settings('old', 'two'),
                            self.expected_settings('old', 'two'))

    def test_list(self):
        self.compare_output(self.files_settings('list'),
                            self.expected_settings('list'))

    def test_list2(self):
        # setting `stylesheet` in 'list2' resets stylesheet_path to None
        self.compare_output(self.files_settings('list', 'list2'),
                            self.expected_settings('list2'))

    def test_error_handler(self):
        # set error_encoding and error_encoding_error_handler (from affix)
        self.compare_output(self.files_settings('error'),
                            self.expected_settings('error'))

    def test_error_handler2(self):
        # second config file only changes encoding, not error_handler:
        self.compare_output(self.files_settings('error', 'error2'),
                            self.expected_settings('error', 'error2'))

class ConfigEnvVarFileTests(ConfigFileTests):

    """
    Repeats the tests of `ConfigFileTests` using the ``DOCUTILSCONFIG``
    environment variable and the standard Docutils config file mechanism.
    """

    def setUp(self):
        ConfigFileTests.setUp(self)
        self.orig_environ = os.environ
        os.environ = os.environ.copy()

    def files_settings(self, *names):
        files = [self.config_files[name] for name in names]
        os.environ['DOCUTILSCONFIG'] = os.pathsep.join(files)
        settings = self.option_parser.get_standard_config_settings()
        return settings.__dict__

    def tearDown(self):
        os.environ = self.orig_environ

class HelperFunctionsTests(unittest.TestCase):

    pathdict = {'foo': 'hallo', 'ham': 'h\xE4m', 'spam': 'spam'}
    keys = ['foo', 'ham']

    def setUp(self):
        self.option_parser = frontend.OptionParser(
            components=(rst.Parser,), read_config_files=None)

    def test_make_paths_absolute(self):
        pathdict = self.pathdict.copy()
        frontend.make_paths_absolute(pathdict, self.keys, base_path='base')
        self.assertEqual(pathdict['foo'], os.path.abspath('base/hallo'))
        self.assertEqual(pathdict['ham'], os.path.abspath('base/h\xE4m'))
        # not touched, because key not in keys:
        self.assertEqual(pathdict['spam'], 'spam')

    def test_make_paths_absolute_cwd(self):
        # With base_path None, the cwd is used as base path.
        # Settings values may-be `unicode` instances, therefore
        # os.getcwdu() is used and the converted path is a unicode instance:
        pathdict = self.pathdict.copy()
        frontend.make_paths_absolute(pathdict, self.keys)
        self.assertEqual(pathdict['foo'], os.path.abspath('hallo'))
        self.assertEqual(pathdict['ham'], os.path.abspath('h\xE4m'))
        # not touched, because key not in keys:
        self.assertEqual(pathdict['spam'], 'spam')

    boolean_settings = (
                (True, True ),
                ('1', True ),
                ('on', True ),
                ('yes', True ),
                ('true', True ),
                ('0', False ),
                ('off', False ),
                ('no', False ),
                ('false', False ),
               )
    def test_validate_boolean(self):
        for t in self.boolean_settings:
            self.assertEqual(
                frontend.validate_boolean(None, t[0], self.option_parser),
                t[1])

    def test_validate_ternary(self):
        tests = (
                 ('500V', '500V'),
                 ('parrot', 'parrot'),
                )
        for t in self.boolean_settings + tests:
            self.assertEqual(
                frontend.validate_ternary(None, t[0], self.option_parser),
                t[1])

    def test_validate_colon_separated_string_list(self):
        tests = (
                    ('a', ['a',] ),
                    ('a:b', ['a', 'b'] ),
                    (['a',], ['a',] ),
                    (['a', 'b:c'], ['a', 'b', 'c'] ),
                )
        for t in tests:
            self.assertEqual(
                    frontend.validate_colon_separated_string_list(None, t[0], None),
                    t[1])

    def test_validate_comma_separated_list(self):
        tests = (
                    ('a', ['a',] ),
                    ('a,b', ['a', 'b'] ),
                    (['a',], ['a',] ),
                    (['a', 'b,c'], ['a', 'b', 'c'] ),
                )
        for t in tests:
            self.assertEqual(
                    frontend.validate_comma_separated_list(None, t[0], None),
                    t[1])

    def test_validate_url_trailing_slash(self):
        tests = (
                    ('', './' ),
                    (None, './' ),
                    ('http://example.org', 'http://example.org/' ),
                    ('http://example.org/', 'http://example.org/' ),
                )
        for t in tests:
            self.assertEqual(
                    frontend.validate_url_trailing_slash(None, t[0], None),
                    t[1])

    def test_validate_smartquotes_locales(self):
        tests = (
                 ('en:ssvv', [('en', 'ssvv')]),
                 ('sd:«»°°', [('sd', '«»°°')]),
                 ([('sd', '«»°°'), 'ds:°°«»'], [('sd', '«»°°'), ('ds', '°°«»')]),
                 ('frs:« : »:((:))', [('frs', ['« ', ' »', '((', '))'])]),
                )
        for t in tests:
            self.assertEqual(
                    frontend.validate_smartquotes_locales(None, t[0], None),
                    t[1])

    def test_set_conditions_deprecation_warning(self):
        reporter = utils.Reporter('test', 1, 4)
        with warnings.catch_warnings(record=True) as wng:
            warnings.simplefilter("always")
            reporter.set_conditions('foo', 1, 4) # trigger warning
            self.assertEqual(len(wng), 1, "Expected a DeprecationWarning.")
            assert issubclass(wng[-1].category, DeprecationWarning)

if __name__ == '__main__':
    unittest.main()
