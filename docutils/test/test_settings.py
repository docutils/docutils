#!/usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests of runtime settings.
"""

import os
import difflib
import warnings
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from docutils import frontend, utils
from docutils.writers import pep_html, html5_polyglot
from docutils.parsers import rst

# DATA_ROOT is ./test/data/ from the docutils root
DATA_ROOT = os.path.abspath(os.path.join(__file__, '..', 'data'))


def fixpath(path):
    return os.path.join(DATA_ROOT, path)


class ConfigFileTests(unittest.TestCase):

    config_files = {'old': fixpath('config_old.rst'),
                    'one': fixpath('config_1.rst'),
                    'two': fixpath('config_2.rst'),
                    'list': fixpath('config_list.rst'),
                    'list2': fixpath('config_list_2.rst'),
                    'error': fixpath('config_encoding.rst'),
                    'error2': fixpath('config_encoding_2.rst'),
                    'syntax_error': fixpath('config_syntax_error.rst'),
                    }

    # expected settings after parsing the equally named config_file:
    settings = {
        'old': {'datestamp': '%Y-%m-%d %H:%M UTC',
                'generator': True,
                'no_random': True,
                'python_home': 'http://www.python.org',
                'source_link': True,
                'stylesheet': None,
                'stylesheet_path': ['stylesheets/pep.css'],
                'template': fixpath('pep-html-template'),
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
                'template': fixpath('pep-html-template'),
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
        warnings.filterwarnings('ignore',
                                category=frontend.ConfigDeprecationWarning)
        warnings.filterwarnings('ignore', category=DeprecationWarning)
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
        self.assertIn('record_dependencies', result)
        rd_result = result.pop('record_dependencies')
        rd_expected = expected.pop('record_dependencies', None)
        if rd_expected is not None:
            self.assertEqual(str(rd_result), str(rd_expected))
        self.assertEqual(expected, result)

    def test_nofiles(self):
        self.compare_output(self.files_settings(),
                            self.expected_settings())

    def test_old(self):
        with self.assertWarnsRegex(FutureWarning,
                                   r'The "\[option\]" section is deprecated.'):
            self.files_settings('old')

    def test_syntax_error(self):
        with self.assertRaisesRegex(
                 ValueError,
                 'Error in config file ".*config_syntax_error.rst", '
                 r'section "\[general\]"'):
            self.files_settings('syntax_error')

    def test_one(self):
        self.compare_output(self.files_settings('one'),
                            self.expected_settings('one'))

    def test_multiple(self):
        self.compare_output(self.files_settings('one', 'two'),
                            self.expected_settings('one', 'two'))

    def test_multiple_with_html5_writer(self):
        # initialize option parser with different component set
        self.option_parser = frontend.OptionParser(
            components=(html5_polyglot.Writer, rst.Parser),
            read_config_files=None)
        # generator setting not changed by "config_2.rst":
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

    def test_encoding_error_handler(self):
        # set error_encoding and error_encoding_error_handler (from affix)
        self.compare_output(self.files_settings('error'),
                            self.expected_settings('error'))

    def test_encoding_error_handler2(self):
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

    def test_old(self):
        pass  # don't repreat this test

    @unittest.skipUnless(
        os.name == 'posix',
        'os.path.expanduser() does not use HOME on Windows (since 3.8)')
    def test_get_standard_config_files(self):
        os.environ['HOME'] = '/home/parrot'
        # TODO: set up mock home directory under Windows
        self.assertEqual(self.option_parser.get_standard_config_files(),
                         ['/etc/docutils.conf',
                          './docutils.conf',
                          '/home/parrot/.docutils'])
        # split at ':', expand leading '~':
        os.environ['DOCUTILSCONFIG'] = ('/etc/docutils2.conf'
                                        ':~/.config/docutils.conf')
        self.assertEqual(self.option_parser.get_standard_config_files(),
                         ['/etc/docutils2.conf',
                          '/home/parrot/.config/docutils.conf'])


class HelperFunctionsTests(unittest.TestCase):

    pathdict = {'foo': 'hallo', 'ham': 'häm', 'spam': 'spam'}
    keys = ['foo', 'ham']

    def setUp(self):
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            self.option_parser = frontend.OptionParser(
                components=(rst.Parser,), read_config_files=None)

    def test_make_paths_absolute(self):
        pathdict = self.pathdict.copy()
        frontend.make_paths_absolute(pathdict, self.keys, base_path='base')
        self.assertEqual(pathdict['foo'], os.path.abspath('base/hallo'))
        self.assertEqual(pathdict['ham'], os.path.abspath('base/häm'))
        # not touched, because key not in keys:
        self.assertEqual(pathdict['spam'], 'spam')

    def test_make_paths_absolute_cwd(self):
        # With base_path None, the cwd is used as base path.
        # Settings values may-be `unicode` instances, therefore
        # os.getcwdu() is used and the converted path is a unicode instance:
        pathdict = self.pathdict.copy()
        frontend.make_paths_absolute(pathdict, self.keys)
        self.assertEqual(pathdict['foo'], os.path.abspath('hallo'))
        self.assertEqual(pathdict['ham'], os.path.abspath('häm'))
        # not touched, because key not in keys:
        self.assertEqual(pathdict['spam'], 'spam')

    boolean_settings = (
                (True, True),
                ('1', True),
                ('on', True),
                ('yes', True),
                ('true', True),
                ('0', False),
                ('off', False),
                ('no', False),
                ('false', False),
               )

    def test_validate_boolean(self):
        for v, result in self.boolean_settings:
            self.assertEqual(frontend.validate_boolean(v), result)

    def test_validate_ternary(self):
        tests = (
                 ('500V', '500V'),
                 ('parrot', 'parrot'),
                )
        for v, result in self.boolean_settings + tests:
            self.assertEqual(frontend.validate_ternary(v), result)

    def test_validate_threshold(self):
        tests = (('1', 1),
                 ('info', 1),
                 ('warning', 2),
                 ('error', 3),
                 ('severe', 4),
                 ('none', 5),
                 )
        for v, result in tests:
            self.assertEqual(
                frontend.validate_threshold(v), result)
        with self.assertRaisesRegex(LookupError, "unknown threshold: 'debug'"):
            frontend.validate_threshold('debug')

    def test_validate_colon_separated_string_list(self):
        tests = (('a', ['a']),
                 ('a:b', ['a', 'b']),
                 (['a'], ['a']),
                 (['a', 'b:c'], ['a', 'b', 'c']),
                 )
        for v, result in tests:
            self.assertEqual(
                frontend.validate_colon_separated_string_list(v), result)

    def test_validate_comma_separated_list(self):
        tests = (('a', ['a']),
                 ('a,b', ['a', 'b']),
                 (['a'], ['a']),
                 (['a', 'b,c'], ['a', 'b', 'c']),
                 )
        for v, result in tests:
            self.assertEqual(frontend.validate_comma_separated_list(v), result)

    def test_validate_math_output(self):
        tests = (('', ()),
                 ('LaTeX ', ('latex', '')),
                 ('MathML', ('mathml', '')),
                 ('MathML  PanDoc', ('mathml', 'pandoc')),
                 ('HTML  math.css, X.css', ('html', 'math.css, X.css')),
                 ('MathJax  /MathJax.js', ('mathjax', '/MathJax.js')),
                 )
        for v, result in tests:
            self.assertEqual(frontend.validate_math_output(v), result)

    def test_validate_math_output_errors(self):
        tests = (('XML', 'Unknown math output format: "XML",\n'
                  "    choose from ('html', 'latex', 'mathml', 'mathjax')."),
                 ('MathML blame', 'MathML converter "blame" not supported,\n'
                  "    choose from ('', 'latexml', 'ttm', 'blahtexml', "
                  "'pandoc')."),
                 )
        for value, message in tests:
            with self.assertRaises(LookupError) as cm:
                frontend.validate_math_output(value)
            self.assertEqual(message, str(cm.exception))

    def test_validate_url_trailing_slash(self):
        tests = (('', './'),
                 (None, './'),
                 ('http://example.org', 'http://example.org/'),
                 ('http://example.org/', 'http://example.org/'),
                 )
        for v, result in tests:
            self.assertEqual(frontend.validate_url_trailing_slash(v), result)

    def test_validate_smartquotes_locales(self):
        tests = (
            ('en:ssvv', [('en', 'ssvv')]),
            ('sd:«»°°', [('sd', '«»°°')]),
            ([('sd', '«»°°'), 'ds:°°«»'], [('sd', '«»°°'), ('ds', '°°«»')]),
            ('frs:« : »:((:))', [('frs', ['« ', ' »', '((', '))'])]),
            )
        for v, result in tests:
            self.assertEqual(frontend.validate_smartquotes_locales(v), result)


if __name__ == '__main__':
    unittest.main()
