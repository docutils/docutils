#!/usr/bin/env python

# Author: David Goodger
# Contact: goodger@python.org
# Revision:  $Revision$
# Date:      $Date$
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
from docutils import frontend
from docutils.writers import html4css1
from docutils.writers import pep_html


warnings.filterwarnings(action='ignore',
                        category=frontend.ConfigDeprecationWarning)

def fixpath(path):
    return os.path.abspath(os.path.join(mydir, path))

mydir = os.path.dirname(fixpath.func_code.co_filename)


class ConfigFileTests(unittest.TestCase):

    config_files = {'old': fixpath('data/config_old.txt'),
                    'one': fixpath('data/config_1.txt'),
                    'two': fixpath('data/config_2.txt'),
                    'list': fixpath('data/config_list.txt'),
                    'error': fixpath('data/config_error_handler.txt')}

    settings = {
        'old': {'datestamp': '%Y-%m-%d %H:%M UTC',
                'generator': 1,
                'no_random': 1,
                'python_home': 'http://www.python.org',
                'source_link': 1,
                'stylesheet_path': fixpath('data/stylesheets/pep.css'),
                'template': fixpath('data/pep-html-template')},
        'one': {'datestamp': '%Y-%m-%d %H:%M UTC',
                'generator': 1,
                'no_random': 1,
                'python_home': 'http://www.python.org',
                'source_link': 1,
                'stylesheet_path': fixpath('data/stylesheets/pep.css'),
                'template': fixpath('data/pep-html-template')},
        'two': {'generator': 0,
                'stylesheet_path': fixpath('data/test.css')},
        'list': {'expose_internals': ['a', 'b', 'c', 'd', 'e']},
        'error': {'error_encoding': 'ascii',
                  'error_encoding_error_handler': 'strict'},
        }

    compare = difflib.Differ().compare
    """Comparison method shared by all tests."""

    def setUp(self):
        self.option_parser = frontend.OptionParser(
            components=(pep_html.Writer,), read_config_files=None)

    def compare_output(self, result, expected):
        """`result` and `expected` should both be dicts."""
        result = pprint.pformat(result)
        expected = pprint.pformat(expected)
        try:
            self.assertEquals('\n' + result, '\n' + expected)
        except AssertionError:
            print >>sys.stderr, '\n%s\n' % (self,)
            print >>sys.stderr, '-: expected\n+: result'
            print >>sys.stderr, ''.join(self.compare(expected.splitlines(1),
                                                     result.splitlines(1)))
            raise

    def test_old(self):
        settings = self.option_parser.get_config_file_settings(
            self.config_files['old'])
        self.compare_output(settings, self.settings['old'])

    def test_one(self):
        settings = self.option_parser.get_config_file_settings(
            self.config_files['one'])
        self.compare_output(settings, self.settings['one'])

    def test_multiple(self):
        settings = self.option_parser.get_config_file_settings(
            self.config_files['one'])
        settings.update(self.option_parser.get_config_file_settings(
            self.config_files['two']))
        expected = self.settings['one'].copy()
        expected.update(self.settings['two'])
        self.compare_output(settings, expected)

    def test_old_and_new(self):
        settings = self.option_parser.get_config_file_settings(
            self.config_files['one'])
        settings.update(self.option_parser.get_config_file_settings(
            self.config_files['two']))
        expected = self.settings['old'].copy()
        expected.update(self.settings['two'])
        self.compare_output(settings, expected)

    def test_list(self):
        settings = self.option_parser.get_config_file_settings(
            self.config_files['list'])
        self.compare_output(settings, self.settings['list'])

    def test_error_handler(self):
        settings = self.option_parser.get_config_file_settings(
            self.config_files['error'])
        self.compare_output(settings, self.settings['error'])


if __name__ == '__main__':
    unittest.main()
