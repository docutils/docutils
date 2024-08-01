#!/usr/bin/env python3

# $Id$
# Authors: Engelbert Gruber <grubert@users.sourceforge.net>;
#          David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for language module completeness.

Specify a language code (e.g. "de") as a command-line parameter to test only
that language.
"""

from pathlib import Path
import os
import re
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))


from docutils import frontend, languages, utils
from docutils.parsers.rst import languages as rst_languages
from docutils.parsers.rst.directives import _directive_registry
from docutils.parsers.rst.roles import _role_registry

LANGUAGE_MODULE_PATTERN = re.compile(r'^([a-z]{2,3}(_[a-z]{2,8})*)\.py$')
REPORTER = utils.new_reporter('', frontend.get_default_settings())
REF = languages.get_language('en', REPORTER)


def get_languages():
    """
    Get installed language translations from docutils.languages and from
    docutils.parsers.rst.languages.
    """
    translations = set()
    for mod in (os.listdir(languages.__path__[0])
                + os.listdir(rst_languages.__path__[0])):
        match = LANGUAGE_MODULE_PATTERN.match(mod)
        if match:
            translations.add(match.group(1))
    language_list = list(translations)
    # test language tag normalization:
    language_list += ['en_gb', 'en_US', 'en-CA', 'de-DE', 'de-AT-1901',
                      'pt-BR', 'pt-foo-BR']
    # test that locally created language files are also loaded.
    # requires local_dummy_lang.py in test directory (testroot)
    # The local_dummy_lang.py contains all the fields from both
    # the docutils language tags and the parser.rst language tags
    language_list += ['test.local_dummy_lang']
    return language_list


class LanguageTestCase(unittest.TestCase):
    maxDiff = None

    def test_labels(self):
        ref_labels = {*REF.labels}
        for language in get_languages():
            with self.subTest(id=f'{language}.py'):
                try:
                    module = languages.get_language(language, REPORTER)
                except ImportError:
                    mod = f'docutils.languages.{language}'
                    raise AssertionError(f'No {mod} module.')
                self.assertIsNotNone(module)
                self.assertSetEqual({*module.labels}, ref_labels)

    def test_bibliographic_fields(self):
        ref_fields = {*REF.bibliographic_fields.values()}
        for language in get_languages():
            with self.subTest(id=f'{language}.py'):
                try:
                    module = languages.get_language(language, REPORTER)
                except ImportError:
                    mod = f'docutils.languages.{language}'
                    raise AssertionError(f'No {mod} module.')
                self.assertIsNotNone(module)
                mod_fields = {*module.bibliographic_fields.values()}
                self.assertSetEqual(mod_fields, ref_fields)

    def test_directives(self):
        for language in get_languages():
            with self.subTest(id=f'{language}.py'):
                try:
                    module = rst_languages.get_language(language)
                except ImportError:
                    mod = f'docutils.parsers.rst.languages.{language}'
                    raise AssertionError(f'No {mod} module.')
                self.assertIsNotNone(module)
                for d in module.directives.values():
                    self.assertIn(d, _directive_registry)
                for name in _directive_registry:
                    if name != 'restructuredtext-test-directive':
                        self.assertIn(name, set(module.directives.values()))

    def test_roles(self):
        for language in get_languages():
            with self.subTest(id=f'{language}.py'):
                try:
                    module = rst_languages.get_language(language)
                except ImportError:
                    mod = f'docutils.parsers.rst.languages.{language}'
                    raise AssertionError(f'No {mod} module.')
                self.assertIsNotNone(module)
                for d in module.roles.values():
                    self.assertIn(d, _role_registry)
                for name in _role_registry:
                    if name != 'restructuredtext-unimplemented-role':
                        self.assertIn(name, set(module.roles.values()))


if __name__ == '__main__':
    unittest.main()
