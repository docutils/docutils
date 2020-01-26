# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

# Internationalization details are documented in
# <http://docutils.sf.net/docs/howto/i18n.html>.

"""
This package contains modules for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

import sys
from importlib import import_module

from docutils.utils import normalize_language_tag

class LanguageImporter(object):

    packages = ('docutils.languages.', '')
    warn_msg = ('Language "%s" not supported: '
                'Docutils-generated text will be in English.')
    fallback = 'en'
    # TODO: use a dummy module returning emtpy strings?, configurable?

    def __init__(self):
        self.cache = {}

    def import_from_packages(self, name):
        """Try loading module `name` from `self.packages`."""
        try:
            return self.cache[name]
        except KeyError:
            pass
        for package in self.packages:
            try:
                module = import_module(package+name)
                self.check_content(module)
            except (ImportError, AttributeError, AssertionError):
                continue
            self.cache[name] = module
            return module
        return None

    def check_content(self, module):
        """Check if we got a Docutils language module."""
        assert isinstance(module.labels, dict)
        assert isinstance(module.bibliographic_fields, dict)
        assert isinstance(module.author_separators, list)

    def __call__(self, language_code, reporter=None):
        try:
            return self.cache[language_code]
        except KeyError:
            pass
        for tag in normalize_language_tag(language_code):
            tag = tag.replace('-', '_') # '-' not valid in module names
            module = self.import_from_packages(tag)
            if module is not None:
                return module
        if reporter is not None:
            reporter.warning(self.warn_msg % language_code)
        module = self.import_from_packages('en')
        self.cache[tag] = module # warn only one time!
        return module


get_language = LanguageImporter()
"""Return module with language localizations

from `docutils.languages` or the PYTHONPATH.

The argument `language_code` is a "BCP 47" language tag.
If there is no matching module, warn and fall back to English.
"""
