# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

# Internationalization details are documented in
# <http://docutils.sf.net/docs/howto/i18n.html>.

"""
This package contains modules for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

from docutils.utils import normalize_language_tag

_languages = {}

def get_language(language_code):
    for tag in normalize_language_tag(language_code):
        if tag in _languages:
            return _languages[tag]
        try:
            module = __import__(tag, globals(), locals())
        except ImportError:
            continue
        _languages[tag] = module
        return module
    # TODO: use Docutils reporter, warn instead of fail
    # warn ('language "%s" not supported' % tag)
    raise
