# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

# Internationalization details are documented in
# <http://docutils.sf.net/docs/howto/i18n.html>.

"""
This package contains modules for language-dependent features of
reStructuredText.
"""

__docformat__ = 'reStructuredText'

import sys

from docutils.languages import LanguageImporter

class RstLanguageImporter(LanguageImporter):
    packages = ('docutils.parsers.rst.languages.', '')
    warn_msg = 'rST localisation for language "%s" not found.'
    fallback = None

    def check_content(self, module):
        """Check if we got an rST language module."""
        if not (isinstance(module.directives, dict)
                and isinstance(module.roles, dict)):
            raise ImportError

get_language = RstLanguageImporter()
"""Return module with language localizations for reStructuredText.

Get translated directive and rolenames from `docutils.parsers.rst.languages`
or the PYTHONPATH.

The argument `language_code` is a "BCP 47" language tag.
If there is no matching module, warn and return None.
"""
