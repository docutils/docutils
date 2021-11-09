# $Id: html.py 8347 2019-08-26 12:12:02Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Dummy module for backwards compatibility.

This module is provisional: it will be removed in Docutils version 1.2.
"""

__docformat__ = 'reStructuredText'

import warnings

from docutils.parsers.rst.directives.misc import MetaBody, Meta

warnings.warn('The `docutils.parsers.rst.directive.htmp` module'
              ' will be removed in Docutils 1.2.'
              ' Since Docutils 0.18, the "Meta" node is defined in'
              ' `docutils.parsers.rst.directives.misc`.',
              DeprecationWarning, stacklevel=2)
