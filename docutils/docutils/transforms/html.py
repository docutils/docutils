# Author: David Goodger
# Contact: goodger@python.org
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Transforms specific to the HTML writer.
"""

__docformat__ = 'reStructuredText'

from docutils import nodes, utils
from docutils.transforms import Transform, TransformError


class StylesheetCheck(Transform):

    """Check for a proper stylesheet setting."""

    default_priority = 420

    def apply(self):
        if ( self.document.settings._stylesheet_required and
             utils.get_stylesheet_reference(self.document.settings) is None):
            self.document.reporter.warning(
                'No stylesheet path or URI given.  Use the --stylesheet '
                'or --stylesheet-path option to specify the location of '
                'default.css (in the tools/stylesheets/ directory of the '
                'Docutils distribution).')
