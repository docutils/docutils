# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
This package contains the Python Source Reader modules.
"""

__docformat__ = 'reStructuredText'


import sys
import docutils.readers
from docutils.readers.python import moduleparser


class Reader(docutils.readers.Reader):

    config_section = 'python reader'
    config_section_dependencies = ('readers',)

    def parse(self):
        """Parse `self.input` into a document tree."""
        self.document = document = moduleparser.parse_module(self.input)
        document.current_source = document.current_line = None
