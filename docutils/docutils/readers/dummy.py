# Author: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""Dummy reader that is initialized with an existing document tree."""

from docutils import readers, utils

class Reader(readers.Reader):

    """Dummy reader that is initialized with an existing document tree."""

    supported = ('dummy',)

    def __init__(self, doctree):
        readers.Reader.__init__(self)
        self.document = doctree

    def read(self, source, parser, settings):
        # Useful for document serialization, where the reporter is destroyed.
        if self.document.reporter is None:
            self.document.reporter = utils.new_reporter(
                source.source_path, settings)
        # Override document settings with new settings.
        self.document.settings = settings
        # Return document tree.  No parsing necessary.
        return self.document
