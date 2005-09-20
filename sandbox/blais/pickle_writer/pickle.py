# Authors: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A writer that pickles the document tree to a binary string.
Later unpickling will allow you to publish with other Writers.
"""

from docutils import writers
import cPickle as pickle


class Writer(writers.UnfilteredWriter):

    supported = ('pickle')
    """Formats this writer supports."""

    config_section = 'pickle writer'
    config_section_dependencies = ('writers',)

    output = None
    """Final translated form of `document`."""

    def translate(self):
        # Remove stuff that cannot be pickled:
        self.document.transformer = None
        self.document.reporter = None
        # Note: we use the highest protocol, it has some binary in it:
        self.output = pickle.dumps(self.document)
