# Authors: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
A writer that pickles the document tree to a string.
Later unpickling will allow you to publish with other Writers.
"""

from docutils import writers
import cPickle as pickle


class Writer(writers.Writer):

    supported = ('pickle')
    """Formats this writer supports."""

    config_section = 'pickle writer'
    config_section_dependencies = ('writers',)

    output = None
    """Final translated form of `document`."""

    def translate(self):
        # Remove stuff that cannot be pickled.
        # Note: the method that restores this might need to recreate those
        # structures.
        self.document.transformer = None
        self.document.reporter = None

        # Note: we use the highest protocol, it has some binary in it.
        # - we don't want to return the pickled contents in the output.
        self.pickled = pickle.dumps(self.document)
        
        # Pickle the document to a string.
        self.output = pickled.decode('latin-1')

    def supports(self, format):
        """This writer supports all format-specific elements."""
        return 1
