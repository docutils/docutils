# Author: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""Reader for existing document trees."""

from docutils import readers, utils


class Reader(readers.Reader):

    """
    Adapt the Reader API for an existing document tree.

    The existing document tree must be passed as the ``source`` parameter to
    the `docutils.core.Publisher` initializer, wrapped in a
    `docutils.io.DoctreeInput` object::

        pub = docutils.core.Publisher(
            ..., source=docutils.io.DoctreeInput(document), ...)

    The original document settings are overridden; if you want to use the
    settings of the original document, pass ``settings=document.settings`` to
    the Publisher call above.
    """

    supported = ('doctree',)

    config_section = 'doctree reader'
    config_section_dependencies = ('readers',)

    def parse(self):
        """
        No parsing to do; refurbish the document tree instead.
        Overrides the inherited method.
        """
        self.document = self.input
        # Restore the reporter after document serialization:
        if self.document.reporter is None:
            self.document.reporter = utils.new_reporter(
                self.source.source_path, self.settings) 
        # Override document settings with new settings:
        self.document.settings = self.settings
