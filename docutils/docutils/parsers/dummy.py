# Author: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""Dummy parser that does nothing."""

from docutils import parsers

class Parser(parsers.Parser):

    """Dummy parser that does nothing."""

    supported = ('dummy',)

    config_section = 'dummy parser'
    config_section_dependencies = ('parsers',)

    def parse(self, inputstring, document):
        pass
