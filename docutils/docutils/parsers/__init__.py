#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'

from docutils import Component


class Parser(Component):

    def parse(self, inputstring, document):
        """Override to parse `inputstring` into document tree `document`."""
        raise NotImplementedError('subclass must override this method')

    def setup_parse(self, inputstring, document):
        """Initial setup, used by `parse()`."""
        self.inputstring = inputstring
        self.document = document


_parser_aliases = {
      'restructuredtext': 'rst',
      'rest': 'rst',
      'restx': 'rst',
      'rtxt': 'rst',}

def get_parser_class(parser_name):
    """Return the Parser class from the `parser_name` module."""
    parser_name = parser_name.lower()
    if _parser_aliases.has_key(parser_name):
        parser_name = _parser_aliases[parser_name]
    module = __import__(parser_name, globals(), locals())
    return module.Parser
