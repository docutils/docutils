#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'


class Parser:

    def parse(self, inputstring, docroot):
        """Override to parse `inputstring` into document tree `docroot`."""
        raise NotImplementedError('subclass must override this method')

    def setup_parse(self, inputstring, docroot):
        """Initial setup, used by `parse()`."""
        self.inputstring = inputstring
        self.docroot = docroot


_parser_aliases = {
      'restructuredtext': 'rst',
      'rest': 'rst',
      'rtxt': 'rst',}

def get_parser_class(parsername):
    """Return the Parser class from the `parsername` module."""
    parsername = parsername.lower()
    if _parser_aliases.has_key(parsername):
        parsername = _parser_aliases[parsername]
    module = __import__(parsername, globals(), locals())
    return module.Parser
