#! /usr/bin/env python

"""
:Authors: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Simple internal document tree Writer, writes Docutils XML.
"""

__docformat__ = 'reStructuredText'


from docutils import writers


class Writer(writers.Writer):

    supported = ('xml',)
    """Formats this writer supports."""

    cmdline_options = (
        '"Docutils XML" Writer Options',
        'Warning: these options may adversely affect whitespace; use them '
        'only for reading convenience.',
        (('Generate XML with newlines before and after tags.',
          ['--newlines'], {'action': 'store_true'}),
         ('Generate XML with indents and newlines.',
          ['--indents'], {'action': 'store_true'}),),)

    output = None
    """Final translated form of `document`."""

    def translate(self):
        indent = newline = ''
        if self.document.options.newlines:
            newline = '\n'
        if self.document.options.indents:
            newline = '\n'
            indent = '    '
        self.output = self.document.asdom().toprettyxml(indent, newline)
