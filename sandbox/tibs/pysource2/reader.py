"""reader.py - docutils Reader for Python source code

:Author:    Tibs
:Contact:   tibs@tibsnjoan.co.uk
:Revision:  $Revision$
:Date:      $Date$
:Copyright: This module has been placed in the public domain.
"""

__docformat__ = 'reStructuredText'

import docutils.readers
from docutils.readers.python.moduleparser import Node, parse_module
#from package import parse_package
from transform import make_document

class Reader(docutils.readers.Reader):
    """A Python source code specific Reader.
    """

    config_section = 'python reader'
    config_section_dependencies = ('readers',)

    def parse(self):
        """Parse `self.input` into a document tree."""

        tree = parse_module(self.input,self.source.source_path)
        self.document = document = make_document(tree)
        #self.document = document = self.new_document()
        #self.parser.parse(self.input, document)
        document.current_source = document.current_line = None
