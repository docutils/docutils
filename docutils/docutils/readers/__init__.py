#! /usr/bin/env python

"""
:Authors: David Goodger; Ueli Schlaepfer
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This package contains Docutils Reader modules.
"""

__docformat__ = 'reStructuredText'


import sys
from docutils import utils, parsers, Component
from docutils.transforms import universal


class Reader(Component):

    """
    Abstract base class for docutils Readers.

    Each reader module or package must export a subclass also called 'Reader'.

    The three steps of a Reader's responsibility are defined: `scan()`,
    `parse()`, and `transform()`. Call `read()` to process a document.
    """

    transforms = ()
    """Ordered tuple of transform classes (each with a ``transform()`` method).
    Populated by subclasses. `Reader.transform()` instantiates & runs them."""

    def __init__(self, parser, parser_name):
        """
        Initialize the Reader instance.

        Several instance attributes are defined with dummy initial values.
        Subclasses may use these attributes as they wish.
        """

        self.parser = parser
        """A `parsers.Parser` instance shared by all doctrees.  May be left
        unspecified if the document source determines the parser."""

        if parser is None and parser_name:
            self.set_parser(parser_name)

        self.source = None
        """`docutils.io` IO object, source of input data."""

        self.input = None
        """Raw text input; either a single string or, for more complex cases,
        a collection of strings."""

    def set_parser(self, parser_name):
        """Set `self.parser` by name."""
        parser_class = parsers.get_parser_class(parser_name)
        self.parser = parser_class()

    def read(self, source, parser, options):
        self.source = source
        if not self.parser:
            self.parser = parser
        self.options = options
        # May modify self.parser, depending on input:
        self.input = self.source.read(self)
        self.parse()
        self.transform()
        return self.document

    def parse(self):
        """Parse `self.input` into a document tree."""
        self.document = self.new_document()
        self.parser.parse(self.input, self.document)

    def transform(self):
        """Run all of the transforms defined for this Reader."""
        for xclass in (universal.first_reader_transforms
                       + tuple(self.transforms)
                       + universal.last_reader_transforms):
            xclass(self.document, self).transform()

    def new_document(self):
        """Create and return a new empty document tree (root node)."""
        document = utils.new_document(self.options)
        if self.source.source_path:
            document['source'] = self.source.source_path
        return document


_reader_aliases = {
      'rst': 'standalone',
      'rest': 'standalone',
      'restx': 'standalone',
      'rtxt': 'standalone',
      'restructuredtext': 'standalone'}

def get_reader_class(reader_name):
    """Return the Reader class from the `reader_name` module."""
    reader_name = reader_name.lower()
    if _reader_aliases.has_key(reader_name):
        reader_name = _reader_aliases[reader_name]
    module = __import__(reader_name, globals(), locals())
    return module.Reader
