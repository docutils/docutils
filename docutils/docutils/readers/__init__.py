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
from docutils import nodes, utils
from docutils.transforms import universal


class Reader:

    """
    Abstract base class for docutils Readers.

    Each reader module or package must export a subclass also called 'Reader'.

    The three steps of a Reader's responsibility are defined: `scan()`,
    `parse()`, and `transform()`. Call `read()` to process a document.
    """

    transforms = ()
    """Ordered tuple of transform classes (each with a ``transform()`` method).
    Populated by subclasses. `Reader.transform()` instantiates & runs them."""

    def __init__(self, reporter, languagecode):
        """
        Initialize the Reader instance.

        Several instance attributes are defined with dummy initial values.
        Subclasses may use these attributes as they wish.
        """

        self.languagecode = languagecode
        """Default language for new documents."""

        self.reporter = reporter
        """A `utils.Reporter` instance shared by all doctrees."""

        self.source = None
        """Path to the source of raw input."""

        self.input = None
        """Raw text input; either a single string or, for more complex cases,
        a collection of strings."""

        self.transforms = tuple(self.transforms)
        """Instance copy of `Reader.transforms`; may be modified by client."""

    def read(self, source, parser):
        self.source = source
        self.parser = parser
        self.scan()               # may modify self.parser, depending on input
        self.parse()
        self.transform()
        return self.document

    def scan(self):
        """Override to read `self.input` from `self.source`."""
        raise NotImplementedError('subclass must override this method')

    def scanfile(self, source):
        """
        Scan a single file and return the raw data.

        Parameter `source` may be:

        (a) a file-like object, which is read directly;
        (b) a path to a file, which is opened and then read; or
        (c) `None`, which implies `sys.stdin`.
        """
        if hasattr(source, 'read'):
            return source.read()
        if self.source:
            return open(source).read()
        return sys.stdin.read()

    def parse(self):
        """Parse `self.input` into a document tree."""
        self.document = self.newdocument()
        self.parser.parse(self.input, self.document)

    def transform(self):
        """Run all of the transforms defined for this Reader."""
        for xclass in (universal.first_reader_transforms
                       + tuple(self.transforms)
                       + universal.last_reader_transforms):
            xclass(self.document).transform()

    def newdocument(self, languagecode=None):
        """Create and return a new empty document tree (root node)."""
        document = nodes.document(
              languagecode=(languagecode or self.languagecode),
              reporter=self.reporter)
        document['source'] = self.source
        return document


_reader_aliases = {'rtxt': 'standalone',
                   'restructuredtext': 'standalone'}

def get_reader_class(readername):
    """Return the Reader class from the `readername` module."""
    readername = readername.lower()
    if _reader_aliases.has_key(readername):
        readername = _reader_aliases[readername]
    module = __import__(readername, globals(), locals())
    return module.Reader
