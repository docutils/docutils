#! /usr/bin/env python

"""
:Authors: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This package contains Docutils Writer modules.
"""

__docformat__ = 'reStructuredText'


import sys
import docutils
from docutils import languages, Component
from docutils.transforms import universal


class Writer(Component):

    """
    Abstract base class for docutils Writers.

    Each writer module or package must export a subclass also called 'Writer'.
    Each writer must support all standard node types listed in
    `docutils.nodes.node_class_names`.

    Call `write()` to process a document.
    """

    document = None
    """The document to write."""

    language = None
    """Language module for the document."""

    destination = None
    """Where to write the document."""

    transforms = ()
    """Ordered list of transform classes (each with a ``transform()`` method).
    Populated by subclasses. `Writer.transform()` instantiates & runs them."""

    def __init__(self):
        """Initialize the Writer instance."""

        self.transforms = list(self.transforms)
        """Instance copy of `Writer.transforms`; may be modified by client."""

    def write(self, document, destination):
        self.document = document
        self.language = languages.get_language(document.language_code)
        self.destination = destination
        self.transform()
        self.translate()
        self.record()

    def transform(self):
        """Run all of the transforms defined for this Writer."""
        for xclass in (universal.first_writer_transforms
                       + tuple(self.transforms)
                       + universal.last_writer_transforms):
            xclass(self.document, self).transform()

    def translate(self):
        """
        Override to do final document tree translation.

        This is usually done with a `docutils.nodes.NodeVisitor` subclass, in
        combination with a call to `docutils.nodes.Node.walk()` or
        `docutils.nodes.Node.walkabout()`.  The ``NodeVisitor`` subclass must
        support all standard elements (listed in
        `docutils.nodes.node_class_names`) and possibly non-standard elements
        used by the current Reader as well.
        """
        raise NotImplementedError('subclass must override this method')

    def record(self):
        """Override to record `document` to `destination`."""
        raise NotImplementedError('subclass must override this method')

    def recordfile(self, output, destination):
        """
        Write `output` to a single file.

        Parameters:

        - `output`: Data to write.
        - `destination`: one of:

          (a) a file-like object, which is written directly;
          (b) a path to a file, which is opened and then written; or
          (c) `None`, which implies `sys.stdout`.
        """
        output = output.encode('utf-8') # @@@ temporary; must not hard-code
        if hasattr(self.destination, 'write'):
            destination.write(output)
        elif self.destination:
            open(self.destination, 'w').write(output)
        else:
            sys.stdout.write(output)


_writer_aliases = {
      'html': 'html4css1',
      'pprint': 'pseudoxml',
      'pformat': 'pseudoxml',
      'pdf': 'rlpdf',}

def get_writer_class(writer_name):
    """Return the Writer class from the `writer_name` module."""
    writer_name = writer_name.lower()
    if _writer_aliases.has_key(writer_name):
        writer_name = _writer_aliases[writer_name]
    module = __import__(writer_name, globals(), locals())
    return module.Writer
