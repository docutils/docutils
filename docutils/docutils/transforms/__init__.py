#! /usr/bin/env python
"""
:Authors: David Goodger, Ueli Schlaepfer
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This package contains modules for standard tree transforms available
to Docutils components. Tree transforms serve a variety of purposes:

- To tie up certain syntax-specific "loose ends" that remain after the
  initial parsing of the input plaintext. These transforms are used to
  supplement a limited syntax.

- To automate the internal linking of the document tree (hyperlink
  references, footnote references, etc.).

- To extract useful information from the document tree. These
  transforms may be used to construct (for example) indexes and tables
  of contents.

Each transform is an optional step that a Docutils Reader may choose to
perform on the parsed document, depending on the input context. A Docutils
Reader may also perform Reader-specific transforms before or after performing
these standard transforms.
"""

__docformat__ = 'reStructuredText'


from docutils import languages


class TransformError(Exception): pass


class Transform:

    """
    Docutils transform component abstract base class.
    """

    def __init__(self, doctree, startnode=None):
        """
        Initial setup for in-place document transforms.
        """

        self.doctree = doctree
        """The document tree to transform."""

        self.startnode = startnode
        """Node from which to begin the transform.  For many transforms which
        apply to the document as a whole, `startnode` is not set (i.e. its
        value is `None`)."""

        self.language = languages.getlanguage(doctree.languagecode)
        """Language module local to this document."""

    def transform(self):
        """Override to transform the document tree."""
        raise NotImplementedError('subclass must override this method')
