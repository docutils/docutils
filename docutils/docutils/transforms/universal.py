#! /usr/bin/env python
"""
:Authors: David Goodger, Ueli Schlaepfer
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Transforms needed by most or all documents:

- `Messages`: Placement of system messages stored in
  `nodes.document.messages`.
- `TestMessages`: Like `Messages`, used on test runs.
- `FinalReferences`: Resolve remaining references.
- `Pending`: Execute pending transforms (abstract base class;
  `FirstReaderPending`, `LastReaderPending`, `FirstWriterPending`, and
  `LastWriterPending` are its concrete subclasses).
"""

__docformat__ = 'reStructuredText'

import re
from docutils import nodes, utils
from docutils.transforms import TransformError, Transform


class Messages(Transform):

    """
    Place any system messages generated after parsing into a dedicated section
    of the document.
    """

    def transform(self):
        # @@@ filter out msgs below threshold?
        if len(self.doctree.messages) > 0:
            section = nodes.section(CLASS='system-messages')
            # @@@ get this from the language module?
            section += nodes.title('', 'Docutils System Messages')
            section += self.doctree.messages.getchildren()
            self.doctree.messages[:] = []
            self.doctree += section


class TestMessages(Transform):

    """
    Append all system messages to the end of the doctree.
    """

    def transform(self):
        self.doctree += self.doctree.messages.getchildren()


class FinalChecks(Transform):

    """
    Perform last-minute checks.

    - Check for dangling references (incl. footnote & citation).
    """

    def transform(self):
        visitor = FinalCheckVisitor(self.doctree)
        self.doctree.walk(visitor)


class FinalCheckVisitor(nodes.NodeVisitor):

    def unknown_visit(self, node):
        pass

    def visit_reference(self, node):
        if node.resolved or not node.hasattr('refname'):
            return
        refname = node['refname']
        try:
            id = self.doctree.nameids[refname]
        except KeyError:
            msg = self.doctree.reporter.error(
                  'Unknown target name: "%s".' % (node['refname']))
            self.doctree.messages += msg
            msgid = self.doctree.set_id(msg)
            prb = nodes.problematic(
                  node.rawsource, node.rawsource, refid=msgid)
            prbid = self.doctree.set_id(prb)
            msg.add_backref(prbid)
            node.parent.replace(node, prb)
            return
        del node['refname']
        node['refid'] = id
        self.doctree.ids[id].referenced = 1
        node.resolved = 1

    visit_footnote_reference = visit_citation_reference = visit_reference


class Pending(Transform):

    """
    Execute pending transforms.
    """

    stage = None
    """The stage of processing applicable to this transform; match with
    `nodes.pending.stage`.  Possible values include 'first_reader',
    'last_reader', 'first_writer', and 'last_writer'.  Override in
    subclasses."""

    def transform(self):
        for pending in self.doctree.pending:
            if pending.stage == self.stage:
                pending.transform(self.doctree, pending).transform()


class FirstReaderPending(Pending):

    stage = 'first_reader'


class LastReaderPending(Pending):

    stage = 'last_reader'


class FirstWriterPending(Pending):

    stage = 'first_writer'


class LastWriterPending(Pending):

    stage = 'last_writer'


test_transforms = (TestMessages,)
"""Universal transforms to apply to the raw doctree when testing."""

first_reader_transforms = (FirstReaderPending,)
"""Universal transforms to apply before any other Reader transforms."""

last_reader_transforms = (LastReaderPending,)
"""Universal transforms to apply after all other Reader transforms."""

first_writer_transforms = (FirstWriterPending,)
"""Universal transforms to apply before any other Writer transforms."""

last_writer_transforms = (LastWriterPending, FinalChecks, Messages)
"""Universal transforms to apply after all other Writer transforms."""
