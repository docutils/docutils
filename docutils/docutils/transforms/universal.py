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

import re, sys
from docutils import nodes, utils
from docutils.transforms import TransformError, Transform


class Messages(Transform):

    """
    Place any system messages generated after parsing into a dedicated section
    of the document.
    """

    def transform(self):
        unfiltered = self.document.messages.get_children()
        threshold = self.document.reporter['writer'].warning_level
        messages = []
        for msg in unfiltered:
            if msg['level'] >= threshold:
                messages.append(msg)
        if len(messages) > 0:
            section = nodes.section(CLASS='system-messages')
            # @@@ get this from the language module?
            section += nodes.title('', 'Docutils System Messages')
            section += messages
            self.document.messages[:] = []
            self.document += section


class TestMessages(Transform):

    """
    Append all system messages to the end of the document.
    """

    def transform(self):
        self.document += self.document.messages.get_children()


class FinalChecks(Transform):

    """
    Perform last-minute checks.

    - Check for dangling references (incl. footnote & citation).
    """

    def transform(self):
        visitor = FinalCheckVisitor(self.document)
        self.document.walk(visitor)


class FinalCheckVisitor(nodes.SparseNodeVisitor):

    def unknown_visit(self, node):
        pass

    def visit_reference(self, node):
        if node.resolved or not node.hasattr('refname'):
            return
        refname = node['refname']
        try:
            id = self.document.nameids[refname]
        except KeyError:
            msg = self.document.reporter.error(
                  'Unknown target name: "%s".' % (node['refname']))
            self.document.messages += msg
            msgid = self.document.set_id(msg)
            prb = nodes.problematic(
                  node.rawsource, node.rawsource, refid=msgid)
            prbid = self.document.set_id(prb)
            msg.add_backref(prbid)
            node.parent.replace(node, prb)
            return
        del node['refname']
        node['refid'] = id
        self.document.ids[id].referenced = 1
        node.resolved = 1

    visit_footnote_reference = visit_citation_reference = visit_reference


class Pending(Transform):

    """
    Base class for the execution of pending transforms.

    `nodes.pending` element objects each contain a "stage" attribute; the
    stage of the pending element must match the `stage` of this transform.
    """

    stage = None
    """The stage of processing applicable to this transform; match with
    `nodes.pending.stage`.  Possible values include 'first reader',
    'last reader', 'first writer', and 'last writer'.  Overriden in
    subclasses (below)."""

    def transform(self):
        for pending in self.document.pending:
            if pending.stage == self.stage:
                pending.transform(self.document, self.component,
                                  pending).transform()


class FirstReaderPending(Pending):

    stage = 'first reader'


class LastReaderPending(Pending):

    stage = 'last reader'


class FirstWriterPending(Pending):

    stage = 'first writer'


class LastWriterPending(Pending):

    stage = 'last writer'


test_transforms = (TestMessages,)
"""Universal transforms to apply to the raw document when testing."""

first_reader_transforms = (FirstReaderPending,)
"""Universal transforms to apply before any other Reader transforms."""

last_reader_transforms = (LastReaderPending,)
"""Universal transforms to apply after all other Reader transforms."""

first_writer_transforms = (FirstWriterPending,)
"""Universal transforms to apply before any other Writer transforms."""

last_writer_transforms = (LastWriterPending, FinalChecks, Messages)
"""Universal transforms to apply after all other Writer transforms."""
