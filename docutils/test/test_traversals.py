#! /usr/bin/env python3

# $Id$
# Author: Martin Blais <blais@furius.ca>
# Copyright: This module has been placed in the public domain.

"""
Test module for traversals.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import docutils
from docutils import core, nodes, writers


stop_traversal_input = '''
==================
   Train Travel
==================

Happily, happily going by train.

.. attention:: Attention, attention.  This is a public annoucement.
               You must get off the train now.

KaZoom! Train crashes.

- Told ya!!!  Get off the train next time.

'''


class AttentiveVisitor(nodes.SparseNodeVisitor):

    def visit_attention(self, node):
        raise nodes.StopTraversal

    def visit_bullet_list(self, node):
        raise RuntimeError("It's too late for attention, "
                           "more discipline is needed!.")


class AttentiveWriter(writers.Writer):

    def translate(self):
        self.visitor = visitor = AttentiveVisitor(self.document)

        # Test both kinds of traversals.
        self.document.walkabout(visitor)
        self.document.walk(visitor)


class StopTraversalTests(unittest.TestCase, docutils.SettingsSpec):

    """
    Test interrupting the visitor during traversal.  In this test we stop it
    when we reach an attention node.
    """
    def test_stop_traversal(self):
        # Load some document tree in memory.
        doctree = core.publish_doctree(
            source=stop_traversal_input,
            reader='standalone',
            parser='restructuredtext',
            settings_spec=self)
        self.assertTrue(isinstance(doctree, nodes.document))

        core.publish_parts(
            reader='doctree', source_class=docutils.io.DocTreeInput,
            source=doctree, source_path='test',
            writer=AttentiveWriter())


if __name__ == '__main__':
    unittest.main()
