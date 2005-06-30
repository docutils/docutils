#!/usr/bin/env python

# Author: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test the `Publisher` facade and the ``publish_*`` convenience functions.
"""

import unittest
from types import DictType, StringType
from docutils import core, nodes
import pickle


test_document = """\
Test Document
=============

This is a test document.
"""
pseudoxml_output = """\
<document ids="test-document" names="test document" source="<string>" title="Test Document">
    <title>
        Test Document
    <paragraph>
        This is a test document.
"""


class PublishDoctreeTestCase(unittest.TestCase):

    def test_publish_doctree(self):
        """Test `publish_doctree` and `publish_from_doctree`."""

        # Produce the document tree.
        doctree = core.publish_doctree(
            source=test_document,
            reader_name='standalone',
            parser_name='restructuredtext',
            settings_overrides={'_disable_config': 1})
        self.assert_(isinstance(doctree, nodes.document))
        
        # Confirm that transforms have been applied (in this case, the
        # DocTitle transform):
        self.assert_(isinstance(doctree[0], nodes.title))
        self.assert_(isinstance(doctree[1], nodes.paragraph))

        # Write out the document:
        output, parts = core.publish_from_doctree(
            doctree, writer_name='pseudoxml',
            settings_overrides={'_disable_config': 1})
        self.assertEquals(output, pseudoxml_output)
        self.assert_(isinstance(parts, DictType))

    def test_publish_pickle(self):
        """Test publishing a document tree with pickling and unpickling."""
        
        # Produce the document tree.
        doctree = core.publish_doctree(
            source=test_document,
            reader_name='standalone',
            parser_name='restructuredtext',
            settings_overrides={'_disable_config': 1})
        self.assert_(isinstance(doctree, nodes.document))
        
        # Confirm that transforms have been applied (in this case, the
        # DocTitle transform):
        self.assert_(isinstance(doctree[0], nodes.title))
        self.assert_(isinstance(doctree[1], nodes.paragraph))

        # Pickle the document.  Note: if this fails, some unpickleable reference
        # has been added somewhere within the document tree.  If so, you need to
        # fix that.
        #
        # Note: Please do not remove this test, this is an important
        # requirement, applications will be built on the assumption that we can
        # pickle the document.

        # remove the reporter before pickling.
        doctree.reporter = None

        doctree_pickled = pickle.dumps(doctree)
        self.assert_(isinstance(doctree_pickled, StringType))
        del doctree

        # Unpickle the document.
        doctree_zombie = pickle.loads(doctree_pickled)
        self.assert_(isinstance(doctree_zombie, nodes.document))

        # Write out the document:
        output, parts = core.publish_from_doctree(
            doctree_zombie, writer_name='pseudoxml',
            settings_overrides={'_disable_config': 1})
        self.assertEquals(output, pseudoxml_output)
        self.assert_(isinstance(parts, DictType))


if __name__ == '__main__':
    unittest.main()
