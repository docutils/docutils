#!/usr/bin/env python

# Author: Martin Blais
# Contact: blais@furius.ca
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Perform tests with publishing to a tree, and running a writer on that tree later.
"""

import unittest
from types import DictType, StringType
import docutils.core
import docutils.nodes


test_document = """\
Test Document
=============

This is a test document.
"""


class PublishDoctreeTestCase(unittest.TestCase):

    def test_publish_doctree(self):
        """Test publish_doctree and publish_from_doctree."""
        # Produce the document tree.
        doctree, parts = docutils.core.publish_doctree(
            source=test_document,
            reader_name='standalone',
            parser_name='restructuredtext',
            settings_overrides={'_disable_config': 1})

        self.assert_(isinstance(doctree, docutils.nodes.document))
        self.assert_(isinstance(parts, DictType))

        # Write out the document.
        output, parts = docutils.core.publish_from_doctree(
            doctree, writer_name='pseudoxml')

        self.assert_(isinstance(output, StringType))
        assert isinstance(parts, dict)


if __name__ == '__main__':
    unittest.main()
