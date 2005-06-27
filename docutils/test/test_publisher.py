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


test_document = """\
Test Document
=============

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
        # Assert transforms have been applied (in this case the
        # DocTitle transform).
        self.assert_(isinstance(doctree[0], nodes.title))
        self.assert_(isinstance(doctree[1], nodes.paragraph))

        # Write out the document.
        output, parts = core.publish_from_doctree(
            doctree, writer_name='pseudoxml')

        self.assert_(isinstance(output, StringType))
        assert isinstance(parts, DictType)


if __name__ == '__main__':
    unittest.main()
