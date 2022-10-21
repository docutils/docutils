#! /usr/bin/env python3

# $Id$
# Author: grubert
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
test get_writer_class
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test import DocutilsTestSupport
from docutils.writers import get_writer_class


class GetWriterClassTestCase(DocutilsTestSupport.StandardTestCase):

    def test_registered_writer(self):
        get_writer_class('manpage')
        # raises ImportError on failure

    def test_bogus_writer(self):
        with self.assertRaises(ImportError):
            get_writer_class('nope')

    def test_local_writer(self):
        # requires local-writer.py in test directory (testroot)
        get_writer_class('local-writer')
        # raises ImportError on failure


if __name__ == '__main__':
    import unittest
    unittest.main()
