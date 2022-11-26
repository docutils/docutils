#! /usr/bin/env python3

# $Id$
# Author: grubert
# Maintainer: docutils-develop@lists.sourceforge.net
# Copyright: This module has been placed in the public domain.

"""
test get_writer_class
"""

from pathlib import Path
import sys
import unittest

# Prepend the "docutils root" to the Python library path
# so we import the local `docutils` and `test` packages.
# Ensure `test` package can be loaded also if not running as __main__
# (required by ``python -m unittest``
DOCUTILS_ROOT = Path(__file__).resolve().parents[2]
if str(DOCUTILS_ROOT) not in sys.path:
    sys.path.insert(0, str(DOCUTILS_ROOT))

from docutils.writers import get_writer_class  # noqa: E402


class GetWriterClassTestCase(unittest.TestCase):

    def test_registered_writer(self):
        get_writer_class('manpage')
        # raises ImportError on failure

    def test_bogus_writer(self):
        with self.assertRaises(ImportError):
            get_writer_class('nope')

    def test_local_writer(self):
        # imports local-writer.py from the test package (added above)
        get_writer_class('test.local-writer')
        # raises ImportError on failure


if __name__ == '__main__':
    unittest.main()
