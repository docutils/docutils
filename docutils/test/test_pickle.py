#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests of document tree pickling.
"""

from pathlib import Path
import pickle
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from docutils import core


class PickleTests(unittest.TestCase):

    def test_pickle(self):
        doctree = core.publish_doctree(
            source='Title\n=====\n\nparagraph\n',
            settings_overrides={'_disable_config': True})
        dill = pickle.dumps(doctree)
        reconstituted = pickle.loads(dill)
        self.assertEqual(doctree.pformat(), reconstituted.pformat())


if __name__ == '__main__':
    unittest.main()
