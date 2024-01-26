#! /usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for transforms/__init__.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils import transforms, utils


class TestTransform(transforms.Transform):

    # marker for pytest to ignore this class during test discovery
    __test__ = False

    default_priority = 100

    applied = 0

    def apply(self, **kwargs):
        self.applied += 1
        assert kwargs == {'foo': 42}


class KwargsTestCase(unittest.TestCase):

    def test_kwargs(self):
        transformer = transforms.Transformer(utils.new_document('test data'))
        transformer.add_transform(TestTransform, foo=42)
        transformer.apply_transforms()
        self.assertEqual(len(transformer.applied), 1)
        self.assertEqual(len(transformer.applied[0]), 4)
        transform_record = transformer.applied[0]
        self.assertEqual(transform_record[1], TestTransform)
        self.assertEqual(transform_record[3], {'foo': 42})


if __name__ == '__main__':
    unittest.main()
