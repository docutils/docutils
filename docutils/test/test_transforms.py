#! /usr/bin/env python

# Author: Felix Wiemann
# Contact: Felix_Wiemann@ososo.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Test module for transforms/__init__.py.
"""

from docutils import transforms, utils

import unittest

class TestTransform(transforms.Transform):

    default_priority = 100

    applied = 0
    def apply(self):
        self.applied += 1

class KwargsTestCase(unittest.TestCase):

    def test_kwargs(self):
        transformer = transforms.Transformer(utils.new_document('test data'))
        transformer.add_transform(TestTransform, foo=42)
        transformer.apply_transforms()
        self.assertEqual(len(transformer.applied), 1)
        self.assertEqual(len(transformer.applied[0]), 4)
        transform = transformer.applied[0][0]
        self.assertEqual(transform.__class__, TestTransform)
        self.assertEqual(transform.data, {'foo': 42})


if __name__ == '__main__':
    unittest.main()
