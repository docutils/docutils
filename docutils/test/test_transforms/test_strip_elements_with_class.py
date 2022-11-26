#! /usr/bin/env python3

# $Id$
# Author: Guenter Milde <milde@users.sf.net>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.universal.StripClassesAndElements.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.universal import StripClassesAndElements, TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.strip_elements_with_classes = ['spam', 'no-ham']
        settings.strip_classes = ['spam', 'noise']
        for name, (transforms, cases) in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    # Don't do a ``populate_from_components()`` because that
                    # would enable the Transformer's default transforms.
                    document.transformer.add_transforms(transforms)
                    document.transformer.add_transform(TestMessages)
                    document.transformer.apply_transforms()
                    output = document.pformat()
                    self.assertEqual(output, case_expected)


totest = {}

totest['strip_spam'] = ((StripClassesAndElements,), [
["""\
not classy

.. class:: spam

this is spam

.. class:: ham noise

this is ham

.. code::
   :class: spam
   \n\
   print("spam")
   \n\
.. image:: spam.jpg
   :class: spam

this is not ham
""",
"""\
<document source="test data">
    <paragraph>
        not classy
    <paragraph classes="ham">
        this is ham
    <paragraph>
        this is not ham
"""],
])


if __name__ == '__main__':
    unittest.main()
