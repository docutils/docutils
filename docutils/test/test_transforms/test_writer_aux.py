#! /usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for writer_aux transforms.
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
from docutils.transforms import writer_aux
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
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
                    self.assertEqual(case_expected, output)


totest = {}

totest['admonitions'] = ((writer_aux.Admonitions,), [
["""\
.. note::

   These are the note contents.

   Another paragraph.
""",
"""\
<document source="test data">
    <admonition classes="note">
        <title>
            Note
        <paragraph>
            These are the note contents.
        <paragraph>
            Another paragraph.
"""],
["""\
.. admonition:: Generic

   Admonitions contents...
""",
"""\
<document source="test data">
    <admonition classes="admonition-generic admonition">
        <title>
            Generic
        <paragraph>
            Admonitions contents...
"""],
])


if __name__ == '__main__':
    unittest.main()
