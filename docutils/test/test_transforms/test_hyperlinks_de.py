#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Hyperlinks with non-English language.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).parents[2]))

from docutils.frontend import get_default_settings
from docutils.languages import de, get_language
from docutils.parsers.rst import directives, Parser
from docutils.transforms.references import (
         PropagateTargets, AnonymousHyperlinks, IndirectHyperlinks,
         ExternalTargets, InternalTargets, DanglingReferences)
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    def setUp(self):
        """
        The "info" system-messages for directive fallbacks are only generated
        once (the name -> directive mapping is cached in
        ``docutils.parsers.rst.directives._directives``), so we need to reset
        the cache before running this test.

        See also https://sourceforge.net/p/docutils/feature-requests/71/
        """
        directives._directives.clear()
        get_language.cache.clear()

    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.language_code = 'de'
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

totest['hyperlinks'] = ((PropagateTargets, AnonymousHyperlinks,
                         IndirectHyperlinks, ExternalTargets,
                         InternalTargets, DanglingReferences), [

["""\
Target_ should propagate past the system_message to set "id" on note.

.. _target:
.. note:: Kurznotiz
   :name: mynote
""",
f"""\
<document source="test data">
    <paragraph>
        <reference name="Target" refid="target">
            Target
         should propagate past the system_message to set "id" on note.
    <target refid="target">
    <system_message level="1" line="4" source="test data" type="INFO">
        <paragraph>
            No directive entry for "note" in module "docutils.parsers.rst.languages.de".
            Using English fallback for directive "note".
    <note ids="mynote target" names="mynote target">
        <paragraph>
            Kurznotiz
    <system_message level="1" source="test data" type="INFO">
        <paragraph>
            Using {de} for language "de".
"""],
])

if __name__ == '__main__':
    unittest.main()
