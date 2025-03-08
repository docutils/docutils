#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Substitutions.
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
from docutils.transforms.references import Substitutions
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):
    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.line_length_limit = 80
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


# pseudoxml representation of the substitution definition content:
a = '        lol'
b = '        10^1 \n' + '\n         \n'.join(10 * [a])
c = '        10^2 \n' + '\n         \n'.join(10 * [b])

totest = {}

totest['substitutions'] = ((Substitutions,), [
["""\
The billion laughs attack for ReStructuredText:

.. |a| replace:: lol
.. |b| replace:: 10^1 |a| |a| |a| |a| |a| |a| |a| |a| |a| |a|
.. |c| replace:: 10^2 |b| |b| |b| |b| |b| |b| |b| |b| |b| |b|
.. ...

|a| |c| continuation text
""",
f"""\
<document source="test data">
    <paragraph>
        The billion laughs attack for ReStructuredText:
    <substitution_definition names="a">
        lol
    <substitution_definition names="b">
{b}
    <substitution_definition names="c">
{c}
    <comment xml:space="preserve">
        ...
    <paragraph>
        lol
         \n\
        <problematic ids="problematic-1" refid="system-message-1">
            |c|
         continuation text
    <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="9" source="test data" type="ERROR">
        <paragraph>
            Substitution definition "c" exceeds the line-length-limit.
"""],
])


if __name__ == '__main__':
    unittest.main()
