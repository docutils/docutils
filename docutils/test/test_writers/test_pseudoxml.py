#!/usr/bin/env python3

# $Id$
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test for pseudo-XML writer.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.core import publish_string
from docutils.writers import pseudoxml


class WriterPublishTestCase(unittest.TestCase):
    maxDiff = None

    def test_publish(self):
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer=pseudoxml.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                        }).decode()
                    self.assertEqual(case_expected, output)

        for name, cases in totest_detailed.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest_detailed[{name!r}][{casenum}]'):
                    output = publish_string(
                        source=case_input,
                        writer=pseudoxml.Writer(),
                        settings_overrides={
                            '_disable_config': True,
                            'strict_visitor': True,
                            'detailed': True,
                        }).decode()
                    self.assertEqual(case_expected, output)


totest = {}
totest_detailed = {}

totest['basic'] = [
# input
[r"""
This is a paragraph.

----------

This is a paragraph
with \escaped \characters.

A Section
---------

Foo.
""",
# output
"""\
<document source="<string>">
    <paragraph>
        This is a paragraph.
    <transition>
    <paragraph>
        This is a paragraph
        with escaped characters.
    <section ids="a-section" names="a\\ section">
        <title>
            A Section
        <paragraph>
            Foo.
"""]
]

totest_detailed['basic'] = [
# input
[totest['basic'][0][0],
# output
"""\
<document source="<string>">
    <paragraph>
        <#text>
            'This is a paragraph.'
    <transition>
    <paragraph>
        <#text>
            'This is a paragraph\\n'
            'with \\x00escaped \\x00characters.'
    <section ids="a-section" names="a\\ section">
        <title>
            <#text>
                'A Section'
        <paragraph>
            <#text>
                'Foo.'
"""]
]

if __name__ == '__main__':
    unittest.main()
