#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for misc.Transitions transform.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers import rst, docutils_xml
from docutils.transforms.misc import Transitions
from docutils.transforms.universal import TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):

    maxDiff = None

    def test_transforms(self):
        parser = rst.Parser()
        settings = get_default_settings(rst.Parser)
        settings.legacy_ids = False
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

    def test_transfroms_x(self):
        # test samples given as Docutils XML in `totest_x`
        # (for document trees that cannot be generated from rST)
        parser = docutils_xml.Parser()
        settings = get_default_settings(docutils_xml.Parser)
        settings.warning_stream = ''
        for name, (transforms, cases) in totest_x.items():
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


totest = {}  # rST samples and expected pseudoXML
totest_x = {}  # XML samples and expected pseudoXML

totest['transitions'] = ((Transitions,), [
["""\
Section 1
=========

Subsection 1
------------

A transition at the end of a section is moved behind the section.

----------

Section 2
=========

Some text.
""",
"""\
<document source="test data">
    <section names="section\\ 1">
        <title>
            Section 1
        <section names="subsection\\ 1">
            <title>
                Subsection 1
            <paragraph>
                A transition at the end of a section is moved behind the section.
    <transition>
    <section names="section\\ 2">
        <title>
            Section 2
        <paragraph>
            Some text.
"""],
["""\
A paragraph.

----------

Section 1
=========

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph.
    <transition>
    <section names="section\\ 1">
        <title>
            Section 1
        <paragraph>
            Paragraph.
"""],
["""\
--------

A system message warns about invalid placement of transitions.
""",
"""\
<document source="test data">
    <transition>
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Transition at the start of the document.
    <paragraph>
        A system message warns about invalid placement of transitions.
"""],
["""\
rST and Doctree specifications say ...

--------

--------

... that two transitions should not be adjacent.
""",
"""\
<document source="test data">
    <paragraph>
        rST and Doctree specifications say ...
    <transition>
    <transition>
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            At least one body element should separate transitions.
    <paragraph>
        ... that two transitions should not be adjacent.
"""],
["""\
The specs also say that a section or document
may not end with a transition.

--------
""",
"""\
<document source="test data">
    <paragraph>
        The specs also say that a section or document
        may not end with a transition.
    <transition>
    <system_message level="2" line="4" source="test data" type="WARNING">
        <paragraph>
            Transition at the end of the document.
"""],
["""\
Moving and invisible elements don't count.

----------

.. meta:: :keywords: transition test
.. footer:: will move away
.. _anchor:
.. |substitution reference| replace:: is invisible
""",
"""\
<document source="test data">
    <meta content="transition test" name="keywords">
    <decoration>
        <footer>
            <paragraph>
                will move away
    <paragraph>
        Moving and invisible elements don't count.
    <transition>
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Transition at the end of the document.
    <target ids="anchor" names="anchor">
    <substitution_definition names="substitution\\ reference">
        is invisible
"""],
["""\
Sections with transitions at beginning and end.

Section 1
=========

----------

.. Comment after transition.

Section 2
=========

Some text before the transition.

----------
""",
"""\
<document source="test data">
    <paragraph>
        Sections with transitions at beginning and end.
    <section names="section\\ 1">
        <title>
            Section 1
        <transition>
        <system_message level="2" line="6" source="test data" type="WARNING">
            <paragraph>
                Transition at the start of the section.
        <comment xml:space="preserve">
            Comment after transition.
    <section names="section\\ 2">
        <title>
            Section 2
        <paragraph>
            Some text before the transition.
        <transition>
        <system_message level="2" line="15" source="test data" type="WARNING">
            <paragraph>
                Transition at the end of the document.
"""],
["""\
A paragraph and two transitions.

----------

----------
""",
"""\
<document source="test data">
    <paragraph>
        A paragraph and two transitions.
    <transition>
    <transition>
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            At least one body element should separate transitions.
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Transition at the end of the document.
"""],
["""\
A paragraph, two transitions, and a blank line.

----------

----------

""",
"""\
<document source="test data">
    <paragraph>
        A paragraph, two transitions, and a blank line.
    <transition>
    <transition>
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            At least one body element should separate transitions.
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Transition at the end of the document.
"""],
["""\
----------

Document beginning with a transition.
""",
"""\
<document source="test data">
    <transition>
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Transition at the start of the document.
    <paragraph>
        Document beginning with a transition.
"""],
["""\
.. class:: classy
.. meta:: :keywords: transition test
.. footer:: will move away
.. _anchor:
.. |substitution reference| replace:: is invisible

----------

Document beginning with a transition (title, moving elements,
and invisible elements don't count).
""",
"""\
<document source="test data">
    <meta content="transition test" name="keywords">
    <decoration>
        <footer>
            <paragraph>
                will move away
    <target ids="anchor" names="anchor">
    <substitution_definition names="substitution\\ reference">
        is invisible
    <transition classes="classy">
    <system_message level="2" line="7" source="test data" type="WARNING">
        <paragraph>
            Transition at the start of the document.
    <paragraph>
        Document beginning with a transition (title, moving elements,
        and invisible elements don't count).
"""],
["""\
Section 1
=========

----------

----------

----------

Section 2
=========
""",
"""\
<document source="test data">
    <section names="section\\ 1">
        <title>
            Section 1
        <transition>
        <system_message level="2" line="4" source="test data" type="WARNING">
            <paragraph>
                Transition at the start of the section.
        <transition>
    <transition>
    <system_message level="2" line="8" source="test data" type="WARNING">
        <paragraph>
            At least one body element should separate transitions.
    <section names="section\\ 2">
        <title>
            Section 2
"""],
["""\
----------

----------

----------
""",
# The placement of <system_message>s in this sample is an implementation
# detail, see the remarks in the preceding test nr. 11.
"""\
<document source="test data">
    <transition>
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Transition at the start of the document.
    <transition>
    <transition>
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            At least one body element should separate transitions.
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Transition at the end of the document.
"""],
])


totest_x['transitions extra'] = ((Transitions,), [
# currently, a <transition> is only valid inside <document> or <section>
["""\
<document>
  <paragraph>Some text.</paragraph>
  <transition />
  <paragraph>Some text.</paragraph>
  <block_quote>
    <paragraph>Some text.</paragraph>
    <transition />
    <paragraph>Some text.</paragraph>
  </block_quote>
  <paragraph>A <transition /> in a paragraph.</paragraph>
</document>
""",
"""\
<document source="test data">
    <paragraph>
        Some text.
    <transition>
    <paragraph>
        Some text.
    <block_quote>
        <paragraph>
            Some text.
        <transition>
        <system_message level="2" line="7" source="test data" type="WARNING">
            <paragraph>
                Transition only valid as child of <document> or <section>.
        <paragraph>
            Some text.
    <paragraph>
        A \n\
        <transition>
         in a paragraph.
    <system_message level="2" line="10" source="test data" type="WARNING">
        <paragraph>
            Transition only valid as child of <document> or <section>.
"""],
])


if __name__ == '__main__':
    unittest.main()
