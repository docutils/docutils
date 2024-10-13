#! /usr/bin/env python3
# $Id$
# Author: Günter Milde
# Maintainer: docutils-develop@lists.sourceforge.net
# :Copyright: 2021 Günter Milde,
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""Test internal source and line attributes (for correct error reporting).

This test is to ensure source and line numbers are correct.
It does not fix behaviour regarding which nodes have source/line attributes,
adding them to more nodes is regarded a compatible feature extension.
"""

# Requires the `universal.ExposeInternals` transform (tested in
# ``test_transforms/test_expose_internals.py``)
# to make internal attributes visible.

import os
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.universal import ExposeInternals, TestMessages
from docutils.utils import new_document

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = os.path.abspath(os.path.join(__file__, '..', '..', '..'))


class TransformTestCase(unittest.TestCase):
    maxDiff = None

    def test_transforms(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.expose_internals = ['line', 'source']
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


mydir = os.path.join(TEST_ROOT, 'test_parsers/test_rst')
include14 = os.path.relpath(
    os.path.join(mydir, 'includes/include14.rst'),
    os.getcwd()).replace('\\', '/')
totest = {}

totest['transitions'] = ((ExposeInternals,), [
["""\
Paragraph starting in line 1.
With *inline* element in line 2.

  Block quote in line 4

  -- attribution
     in line 6

* bullet list in line 9
*
  second item in line 10
  paragraph starts in line 11

#. enumerated list in line 14
""",
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph starting in line 1.
        With \n\
        <emphasis>
            inline
         element in line 2.
    <block_quote internal:line="4" internal:source="test data">
        <paragraph internal:line="4" internal:source="test data">
            Block quote in line 4
        <attribution internal:line="6" internal:source="test data">
            attribution
            in line 6
    <bullet_list bullet="*" internal:line="9" internal:source="test data">
        <list_item internal:line="9" internal:source="test data">
            <paragraph internal:line="9" internal:source="test data">
                bullet list in line 9
        <list_item internal:line="10" internal:source="test data">
            <paragraph internal:line="11" internal:source="test data">
                second item in line 10
                paragraph starts in line 11
    <enumerated_list enumtype="arabic" internal:line="14" internal:source="test data" prefix="" suffix=".">
        <list_item internal:line="14" internal:source="test data">
            <paragraph internal:line="14" internal:source="test data">
                enumerated list in line 14
"""],
["""\
Paragraph

  Block quote in line 3

  -- attribution in line 5
""",
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <block_quote internal:line="3" internal:source="test data">
        <paragraph internal:line="3" internal:source="test data">
            Block quote in line 3
        <attribution internal:line="5" internal:source="test data">
            attribution in line 5
"""],
["""\
Paragraph

  Block quote in line 3

     nested block quote
     in line 5

       double nested quote in line 8

       -- double-nested attribution in line 10

     line 12

     -- nested attribution in line 14

  -- attribution in line 16
""",
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <block_quote internal:line="3" internal:source="test data">
        <paragraph internal:line="3" internal:source="test data">
            Block quote in line 3
        <block_quote internal:line="5" internal:source="test data">
            <paragraph internal:line="5" internal:source="test data">
                nested block quote
                in line 5
            <block_quote internal:line="8" internal:source="test data">
                <paragraph internal:line="8" internal:source="test data">
                    double nested quote in line 8
                <attribution internal:line="10" internal:source="test data">
                    double-nested attribution in line 10
            <paragraph internal:line="12" internal:source="test data">
                line 12
            <attribution internal:line="14" internal:source="test data">
                nested attribution in line 14
        <attribution internal:line="16" internal:source="test data">
            attribution in line 16
"""],
[f"""\
Paragraph

.. include:: {include14}
""",
f"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <paragraph internal:line="1" internal:source="{include14}">
        Paragraph starting in line 1.
        With \n\
        <emphasis>
            inline
         element in line 2.
    <block_quote internal:line="4" internal:source="{include14}">
        <paragraph internal:line="4" internal:source="{include14}">
            Block quote in line 4
        <attribution internal:line="6" internal:source="{include14}">
            attribution
            in line 6
    <bullet_list bullet="*" internal:line="9" internal:source="{include14}">
        <list_item internal:line="9" internal:source="{include14}">
            <paragraph internal:line="9" internal:source="{include14}">
                bullet list in line 9
        <list_item internal:line="10" internal:source="{include14}">
            <paragraph internal:line="10" internal:source="{include14}">
                second item in line 10
    <enumerated_list enumtype="arabic" internal:line="12" internal:source="{include14}" prefix="" suffix=".">
        <list_item internal:line="12" internal:source="{include14}">
            <paragraph internal:line="12" internal:source="{include14}">
                enumerated list in line 12
    <admonition classes="admonition-line-14" internal:line="14" internal:source="{include14}">
        <title internal:line="14" internal:source="{include14}">
            line 14
        <paragraph internal:line="16" internal:source="{include14}">
            Generic admonition text in line 16
    <definition_list internal:line="18" internal:source="{include14}">
        <definition_list_item internal:line="18" internal:source="{include14}">
            <term internal:line="18" internal:source="{include14}">
                term on line 18
            <definition internal:line="19" internal:source="{include14}">
                <paragraph internal:line="19" internal:source="{include14}">
                    definition in line 19
"""],
["""\
Paragraph

  Block quote in line 3

  -- attribution in line 5

  Second block quote in line 7

  -- attribution in line 9

Final paragraph in line 11
""",
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <block_quote internal:line="3" internal:source="test data">
        <paragraph internal:line="3" internal:source="test data">
            Block quote in line 3
        <attribution internal:line="5" internal:source="test data">
            attribution in line 5
    <block_quote internal:line="7" internal:source="test data">
        <paragraph internal:line="7" internal:source="test data">
            Second block quote in line 7
        <attribution internal:line="9" internal:source="test data">
            attribution in line 9
    <paragraph internal:line="11" internal:source="test data">
        Final paragraph in line 11
"""],
["""\
Paragraph

.. image:: line-3.png
   :width: 3em

.. figure:: line-6.png

   caption on line 8

Final paragraph in line 10
""",
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <image internal:line="3" internal:source="test data" uri="line-3.png" width="3em">
    <figure internal:line="6" internal:source="test data">
        <image internal:line="6" internal:source="test data" uri="line-6.png">
        <caption internal:line="8" internal:source="test data">
            caption on line 8
    <paragraph internal:line="10" internal:source="test data">
        Final paragraph in line 10
"""],
["""\
Paragraph

.. sidebar::

   .. compound::
      paragraph at line 6

      | line block,
        first line
      | line block, second line at line 10
      |   indented
          line

      .. line-block::  line block at line 14
                       with legacy directive

   continuation at line 17

.. topic:: topic title at line 19

   .. container::

         container content at line 23

Final paragraph at line 25
""",
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <sidebar internal:line="3" internal:source="test data">
        <compound internal:line="5" internal:source="test data">
            <paragraph internal:line="6" internal:source="test data">
                paragraph at line 6
            <line_block internal:line="8" internal:source="test data">
                <line internal:line="8" internal:source="test data">
                    line block,
                    first line
                <line internal:line="10" internal:source="test data">
                    line block, second line at line 10
                <line_block>
                    <line internal:line="11" internal:source="test data">
                        indented
                        line
            <line_block internal:line="14" internal:source="test data">
                <line internal:line="14" internal:source="test data">
                    line block at line 14
                <line internal:line="15" internal:source="test data">
                    with legacy directive
        <paragraph internal:line="17" internal:source="test data">
            continuation at line 17
    <topic internal:line="19" internal:source="test data">
        <title>
            topic title at line 19
        <container internal:line="21" internal:source="test data">
            <paragraph internal:line="23" internal:source="test data">
                container content at line 23
    <paragraph internal:line="25" internal:source="test data">
        Final paragraph at line 25
"""],
])


if __name__ == '__main__':
    unittest.main()
