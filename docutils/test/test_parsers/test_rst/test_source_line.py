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

if __name__ == '__main__':
    import __init__  # noqa: F401
from test_transforms import DocutilsTestSupport  # before importing docutils!
from docutils.transforms.universal import ExposeInternals
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(
    parser, suite_settings={'expose_internals': ['line', 'source']})
    s.generateTests(totest)
    return s


mydir = 'test_parsers/test_rst/'
include14 = os.path.join(mydir, 'includes/include14.txt')

totest = {}

totest['transitions'] = ((ExposeInternals,), [
["""\
Paragraph starting in line 1.
With *inline* element in line 2.

  Block quote in line 4

  -- attribution
     in line 6

* bullet list in line 9
* second item in line 10

1. enumerated list in line 12
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
        <list_item internal:source="test data">
            <paragraph internal:line="9" internal:source="test data">
                bullet list in line 9
        <list_item internal:source="test data">
            <paragraph internal:line="10" internal:source="test data">
                second item in line 10
    <enumerated_list enumtype="arabic" internal:line="12" internal:source="test data" prefix="" suffix=".">
        <list_item internal:source="test data">
            <paragraph internal:line="12" internal:source="test data">
                enumerated list in line 12
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
["""\
Paragraph

.. include:: %s
""" % include14,
"""\
<document source="test data">
    <paragraph internal:line="1" internal:source="test data">
        Paragraph
    <paragraph internal:line="1" internal:source="test_parsers/test_rst/includes/include14.txt">
        Paragraph starting in line 1.
        With \n\
        <emphasis>
            inline
         element in line 2.
    <block_quote internal:line="4" internal:source="test_parsers/test_rst/includes/include14.txt">
        <paragraph internal:line="4" internal:source="test_parsers/test_rst/includes/include14.txt">
            Block quote in line 4
        <attribution internal:line="6" internal:source="test_parsers/test_rst/includes/include14.txt">
            attribution
            in line 6
    <bullet_list bullet="*" internal:line="9" internal:source="test_parsers/test_rst/includes/include14.txt">
        <list_item internal:source="test_parsers/test_rst/includes/include14.txt">
            <paragraph internal:line="9" internal:source="test_parsers/test_rst/includes/include14.txt">
                bullet list in line 9
        <list_item internal:source="test_parsers/test_rst/includes/include14.txt">
            <paragraph internal:line="10" internal:source="test_parsers/test_rst/includes/include14.txt">
                second item in line 10
    <enumerated_list enumtype="arabic" internal:line="12" internal:source="test_parsers/test_rst/includes/include14.txt" prefix="" suffix=".">
        <list_item internal:source="test_parsers/test_rst/includes/include14.txt">
            <paragraph internal:line="12" internal:source="test_parsers/test_rst/includes/include14.txt">
                enumerated list in line 12
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
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
