#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for `docutils.transforms.misc.ClassAttribute`.
"""

from __init__ import DocutilsTestSupport
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['class'] = ((), [
["""\
.. class:: one

paragraph
""",
"""\
<document source="test data">
    <paragraph class="one">
        paragraph
"""],
["""\
.. class:: two
..

    Block quote
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
    <block_quote class="two">
        <paragraph>
            Block quote
"""],
["""\
    Block quote

    .. class:: three

Paragraph
""",
"""\
<document source="test data">
    <block_quote>
        <paragraph>
            Block quote
    <paragraph class="three">
        Paragraph
"""],
["""\
.. class:: four

Section Title
=============

Paragraph
""",
"""\
<document source="test data">
    <section class="four" id="section-title" name="section title">
        <title>
            Section Title
        <paragraph>
            Paragraph
"""],
["""\
.. class::

.. class:: 99
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "class" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            .. class::
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Invalid class attribute value for "class" directive: "99".
        <literal_block xml:space="preserve">
            .. class:: 99
"""],
["""\
.. class:: one
.. class:: two

multiple class values may be assigned to one element
""",
"""\
<document source="test data">
    <paragraph class="one two">
        multiple class values may be assigned to one element
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
