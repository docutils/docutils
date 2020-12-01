#!/usr/bin/env python3
# -*- coding: utf8 -*-
# :Copyright: © 2020 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

"""
Test for block quotes in CommonMark parsers
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""

from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.RecommonmarkParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['block_quotes'] = [
["""\
> block quote
> line 2
""",
"""\
<document source="test data">
    <block_quote>
        <paragraph>
            block quote
            line 2
"""],
["""\
Line 1.

  > Indented block quote.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
    <block_quote>
        <paragraph>
            Indented block quote.
"""],
["""\
Line 1.
Line 2.
> Block quote, without blank line before.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Block quote, without blank line before.
"""],
["""\
Line 1.
Line 2.

>Block quote,
continuation line 
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Block quote,
            continuation line
"""],
["""\
Here is a paragraph.

>   >  Nested
>
>   block quotes.
""",
"""\
<document source="test data">
    <paragraph>
        Here is a paragraph.
    <block_quote>
        <block_quote>
            <paragraph>
                Nested
        <paragraph>
            block quotes.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
