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
Test for enumerated lists in CommonMark parsers
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

totest['enumerated_lists'] = [
["""\
1. Item one.

2. Item two.

3. Item three.
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
            <paragraph>
                Item one.
        <list_item>
            <paragraph>
                Item two.
        <list_item>
            <paragraph>
                Item three.
"""],
["""\
No blank lines betwen items:

1. Item one.
2. Item two.
3. Item three.
""",
"""\
<document source="test data">
    <paragraph>
        No blank lines betwen items:
    <enumerated_list>
        <list_item>
            <paragraph>
                Item one.
        <list_item>
            <paragraph>
                Item two.
        <list_item>
            <paragraph>
                Item three.
"""],
["""\
1.
   Content may start at the next line
   if it's indented at least 3 spaces.
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
            <paragraph>
                Content may start at the next line
                if it's indented at least 3 spaces.
"""],
["""\
1.
empty item above, no blank line, no indent
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
    <paragraph>
        empty item above, no blank line, no indent
"""],
["""\
Items are auto-numbered.
No check for consistency: Skipping item 3

1. Item 1.
2. Item 2.
4. Item 4.
""",
"""\
<document source="test data">
    <paragraph>
        Items are auto-numbered.
        No check for consistency: Skipping item 3
    <enumerated_list>
        <list_item>
            <paragraph>
                Item 1.
        <list_item>
            <paragraph>
                Item 2.
        <list_item>
            <paragraph>
                Item 4.
"""],
["""\
No warning when starting with non-ordinal-1:

0. Item zero.
1. Item one.
2. Item two.
3. Item three.

And again:

2. Item two.
3. Item three.
""",
"""\
<document source="test data">
    <paragraph>
        No warning when starting with non-ordinal-1:
    <enumerated_list>
        <list_item>
            <paragraph>
                Item zero.
        <list_item>
            <paragraph>
                Item one.
        <list_item>
            <paragraph>
                Item two.
        <list_item>
            <paragraph>
                Item three.
    <paragraph>
        And again:
    <enumerated_list>
        <list_item>
            <paragraph>
                Item two.
        <list_item>
            <paragraph>
                Item three.
"""],
["""\
1. Item one: line 1,
   line 2.
2. Item two: line 1,
line 2.
3. Item three: paragraph 1, line 1,
   line 2.

   Paragraph 2.
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
            <paragraph>
                Item one: line 1,
                line 2.
        <list_item>
            <paragraph>
                Item two: line 1,
                line 2.
        <list_item>
            <paragraph>
                Item three: paragraph 1, line 1,
                line 2.
            <paragraph>
                Paragraph 2.
"""],
["""\
Supported enumeration sequences:

1. Item 1.
2. Item 2.

1) Item 1)
2) Item 2)

""",
"""\
<document source="test data">
    <paragraph>
        Supported enumeration sequences:
    <enumerated_list>
        <list_item>
            <paragraph>
                Item 1.
        <list_item>
            <paragraph>
                Item 2.
    <enumerated_list>
        <list_item>
            <paragraph>
                Item 1)
        <list_item>
            <paragraph>
                Item 2)
"""],
["""\
Nested enumerated lists:

1. Item 1

   1) Item 1.1
   2) Item 1.2
   3) Item 1.3

2. Item 2

   1. Item 2.1

      1) Item 2.1.1
      2) Item 2.1.2
      3) Item 2.1.3

   2. Item 2.2

   3. Item 2.3

3. Item 3.
""",
"""\
<document source="test data">
    <paragraph>
        Nested enumerated lists:
    <enumerated_list>
        <list_item>
            <paragraph>
                Item 1
            <enumerated_list>
                <list_item>
                    <paragraph>
                        Item 1.1
                <list_item>
                    <paragraph>
                        Item 1.2
                <list_item>
                    <paragraph>
                        Item 1.3
        <list_item>
            <paragraph>
                Item 2
            <enumerated_list>
                <list_item>
                    <paragraph>
                        Item 2.1
                    <enumerated_list>
                        <list_item>
                            <paragraph>
                                Item 2.1.1
                        <list_item>
                            <paragraph>
                                Item 2.1.2
                        <list_item>
                            <paragraph>
                                Item 2.1.3
                <list_item>
                    <paragraph>
                        Item 2.2
                <list_item>
                    <paragraph>
                        Item 2.3
        <list_item>
            <paragraph>
                Item 3.
"""],
["""\
1. Item one: line 1,
   line 2.
2. Item two: line 1,
  line 2.
3. Item three: paragraph 1, line 1,
line 2.

   Item three: paragraph 2.
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
            <paragraph>
                Item one: line 1,
                line 2.
        <list_item>
            <paragraph>
                Item two: line 1,
                line 2.
        <list_item>
            <paragraph>
                Item three: paragraph 1, line 1,
                line 2.
            <paragraph>
                Item three: paragraph 2.
"""],
["""\
3-space indent, with a trailing space:

1. \n\
   list item 1

3-space indent, no trailing space:

1.
   list item 1

2-space indent, empty list item:

1.
  foo

1-space indent, empty list item:

1.
 foo

0-space indent, empty list item:

1.
foo

No item content:

1.
""",
"""\
<document source="test data">
    <paragraph>
        3-space indent, with a trailing space:
    <enumerated_list>
        <list_item>
            <paragraph>
                list item 1
    <paragraph>
        3-space indent, no trailing space:
    <enumerated_list>
        <list_item>
            <paragraph>
                list item 1
    <paragraph>
        2-space indent, empty list item:
    <enumerated_list>
        <list_item>
    <paragraph>
        foo
    <paragraph>
        1-space indent, empty list item:
    <enumerated_list>
        <list_item>
    <paragraph>
        foo
    <paragraph>
        0-space indent, empty list item:
    <enumerated_list>
        <list_item>
    <paragraph>
        foo
    <paragraph>
        No item content:
    <enumerated_list>
        <list_item>
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
