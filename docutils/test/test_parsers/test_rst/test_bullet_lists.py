#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for states.py.
"""

import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['bullet_lists'] = [
["""\
- item
""",
"""\
<document>
    <bullet_list bullet="-">
        <list_item>
            <paragraph>
                item
"""],
["""\
* item 1

* item 2
""",
"""\
<document>
    <bullet_list bullet="*">
        <list_item>
            <paragraph>
                item 1
        <list_item>
            <paragraph>
                item 2
"""],
["""\
No blank line between:

+ item 1
+ item 2
""",
"""\
<document>
    <paragraph>
        No blank line between:
    <bullet_list bullet="+">
        <list_item>
            <paragraph>
                item 1
        <list_item>
            <paragraph>
                item 2
"""],
["""\
- item 1, para 1.

  item 1, para 2.

- item 2
""",
"""\
<document>
    <bullet_list bullet="-">
        <list_item>
            <paragraph>
                item 1, para 1.
            <paragraph>
                item 1, para 2.
        <list_item>
            <paragraph>
                item 2
"""],
["""\
- item 1, line 1
  item 1, line 2
- item 2
""",
"""\
<document>
    <bullet_list bullet="-">
        <list_item>
            <paragraph>
                item 1, line 1
                item 1, line 2
        <list_item>
            <paragraph>
                item 2
"""],
["""\
Different bullets:

- item 1

+ item 2

* item 3
- item 4
""",
"""\
<document>
    <paragraph>
        Different bullets:
    <bullet_list bullet="-">
        <list_item>
            <paragraph>
                item 1
    <bullet_list bullet="+">
        <list_item>
            <paragraph>
                item 2
    <bullet_list bullet="*">
        <list_item>
            <paragraph>
                item 3
    <system_message level="2" type="WARNING">
        <paragraph>
            Bullet list ends without a blank line; unexpected unindent at line 8.
    <bullet_list bullet="-">
        <list_item>
            <paragraph>
                item 4
"""],
["""\
- item
no blank line
""",
"""\
<document>
    <bullet_list bullet="-">
        <list_item>
            <paragraph>
                item
    <system_message level="2" type="WARNING">
        <paragraph>
            Bullet list ends without a blank line; unexpected unindent at line 2.
    <paragraph>
        no blank line
"""],
["""\
-

empty item above
""",
"""\
<document>
    <bullet_list bullet="-">
        <list_item>
    <paragraph>
        empty item above
"""],
["""\
-
empty item above, no blank line
""",
"""\
<document>
    <bullet_list bullet="-">
        <list_item>
    <system_message level="2" type="WARNING">
        <paragraph>
            Bullet list ends without a blank line; unexpected unindent at line 2.
    <paragraph>
        empty item above, no blank line
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
