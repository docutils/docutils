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

totest['field_lists'] = [
["""\
One-liners:

:Author: Me

:Version: 1

:Date: 2001-08-11

:Parameter i: integer
""",
"""\
<document>
    <paragraph>
        One-liners:
    <field_list>
        <field>
            <field_name>
                Author
            <field_body>
                <paragraph>
                    Me
        <field>
            <field_name>
                Version
            <field_body>
                <paragraph>
                    1
        <field>
            <field_name>
                Date
            <field_body>
                <paragraph>
                    2001-08-11
        <field>
            <field_name>
                Parameter
            <field_argument>
                i
            <field_body>
                <paragraph>
                    integer
"""],
["""\
One-liners, no blank lines:

:Author: Me
:Version: 1
:Date: 2001-08-11
:Parameter i: integer
""",
"""\
<document>
    <paragraph>
        One-liners, no blank lines:
    <field_list>
        <field>
            <field_name>
                Author
            <field_body>
                <paragraph>
                    Me
        <field>
            <field_name>
                Version
            <field_body>
                <paragraph>
                    1
        <field>
            <field_name>
                Date
            <field_body>
                <paragraph>
                    2001-08-11
        <field>
            <field_name>
                Parameter
            <field_argument>
                i
            <field_body>
                <paragraph>
                    integer
"""],
["""\
:field:
empty item above, no blank line
""",
"""\
<document>
    <field_list>
        <field>
            <field_name>
                field
            <field_body>
    <system_message level="2" type="WARNING">
        <paragraph>
            Field list ends without a blank line; unexpected unindent at line 2.
    <paragraph>
        empty item above, no blank line
"""],
["""\
Field bodies starting on the next line:

:Author:
  Me
:Version:
  1
:Date:
  2001-08-11
:Parameter i:
  integer
""",
"""\
<document>
    <paragraph>
        Field bodies starting on the next line:
    <field_list>
        <field>
            <field_name>
                Author
            <field_body>
                <paragraph>
                    Me
        <field>
            <field_name>
                Version
            <field_body>
                <paragraph>
                    1
        <field>
            <field_name>
                Date
            <field_body>
                <paragraph>
                    2001-08-11
        <field>
            <field_name>
                Parameter
            <field_argument>
                i
            <field_body>
                <paragraph>
                    integer
"""],
["""\
One-paragraph, multi-liners:

:Authors: Me,
          Myself,
          and I
:Version: 1
          or so
:Date: 2001-08-11
       (Saturday)
:Parameter i: counter
              (integer)
""",
"""\
<document>
    <paragraph>
        One-paragraph, multi-liners:
    <field_list>
        <field>
            <field_name>
                Authors
            <field_body>
                <paragraph>
                    Me,
                    Myself,
                    and I
        <field>
            <field_name>
                Version
            <field_body>
                <paragraph>
                    1
                    or so
        <field>
            <field_name>
                Date
            <field_body>
                <paragraph>
                    2001-08-11
                    (Saturday)
        <field>
            <field_name>
                Parameter
            <field_argument>
                i
            <field_body>
                <paragraph>
                    counter
                    (integer)
"""],
["""\
One-paragraph, multi-liners, not lined up:

:Authors: Me,
  Myself,
  and I
:Version: 1
  or so
:Date: 2001-08-11
  (Saturday)
:Parameter i: counter
  (integer)
""",
"""\
<document>
    <paragraph>
        One-paragraph, multi-liners, not lined up:
    <field_list>
        <field>
            <field_name>
                Authors
            <field_body>
                <paragraph>
                    Me,
                    Myself,
                    and I
        <field>
            <field_name>
                Version
            <field_body>
                <paragraph>
                    1
                    or so
        <field>
            <field_name>
                Date
            <field_body>
                <paragraph>
                    2001-08-11
                    (Saturday)
        <field>
            <field_name>
                Parameter
            <field_argument>
                i
            <field_body>
                <paragraph>
                    counter
                    (integer)
"""],
["""\
Multiple body elements:

:Authors: - Me
          - Myself
          - I

:Abstract:
    This is a field list item's body,
    containing multiple elements.

    Here's a literal block::

        def f(x):
            return x**2 + x

    Even nested field lists are possible:

    :Date: 2001-08-11
    :Day: Saturday
    :Time: 15:07
""",
"""\
<document>
    <paragraph>
        Multiple body elements:
    <field_list>
        <field>
            <field_name>
                Authors
            <field_body>
                <bullet_list bullet="-">
                    <list_item>
                        <paragraph>
                            Me
                    <list_item>
                        <paragraph>
                            Myself
                    <list_item>
                        <paragraph>
                            I
        <field>
            <field_name>
                Abstract
            <field_body>
                <paragraph>
                    This is a field list item's body,
                    containing multiple elements.
                <paragraph>
                    Here's a literal block:
                <literal_block>
                    def f(x):
                        return x**2 + x
                <paragraph>
                    Even nested field lists are possible:
                <field_list>
                    <field>
                        <field_name>
                            Date
                        <field_body>
                            <paragraph>
                                2001-08-11
                    <field>
                        <field_name>
                            Day
                        <field_body>
                            <paragraph>
                                Saturday
                    <field>
                        <field_name>
                            Time
                        <field_body>
                            <paragraph>
                                15:07
"""],
["""\
Nested field lists on one line:

:field1: :field2: :field3: body
:field4: :field5: :field6: body
                  :field7: body
         :field8: body
         :field9: body line 1
           body line 2
""",
"""\
<document>
    <paragraph>
        Nested field lists on one line:
    <field_list>
        <field>
            <field_name>
                field1
            <field_body>
                <field_list>
                    <field>
                        <field_name>
                            field2
                        <field_body>
                            <field_list>
                                <field>
                                    <field_name>
                                        field3
                                    <field_body>
                                        <paragraph>
                                            body
        <field>
            <field_name>
                field4
            <field_body>
                <field_list>
                    <field>
                        <field_name>
                            field5
                        <field_body>
                            <field_list>
                                <field>
                                    <field_name>
                                        field6
                                    <field_body>
                                        <paragraph>
                                            body
                                <field>
                                    <field_name>
                                        field7
                                    <field_body>
                                        <paragraph>
                                            body
                    <field>
                        <field_name>
                            field8
                        <field_body>
                            <paragraph>
                                body
                    <field>
                        <field_name>
                            field9
                        <field_body>
                            <paragraph>
                                body line 1
                                body line 2
"""],
["""\
:Parameter i j k: multiple arguments
""",
"""\
<document>
    <field_list>
        <field>
            <field_name>
                Parameter
            <field_argument>
                i
            <field_argument>
                j
            <field_argument>
                k
            <field_body>
                <paragraph>
                    multiple arguments
"""],
["""\
Some edge cases:

:Empty:
:Author: Me
No blank line before this paragraph.

:*Field* `with` **inline** ``markup``: inline markup shouldn't be recognized.

: Field: marker must not begin with whitespace.

:Field : marker must not end with whitespace.

Field: marker is missing its open-colon.

:Field marker is missing its close-colon.
""",
"""\
<document>
    <paragraph>
        Some edge cases:
    <field_list>
        <field>
            <field_name>
                Empty
            <field_body>
        <field>
            <field_name>
                Author
            <field_body>
                <paragraph>
                    Me
    <system_message level="2" type="WARNING">
        <paragraph>
            Field list ends without a blank line; unexpected unindent at line 4.
    <paragraph>
        No blank line before this paragraph.
    <field_list>
        <field>
            <field_name>
                *Field*
            <field_argument>
                `with`
            <field_argument>
                **inline**
            <field_argument>
                ``markup``
            <field_body>
                <paragraph>
                    inline markup shouldn't be recognized.
    <paragraph>
        : Field: marker must not begin with whitespace.
    <paragraph>
        :Field : marker must not end with whitespace.
    <paragraph>
        Field: marker is missing its open-colon.
    <paragraph>
        :Field marker is missing its close-colon.
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
