#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for states.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['section_headers'] = [
["""\
Title
=====

Paragraph.
""",
"""\
<document>
    <section id="title" name="title">
        <title>
            Title
        <paragraph>
            Paragraph.
"""],
["""\
Title
=====
Paragraph (no blank line).
""",
"""\
<document>
    <section id="title" name="title">
        <title>
            Title
        <paragraph>
            Paragraph (no blank line).
"""],
["""\
Paragraph.

Title
=====

Paragraph.
""",
"""\
<document>
    <paragraph>
        Paragraph.
    <section id="title" name="title">
        <title>
            Title
        <paragraph>
            Paragraph.
"""],
["""\
Test unexpected section titles.

    Title
    =====
    Paragraph.

    -----
    Title
    -----
    Paragraph.
""",
"""\
<document>
    <paragraph>
        Test unexpected section titles.
    <block_quote>
        <system_message level="4" type="SEVERE">
            <paragraph>
                Unexpected section title at line 4.
            <literal_block>
                Title
                =====
        <paragraph>
            Paragraph.
        <system_message level="4" type="SEVERE">
            <paragraph>
                Unexpected section title or transition at line 7.
            <literal_block>
                -----
        <system_message level="4" type="SEVERE">
            <paragraph>
                Unexpected section title at line 9.
            <literal_block>
                Title
                -----
        <paragraph>
            Paragraph.
"""],
["""\
Title
====

Test short underline.
""",
"""\
<document>
    <section id="title" name="title">
        <title>
            Title
        <system_message level="2" type="WARNING">
            <paragraph>
                Title underline too short at line 2.
            <literal_block>
                Title
                ====
        <paragraph>
            Test short underline.
"""],
["""\
=====
Title
=====

Test overline title.
""",
"""\
<document>
    <section id="title" name="title">
        <title>
            Title
        <paragraph>
            Test overline title.
"""],
["""\
=======
 Title
=======

Test overline title with inset.
""",
"""\
<document>
    <section id="title" name="title">
        <title>
            Title
        <paragraph>
            Test overline title with inset.
"""],
["""\
========================
 Test Missing Underline
""",
"""\
<document>
    <system_message level="4" type="SEVERE">
        <paragraph>
            Incomplete section title at line 1.
        <literal_block>
            ========================
             Test Missing Underline
"""],
["""\
========================
 Test Missing Underline

""",
"""\
<document>
    <system_message level="4" type="SEVERE">
        <paragraph>
            Missing underline for overline at line 1.
        <literal_block>
            ========================
             Test Missing Underline
"""],
["""\
=======
 Title

Test missing underline, with paragraph.
""",
"""\
<document>
    <system_message level="4" type="SEVERE">
        <paragraph>
            Missing underline for overline at line 1.
        <literal_block>
            =======
             Title
    <paragraph>
        Test missing underline, with paragraph.
"""],
["""\
=======
 Long    Title
=======

Test long title and space normalization.
""",
"""\
<document>
    <section id="long-title" name="long title">
        <title>
            Long    Title
        <system_message level="2" type="WARNING">
            <paragraph>
                Title overline too short at line 1.
            <literal_block>
                =======
                 Long    Title
                =======
        <paragraph>
            Test long title and space normalization.
"""],
["""\
=======
 Title
-------

Paragraph.
""",
"""\
<document>
    <system_message level="4" type="SEVERE">
        <paragraph>
            Title overline & underline mismatch at line 1.
        <literal_block>
            =======
             Title
            -------
    <paragraph>
        Paragraph.
"""],
["""\
========================

========================

Test missing titles; blank line in-between.

========================

========================
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Document or section may not begin with a transition (line 1).
    <transition>
    <system_message level="3" type="ERROR">
        <paragraph>
            At least one body element must separate transitions; adjacent transitions at line 3.
    <transition>
    <paragraph>
        Test missing titles; blank line in-between.
    <transition>
    <transition>
    <system_message level="3" type="ERROR">
        <paragraph>
            Document or section may not end with a transition (line 9).
"""],
["""\
========================
========================

Test missing titles; nothing in-between.

========================
========================
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Invalid section title or transition marker at line 1.
        <literal_block>
            ========================
            ========================
    <paragraph>
        Test missing titles; nothing in-between.
    <system_message level="3" type="ERROR">
        <paragraph>
            Invalid section title or transition marker at line 6.
        <literal_block>
            ========================
            ========================
"""],
["""\
.. Test return to existing, highest-level section (Title 3).

Title 1
=======
Paragraph 1.

Title 2
-------
Paragraph 2.

Title 3
=======
Paragraph 3.

Title 4
-------
Paragraph 4.
""",
"""\
<document>
    <comment>
        Test return to existing, highest-level section (Title 3).
    <section id="title-1" name="title 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
    <section id="title-3" name="title 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
        <section id="title-4" name="title 4">
            <title>
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
Test return to existing, highest-level section (Title 3, with overlines).

=======
Title 1
=======
Paragraph 1.

-------
Title 2
-------
Paragraph 2.

=======
Title 3
=======
Paragraph 3.

-------
Title 4
-------
Paragraph 4.
""",
"""\
<document>
    <paragraph>
        Test return to existing, highest-level section (Title 3, with overlines).
    <section id="title-1" name="title 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
    <section id="title-3" name="title 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
        <section id="title-4" name="title 4">
            <title>
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
Test return to existing, higher-level section (Title 4).

Title 1
=======
Paragraph 1.

Title 2
-------
Paragraph 2.

Title 3
```````
Paragraph 3.

Title 4
-------
Paragraph 4.
""",
"""\
<document>
    <paragraph>
        Test return to existing, higher-level section (Title 4).
    <section id="title-1" name="title 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title>
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title>
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
Test bad subsection order (Title 4).

Title 1
=======
Paragraph 1.

Title 2
-------
Paragraph 2.

Title 3
=======
Paragraph 3.

Title 4
```````
Paragraph 4.
""",
"""\
<document>
    <paragraph>
        Test bad subsection order (Title 4).
    <section id="title-1" name="title 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
    <section id="title-3" name="title 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
        <system_message level="4" type="SEVERE">
            <paragraph>
                Title level inconsistent at line 15:
            <literal_block>
                Title 4
                ```````
        <paragraph>
            Paragraph 4.
"""],
["""\
Test bad subsection order (Title 4, with overlines).

=======
Title 1
=======
Paragraph 1.

-------
Title 2
-------
Paragraph 2.

=======
Title 3
=======
Paragraph 3.

```````
Title 4
```````
Paragraph 4.
""",
"""\
<document>
    <paragraph>
        Test bad subsection order (Title 4, with overlines).
    <section id="title-1" name="title 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
    <section id="title-3" name="title 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
        <system_message level="4" type="SEVERE">
            <paragraph>
                Title level inconsistent at line 19:
            <literal_block>
                ```````
                Title 4
                ```````
        <paragraph>
            Paragraph 4.
"""],
["""\
Title containing *inline* ``markup``
====================================

Paragraph.
""",
"""\
<document>
    <section id="title-containing-inline-markup" name="title containing inline markup">
        <title>
            Title containing \n\
            <emphasis>
                inline
             \n\
            <literal>
                markup
        <paragraph>
            Paragraph.
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
