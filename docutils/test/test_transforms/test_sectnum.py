#! /usr/bin/env python

# Author: David Goodger, Dmitry Jemerov
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for `docutils.transforms.parts.SectNum` (via
`docutils.transforms.universal.LastReaderPending`).
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.references import Substitutions
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['section_numbers'] = ((Substitutions,), [
["""\
.. sectnum::

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
u"""\
<document source="test data">
    <section id="title-1" name="title 1">
        <title auto="1">
            <generated class="sectnum">
                1\u00a0\u00a0\u00a0
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title auto="1">
                <generated class="sectnum">
                    1.1\u00a0\u00a0\u00a0
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title auto="1">
                    <generated class="sectnum">
                        1.1.1\u00a0\u00a0\u00a0
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title auto="1">
                <generated class="sectnum">
                    1.2\u00a0\u00a0\u00a0
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
.. sectnum::

**Bold Title**
==============
Paragraph 1.
""",
u"""\
<document source="test data">
    <section id="bold-title" name="bold title">
        <title auto="1">
            <generated class="sectnum">
                1\u00a0\u00a0\u00a0
            <strong>
                Bold Title
        <paragraph>
            Paragraph 1.
"""],
["""\
.. sectnum:: :depth: 2

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
u"""\
<document source="test data">
    <section id="title-1" name="title 1">
        <title auto="1">
            <generated class="sectnum">
                1\u00a0\u00a0\u00a0
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title auto="1">
                <generated class="sectnum">
                    1.1\u00a0\u00a0\u00a0
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title>
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title auto="1">
                <generated class="sectnum">
                    1.2\u00a0\u00a0\u00a0
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
.. contents::
.. sectnum:: :depth: 2

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
u"""\
<document source="test data">
    <topic class="contents" id="contents" name="contents">
        <title>
            Contents
        <bullet_list class="auto-toc">
            <list_item>
                <paragraph>
                    <reference id="id1" refid="title-1">
                        <generated class="sectnum">
                            1\u00a0\u00a0\u00a0
                        Title 1
                <bullet_list class="auto-toc">
                    <list_item>
                        <paragraph>
                            <reference id="id2" refid="title-2">
                                <generated class="sectnum">
                                    1.1\u00a0\u00a0\u00a0
                                Title 2
                        <bullet_list>
                            <list_item>
                                <paragraph>
                                    <reference id="id3" refid="title-3">
                                        Title 3
                    <list_item>
                        <paragraph>
                            <reference id="id4" refid="title-4">
                                <generated class="sectnum">
                                    1.2\u00a0\u00a0\u00a0
                                Title 4
    <section id="title-1" name="title 1">
        <title auto="1" refid="id1">
            <generated class="sectnum">
                1\u00a0\u00a0\u00a0
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title auto="1" refid="id2">
                <generated class="sectnum">
                    1.1\u00a0\u00a0\u00a0
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title refid="id3">
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title auto="1" refid="id4">
                <generated class="sectnum">
                    1.2\u00a0\u00a0\u00a0
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
.. sectnum::
   :prefix: Arbitrary-

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
u"""\
<document source="test data">
    <section id="title-1" name="title 1">
        <title auto="1">
            <generated class="sectnum">
                Arbitrary-1\u00a0\u00a0\u00a0
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title auto="1">
                <generated class="sectnum">
                    Arbitrary-1.1\u00a0\u00a0\u00a0
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title auto="1">
                    <generated class="sectnum">
                        Arbitrary-1.1.1\u00a0\u00a0\u00a0
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title auto="1">
                <generated class="sectnum">
                    Arbitrary-1.2\u00a0\u00a0\u00a0
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
.. sectnum::
   :start: 3
   
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
u"""\
<document source="test data">
    <section id="title-1" name="title 1">
        <title auto="1">
            <generated class="sectnum">
                3\u00a0\u00a0\u00a0
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title auto="1">
                <generated class="sectnum">
                    3.1\u00a0\u00a0\u00a0
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title auto="1">
                    <generated class="sectnum">
                        3.1.1\u00a0\u00a0\u00a0
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title auto="1">
                <generated class="sectnum">
                    3.2\u00a0\u00a0\u00a0
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
.. sectnum::
   :prefix: (5.9.
   :suffix: )
   :start: 3
   
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
u"""\
<document source="test data">
    <section id="title-1" name="title 1">
        <title auto="1">
            <generated class="sectnum">
                (5.9.3)\u00a0\u00a0\u00a0
            Title 1
        <paragraph>
            Paragraph 1.
        <section id="title-2" name="title 2">
            <title auto="1">
                <generated class="sectnum">
                    (5.9.3.1)\u00a0\u00a0\u00a0
                Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title auto="1">
                    <generated class="sectnum">
                        (5.9.3.1.1)\u00a0\u00a0\u00a0
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section id="title-4" name="title 4">
            <title auto="1">
                <generated class="sectnum">
                    (5.9.3.2)\u00a0\u00a0\u00a0
                Title 4
            <paragraph>
                Paragraph 4.
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
