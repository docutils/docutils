#! /usr/bin/env python

"""
:Author: David Goodger, Dmitry Jemerov
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for `docutils.transforms.parts.SectNum` (via
`docutils.transforms.universal.LastReaderPending`).
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.universal import LastReaderPending
from docutils.transforms.references import Substitutions
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['section_numbers'] = ((Substitutions, LastReaderPending,), [
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
"""\
<document>
    <section autonum_origtitle="Title 1" autonum_prefix="" id="title-1" name="title 1">
        <title>
            1. Title 1
        <paragraph>
            Paragraph 1.
        <section autonum_origtitle="Title 2" autonum_prefix="1." id="title-2" name="title 2">
            <title>
                1.1. Title 2
            <paragraph>
                Paragraph 2.
            <section autonum_origtitle="Title 3" autonum_prefix="1.1." id="title-3" name="title 3">
                <title>
                    1.1.1. Title 3
                <paragraph>
                    Paragraph 3.
        <section autonum_origtitle="Title 4" autonum_prefix="1." id="title-4" name="title 4">
            <title>
                1.2. Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
.. sectnum::

**Bold Title**
==============
Paragraph 1.
""",
"""\
<document>
    <section autonum_origtitle="<strong>Bold Title</strong>" autonum_prefix="" id="bold-title" name="bold title">
        <title>
            1. \n\
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
"""\
<document>
    <section autonum_origtitle="Title 1" autonum_prefix="" id="title-1" name="title 1">
        <title>
            1. Title 1
        <paragraph>
            Paragraph 1.
        <section autonum_origtitle="Title 2" autonum_prefix="1." id="title-2" name="title 2">
            <title>
                1.1. Title 2
            <paragraph>
                Paragraph 2.
            <section id="title-3" name="title 3">
                <title>
                    Title 3
                <paragraph>
                    Paragraph 3.
        <section autonum_origtitle="Title 4" autonum_prefix="1." id="title-4" name="title 4">
            <title>
                1.2. Title 4
            <paragraph>
                Paragraph 4.
"""]
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
