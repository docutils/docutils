#!/usr/bin/env python3
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
Test for section headings in CommonMark parsers.
Cf. the `CommonMark Specification <https://spec.commonmark.org/>`__
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))


from docutils.frontend import get_default_settings
from docutils.parsers.recommonmark_wrapper import Parser
from docutils.utils import new_document


class RecommonmarkParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['section_headers'] = [
["""\
The Title
=========
Paragraph.
""",
r"""<document source="test data">
    <section ids="the-title" names="the\ title">
        <title>
            The Title
        <paragraph>
            Paragraph.
"""],
["""\
Another Section Title
=====================
Paragraph (no blank line required).
""",
r"""<document source="test data">
    <section ids="another-section-title" names="another\ section\ title">
        <title>
            Another Section Title
        <paragraph>
            Paragraph (no blank line required).
"""],
["""\
Paragraph.

Title
=====

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <section ids="title" names="title">
        <title>
            Title
        <paragraph>
            Paragraph.
"""],
# ["""\
# Test unexpected section titles.
#
# * Title
#   =====
#
#   Paragraph.
# """,
# """\
# <document source="test data">
#     <paragraph>
#         Test unexpected section titles.
#     <system_message level="3" source="test data" type="ERROR">
#         <paragraph>
#             Parsing with "recommonmark" returned the error:
#             'NoneType' object has no attribute 'note_implicit_target'
# """],
["""\
Title
==

A short underline is no problem in CommonMark
""",
"""\
<document source="test data">
    <section ids="title" names="title">
        <title>
            Title
        <paragraph>
            A short underline is no problem in CommonMark
"""],
["""\
Test return to existing, highest-level section (Title 3).

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
r"""<document source="test data">
    <paragraph>
        Test return to existing, highest-level section (Title 3).
    <section ids="title-1" names="title\ 1">
        <title>
            Title 1
        <paragraph>
            Paragraph 1.
        <section ids="title-2" names="title\ 2">
            <title>
                Title 2
            <paragraph>
                Paragraph 2.
    <section ids="title-3" names="title\ 3">
        <title>
            Title 3
        <paragraph>
            Paragraph 3.
        <section ids="title-4" names="title\ 4">
            <title>
                Title 4
            <paragraph>
                Paragraph 4.
"""],
["""\
Test bad subsection order.

## Title 1

## Title 2

# Title 3

#### Title 4

### Title 5
""",
r"""<document source="test data">
    <paragraph>
        Test bad subsection order.
    <section ids="title-1" names="title\ 1">
        <title>
            Title 1
    <section ids="title-2" names="title\ 2">
        <title>
            Title 2
    <section ids="title-3" names="title\ 3">
        <title>
            Title 3
        <section ids="title-4" names="title\ 4">
            <title>
                Title 4
        <section ids="title-5" names="title\ 5">
            <title>
                Title 5
"""],
["""\
Title containing *inline* ``markup``
====================================

Paragraph.
""",
"""\
<document source="test data">
    <section ids="title-containing-inline-markup" names="title\\ containing\\ inline\\ markup">
        <title>
            Title containing \n\
            <emphasis>
                inline
             \n\
            <literal classes="code">
                markup
        <paragraph>
            Paragraph.
"""],
["""\
1. Not a numbered title but an enumerated list
==============================================

Paragraph.
""",
"""\
<document source="test data">
    <enumerated_list>
        <list_item>
            <paragraph>
                Not a numbered title but an enumerated list
                ==============================================
    <paragraph>
        Paragraph.
"""],
["""\
ABC
===

Short title.
""",
"""\
<document source="test data">
    <section ids="abc" names="abc">
        <title>
            ABC
        <paragraph>
            Short title.
"""],
["""\
Empty Section
=============
""",
"""\
<document source="test data">
    <section ids="empty-section" names="empty\\ section">
        <title>
            Empty Section
"""],
]


if __name__ == '__main__':
    unittest.main()
