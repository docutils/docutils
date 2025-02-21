#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.frontmatter.DocInfo.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.transforms.frontmatter import DocInfo
from docutils.transforms.universal import FilterMessages, TestMessages
from docutils.utils import new_document


class TransformTestCase(unittest.TestCase):

    maxDiff = None
    settings = get_default_settings(Parser)
    settings.warning_stream = ''
    parser = Parser()

    def check_output(self, samples, settings):
        # yield (output, expected) for each test sample
        for key, (transforms, cases) in samples.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'samples[{key!r}][{casenum}]'):
                    document = new_document('test data', settings)
                    self.parser.parse(case_input, document)
                    # Don't do a ``populate_from_components()`` because that
                    # would enable the Transformer's default transforms.
                    document.transformer.add_transforms(transforms)
                    document.transformer.add_transform(TestMessages)
                    # Filter INFOs with increased priority so that
                    # messages added by `TestMessages` are filtered, too:
                    document.transformer.add_transform(FilterMessages, 890)
                    document.transformer.apply_transforms()
                    self.assertEqual(case_expected, document.pformat())

    def test_transforms(self):
        self.check_output(totest, self.settings)

    def test_transforms_de(self):
        settings = self.settings.copy()
        settings.language_code = 'de'
        self.check_output(totest_de, settings)

    def test_transforms_ru(self):
        settings = self.settings.copy()
        settings.language_code = 'ru'
        self.check_output(totest_ru, settings)


totest = {}
totest_de = {}
totest_ru = {}

totest['bibliographic_field_lists'] = ((DocInfo,), [
["""\
.. Bibliographic element extraction.

:Abstract:
    There can only be one abstract.

    It is automatically moved to the end of the other bibliographic elements.

:Author: E. *Xample*

:Version: 1

:Date: 2001-08-11

:Parameter i: integer
""",
"""\
<document source="test data">
    <docinfo>
        <author>
            E. \n\
            <emphasis>
                Xample
        <version>
            1
        <date>
            2001-08-11
        <field classes="parameter-i">
            <field_name>
                Parameter i
            <field_body>
                <paragraph>
                    integer
    <topic classes="abstract">
        <title>
            Abstract
        <paragraph>
            There can only be one abstract.
        <paragraph>
            It is automatically moved to the end of the other bibliographic elements.
    <comment xml:space="preserve">
        Bibliographic element extraction.
"""],
["""\
.. Bibliographic element extraction.

:Abstract: Abstract 1.
:Author: Me
:Address: 123 My Street
          Example, EX
:Contact: me@my.org
:Version: 1
:Abstract: Abstract 2 (should generate a warning).
:Date: 2001-08-11
:Parameter i: integer
""",
"""\
<document source="test data">
    <docinfo>
        <author>
            Me
        <address xml:space="preserve">
            123 My Street
            Example, EX
        <contact>
            <reference refuri="mailto:me@my.org">
                me@my.org
        <version>
            1
        <field classes="abstract">
            <field_name>
                Abstract
            <field_body>
                <paragraph>
                    Abstract 2 (should generate a warning).
                <system_message level="2" line="9" source="test data" type="WARNING">
                    <paragraph>
                        There can only be one "Abstract" field.
        <date>
            2001-08-11
        <field classes="parameter-i">
            <field_name>
                Parameter i
            <field_body>
                <paragraph>
                    integer
    <topic classes="abstract">
        <title>
            Abstract
        <paragraph>
            Abstract 1.
    <comment xml:space="preserve">
        Bibliographic element extraction.
"""],
["""\
:Author: - must be a paragraph
:Status: a *simple* paragraph
:Date: But only one

       paragraph.
:Version:

.. and not empty either
""",
"""\
<document source="test data">
    <docinfo>
        <field classes="author">
            <field_name>
                Author
            <field_body>
                <bullet_list bullet="-">
                    <list_item>
                        <paragraph>
                            must be a paragraph
                <system_message level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Bibliographic field "Author"
                        must contain a single <paragraph>, not a <bullet_list>.
        <status>
            a \n\
            <emphasis>
                simple
             paragraph
        <field classes="date">
            <field_name>
                Date
            <field_body>
                <paragraph>
                    But only one
                <paragraph>
                    paragraph.
                <system_message level="2" line="3" source="test data" type="WARNING">
                    <paragraph>
                        Bibliographic field "Date"
                        must contain a single <paragraph>, not [<paragraph>, <paragraph>].
        <field classes="version">
            <field_name>
                Version
            <field_body>
                <system_message level="2" line="6" source="test data" type="WARNING">
                    <paragraph>
                        Cannot extract empty bibliographic field "Version".
    <comment xml:space="preserve">
        and not empty either
"""],
["""\
:Authors: Me, Myself, **I**
:Authors: PacMan; Ms. PacMan; PacMan, Jr.
:Authors:
    Here

    There

    *Everywhere*
:Authors: - First
          - Second
          - Third
""",
"""\
<document source="test data">
    <docinfo>
        <authors>
            <author>
                Me
            <author>
                Myself
            <author>
                I
        <authors>
            <author>
                PacMan
            <author>
                Ms. PacMan
            <author>
                PacMan, Jr.
        <authors>
            <author>
                Here
            <author>
                There
            <author>
                <emphasis>
                    Everywhere
        <authors>
            <author>
                First
            <author>
                Second
            <author>
                Third
"""],
["""\
:Authors: Only One
:Authors: One, Only;
""",
"""\
<document source="test data">
    <docinfo>
        <authors>
            <author>
                Only One
        <authors>
            <author>
                One, Only
"""],
[r""":Authors: Me\, Myself; **I**
:Authors: Pac\;Man\\; Ms. Pac\Man; Pac\ Man, Jr.
:Authors:
    Here

    The\re

    *Every\ where*
:Authors: - First\\
          - Se\ cond
          - Thir\d
""",
"""\
<document source="test data">
    <docinfo>
        <authors>
            <author>
                Me, Myself
            <author>
                I
        <authors>
            <author>
                Pac;Man\\
            <author>
                Ms. PacMan
            <author>
                PacMan, Jr.
        <authors>
            <author>
                Here
            <author>
                There
            <author>
                <emphasis>
                    Everywhere
        <authors>
            <author>
                First\\
            <author>
                Second
            <author>
                Third
"""],
["""\
:Authors:

:Authors: A. Einstein
          B. Shaw

:Authors:
    -
    -

:Authors:
    - One

    Two

:Authors:
    - One

      Two
""",
"""\
<document source="test data">
    <docinfo>
        <field classes="authors">
            <field_name>
                Authors
            <field_body>
                <system_message level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Cannot extract empty bibliographic field "Authors".
        <field classes="authors">
            <field_name>
                Authors
            <field_body>
                <enumerated_list enumtype="upperalpha" prefix="" suffix=".">
                    <list_item>
                        <paragraph>
                            Einstein
                    <list_item>
                        <paragraph>
                            Shaw
                <system_message level="2" line="3" source="test data" type="WARNING">
                    <paragraph>
                        Cannot extract "Authors" from bibliographic field:
                        Bibliographic field "Authors" must contain either
                         a single paragraph (with author names separated by a character from the set ";,"),
                         multiple paragraphs (one per author),
                         or a bullet list with one author name per item.
                        Note: Leading initials can cause (mis)recognizing names as enumerated list.
        <field classes="authors">
            <field_name>
                Authors
            <field_body>
                <bullet_list bullet="-">
                    <list_item>
                    <list_item>
                <system_message level="2" line="6" source="test data" type="WARNING">
                    <paragraph>
                        Cannot extract "Authors" from bibliographic field:
                        Bibliographic field "Authors" must contain either
                         a single paragraph (with author names separated by a character from the set ";,"),
                         multiple paragraphs (one per author),
                         or a bullet list with one author name per item.
                        Note: Leading initials can cause (mis)recognizing names as enumerated list.
        <field classes="authors">
            <field_name>
                Authors
            <field_body>
                <bullet_list bullet="-">
                    <list_item>
                        <paragraph>
                            One
                <paragraph>
                    Two
                <system_message level="2" line="10" source="test data" type="WARNING">
                    <paragraph>
                        Cannot extract "Authors" from bibliographic field:
                        Bibliographic field "Authors" must contain either
                         a single paragraph (with author names separated by a character from the set ";,"),
                         multiple paragraphs (one per author),
                         or a bullet list with one author name per item.
                        Note: Leading initials can cause (mis)recognizing names as enumerated list.
        <field classes="authors">
            <field_name>
                Authors
            <field_body>
                <bullet_list bullet="-">
                    <list_item>
                        <paragraph>
                            One
                        <paragraph>
                            Two
                <system_message level="2" line="15" source="test data" type="WARNING">
                    <paragraph>
                        Cannot extract "Authors" from bibliographic field:
                        Bibliographic field "Authors" must contain either
                         a single paragraph (with author names separated by a character from the set ";,"),
                         multiple paragraphs (one per author),
                         or a bullet list with one author name per item.
                        Note: Leading initials can cause (mis)recognizing names as enumerated list.
"""],
["""\
.. RCS keyword extraction.

:Status: (some text) $""" + """RCSfile: test_docinfo.py,v $ (more text)
:Date: (some text) $""" + """Date: 2002/10/08 01:34:23 $ (more text)
:Date: (some text) $""" + """Date: 2005-03-26T16:21:28.693201Z $ (more text)
:Version: (some text) $""" + """Revision: 1.1 $ (more text)
""",
"""\
<document source="test data">
    <docinfo>
        <status>
            (some text) test_docinfo.py (more text)
        <date>
            (some text) 2002-10-08 (more text)
        <date>
            (some text) 2005-03-26 (more text)
        <version>
            (some text) 1.1 (more text)
    <comment xml:space="preserve">
        RCS keyword extraction.
"""],
])

totest_de['bibliographic_field_lists_de'] = ((DocInfo,), [
["""\
.. Bibliographic element extraction for a German document.

:Zusammenfassung: Abstract 1.
:Autor: Me
:Adresse: 123 My Street
          Example, EX
:Kontakt: me@my.org
:Version: 1
:Datum: 2001-08-11
:Parameter i: integer
""",
"""\
<document source="test data">
    <docinfo>
        <author>
            Me
        <address xml:space="preserve">
            123 My Street
            Example, EX
        <contact>
            <reference refuri="mailto:me@my.org">
                me@my.org
        <version>
            1
        <date>
            2001-08-11
        <field classes="parameter-i">
            <field_name>
                Parameter i
            <field_body>
                <paragraph>
                    integer
    <topic classes="abstract">
        <title>
            Zusammenfassung
        <paragraph>
            Abstract 1.
    <comment xml:space="preserve">
        Bibliographic element extraction for a German document.
"""]
])

totest_ru['bibliographic_field_lists_ru'] = ((DocInfo,), [
["""\
.. Bibliographic element extraction for a Russian document.

:аннотация: Abstract 1.
:автор: Me
:адрес: 123 My Street
          Example, EX
:контакт: me@my.org
:версия: 1
:дата: 2001-08-11
:Parameter i: integer
""",
"""\
<document source="test data">
    <docinfo>
        <author>
            Me
        <address xml:space="preserve">
            123 My Street
            Example, EX
        <contact>
            <reference refuri="mailto:me@my.org">
                me@my.org
        <version>
            1
        <date>
            2001-08-11
        <field classes="parameter-i">
            <field_name>
                Parameter i
            <field_body>
                <paragraph>
                    integer
    <topic classes="abstract">
        <title>
            Аннотация
        <paragraph>
            Abstract 1.
    <comment xml:space="preserve">
        Bibliographic element extraction for a Russian document.
"""]
])


if __name__ == '__main__':
    unittest.main()
