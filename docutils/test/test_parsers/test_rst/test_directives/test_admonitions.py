#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for admonitions.py directives.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).parents[4]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):
    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(output, case_expected)


totest = {}

totest['admonitions'] = [
["""\
.. Attention:: Directives at large.

.. Note:: :name: mynote
   :class: testnote

   Admonitions support the generic "name" and "class" options.

.. Tip:: 15% if the
   service is good.

.. Hint:: It's bigger than a bread box.

- .. WARNING:: Strong prose may provoke extreme mental exertion.
     Reader discretion is strongly advised.
- .. Error:: Does not compute.

.. Caution::

   Don't take any wooden nickels.

.. DANGER:: Mad scientist at work!

.. Important::
   - Wash behind your ears.
   - Clean up your room.
   - Call your mother.
   - Back up your data.
""",
"""\
<document source="test data">
    <attention>
        <paragraph>
            Directives at large.
    <note classes="testnote" ids="mynote" names="mynote">
        <paragraph>
            Admonitions support the generic "name" and "class" options.
    <tip>
        <paragraph>
            15% if the
            service is good.
    <hint>
        <paragraph>
            It's bigger than a bread box.
    <bullet_list bullet="-">
        <list_item>
            <warning>
                <paragraph>
                    Strong prose may provoke extreme mental exertion.
                    Reader discretion is strongly advised.
        <list_item>
            <error>
                <paragraph>
                    Does not compute.
    <caution>
        <paragraph>
            Don't take any wooden nickels.
    <danger>
        <paragraph>
            Mad scientist at work!
    <important>
        <bullet_list bullet="-">
            <list_item>
                <paragraph>
                    Wash behind your ears.
            <list_item>
                <paragraph>
                    Clean up your room.
            <list_item>
                <paragraph>
                    Call your mother.
            <list_item>
                <paragraph>
                    Back up your data.
"""],
["""\
.. note:: One-line notes.
.. note:: One after the other.
.. note:: No blank lines in-between.
""",
"""\
<document source="test data">
    <note>
        <paragraph>
            One-line notes.
    <note>
        <paragraph>
            One after the other.
    <note>
        <paragraph>
            No blank lines in-between.
"""],
["""\
.. note:: Content before options
   is possible too.
   :class: mynote

.. note:: :strong:`a role is not an option`.
   :name: role not option

.. note:: a role is
   :strong:`not an option`, even if its starts a line.
""",
"""\
<document source="test data">
    <note classes="mynote">
        <paragraph>
            Content before options
            is possible too.
    <note ids="role-not-option" names="role\\ not\\ option">
        <paragraph>
            <strong>
                a role is not an option
            .
    <note>
        <paragraph>
            a role is
            <strong>
                not an option
            , even if its starts a line.
"""],
["""\
.. note::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "note" directive; none found.
        <literal_block xml:space="preserve">
            .. note::
"""],
["""\
.. admonition:: Admonition

   This is a generic admonition.
""",
"""\
<document source="test data">
    <admonition classes="admonition-admonition">
        <title>
            Admonition
        <paragraph>
            This is a generic admonition.
"""],
["""\
.. admonition:: And, by the way...

   You can make up your own admonition too.
""",
"""\
<document source="test data">
    <admonition classes="admonition-and-by-the-way">
        <title>
            And, by the way...
        <paragraph>
            You can make up your own admonition too.
"""],
["""\
.. admonition:: Admonition
   :class: emergency
   :name: reference name

   Test the "class" override.
""",
"""\
<document source="test data">
    <admonition classes="emergency" ids="reference-name" names="reference\\ name">
        <title>
            Admonition
        <paragraph>
            Test the "class" override.
"""],
["""\
.. admonition::

   Generic admonitions require a title.
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "admonition" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            .. admonition::
            \n\
               Generic admonitions require a title.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main()
