#! /usr/bin/env python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for admonitions.py directives in German document.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport

def suite():
    settings = {'language_code': 'de'}
    s = DocutilsTestSupport.ParserTestSuite(suite_settings=settings)
    s.generateTests(totest)
    return s

totest = {}

totest['admonitions'] = [
["""\
.. Achtung:: Directives at large.

.. Notiz:: :name: mynote
   :class: testnote

   Admonitions support the generic "name" and "class" options.

.. Tipp:: 15% if the
   service is good.

.. Hinweis:: It's bigger than a bread box.

- .. WARNUNG:: Strong prose may provoke extreme mental exertion.
     Reader discretion is strongly advised.
- .. Fehler:: Does not compute.

.. Vorsicht::

   Don't take any wooden nickels.

.. GEFAHR:: Mad scientist at work!

.. Wichtig::
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
.. Notiz:: One-line notes.
.. Notiz:: One after the other.
.. Notiz:: No blank lines in-between.
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
.. Notiz:: Content before options
   is possible too.
   :class: mynote

.. Notiz:: :strong:`a role is not an option`.
   :name: role not option

.. Notiz:: a role is
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
        <system_message level="1" line="5" source="test data" type="INFO">
            <paragraph>
                No role entry for "strong" in module "docutils.parsers.rst.languages.de".
                Using English fallback for role "strong".
    <note>
        <paragraph>
            a role is
            <strong>
                not an option
            , even if its starts a line.
"""],
["""\
.. Notiz::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "Notiz" directive; none found.
        <literal_block xml:space="preserve">
            .. Notiz::
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
            
               Generic admonitions require a title.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
