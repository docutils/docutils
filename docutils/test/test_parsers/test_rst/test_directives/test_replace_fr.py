#! /usr/bin/env python

# $Id: test_replace.py 4667 2006-07-12 21:40:56Z wiemann $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "replace" directive.
Test in french (not default/fallback language).
"""

from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite(suite_settings={'language_code':'fr'})
    s.generateTests(totest)
    return s

totest = {}

totest['replace'] = [
["""\
Test directive containing french role exposant (superscript).

.. |Na+| remplace:: Na\ :exp:`+`

Le |Na+| est l'ion sodium.
""",
"""\
<document source="test data">
    <paragraph>
        Test directive containing french role exposant (superscript).
    <substitution_definition names="Na+">
        Na
        <superscript>
            +
    <paragraph>
        Le \n\
        <substitution_reference refname="Na+">
            Na+
         est l\'ion sodium.
"""],
["""\
Test directive containing english role superscript.
BUG 1830380: the ERROR is an ERROR and the WARNING a followup to the ERROR

.. |Na+| remplace:: Na\ :sup:`+`

Le |Na+| est l'ion sodium.
""",
"""\
<document source="test data">
    <paragraph>
        Test directive containing english role superscript.
        BUG 1830380: the ERROR is an ERROR and the WARNING a followup to the ERROR
    <system_message level="1" line="4" source="test data" type="INFO">
        <paragraph>
            No role entry for "sup" in module "docutils.parsers.rst.languages.fr".
            Using English fallback for role "sup".
    <system_message level="3" line="4" source="test data" type="ERROR">
        <paragraph>
            Error in "remplace" directive: may contain a single paragraph only.
    <system_message level="2" line="4" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "Na+" empty or invalid.
        <literal_block xml:space="preserve">
            .. |Na+| remplace:: Na\\ :sup:`+`
    <paragraph>
        Le \n\
        <substitution_reference refname="Na+">
            Na+
         est l\'ion sodium.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
