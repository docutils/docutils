#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "unicode" directive.
"""

from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['unicode'] = [
["""
Insert an em-dash (|mdash|), a copyright symbol (|copy|), a non-breaking
space (|nbsp|), a backwards-not-equals (|bne|), and a captial omega (|Omega|).

.. |mdash| unicode:: 0x02014
.. |copy| unicode:: \\u00A9
.. |nbsp| unicode:: &#x000A0;
.. |bne| unicode:: U0003D U020E5
.. |Omega| unicode:: U+003A9
""",
u"""\
<document source="test data">
    <paragraph>
        Insert an em-dash (
        <substitution_reference refname="mdash">
            mdash
        ), a copyright symbol (
        <substitution_reference refname="copy">
            copy
        ), a non-breaking
        space (
        <substitution_reference refname="nbsp">
            nbsp
        ), a backwards-not-equals (
        <substitution_reference refname="bne">
            bne
        ), and a captial omega (
        <substitution_reference refname="Omega">
            Omega
        ).
    <substitution_definition name="mdash">
        \u2014
    <substitution_definition name="copy">
        \u00A9
    <substitution_definition name="nbsp">
        \u00A0
    <substitution_definition name="bne">
        =
        \u20e5
    <substitution_definition name="Omega">
        \u03a9
"""],
["""
Bad input:

.. |empty| unicode::
.. |not hex| unicode:: 0xHEX
.. |not all hex| unicode:: UABCX
.. unicode:: not in a substitution definition
""",
"""\
<document source="test data">
    <paragraph>
        Bad input:
    <system_message level="3" line="4" source="test data" type="ERROR">
        <paragraph>
            Error in "unicode" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            unicode::
    <system_message level="2" line="4" source="test data" type="WARNING">
        <paragraph>
            Substitution definition "empty" empty or invalid.
        <literal_block xml:space="preserve">
            .. |empty| unicode::
    <substitution_definition name="not hex">
        0xHEX
    <substitution_definition name="not all hex">
        UABCX
    <system_message level="3" line="7" source="test data" type="ERROR">
        <paragraph>
            Invalid context: the "unicode" directive can only be used within a substitution definition.
        <literal_block xml:space="preserve">
            .. unicode:: not in a substitution definition
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
