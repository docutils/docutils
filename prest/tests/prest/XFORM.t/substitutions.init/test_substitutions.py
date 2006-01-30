#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision: 3807 $
# Date: $Date: 2005-10-06 10:35:16 -0500 (Thu, 06 Oct 2005) $
# Copyright: This module has been placed in the public domain.

"""
Tests for docutils.transforms.references.Substitutions.
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

totest['substitutions'] = ((Substitutions,), [
["""\
The |biohazard| symbol is deservedly scary-looking.

.. |biohazard| image:: biohazard.png
""",
"""\
<document source="test data">
    <paragraph>
        The \n\
        <image alt="biohazard" uri="biohazard.png">
         symbol is deservedly scary-looking.
    <substitution_definition names="biohazard">
        <image alt="biohazard" uri="biohazard.png">
"""],
["""\
Here's an |unknown| substitution.
""",
"""\
<document source="test data">
    <paragraph>
        Here's an \n\
        <problematic ids="id2" refid="id1">
            |unknown|
         substitution.
    <system_message backrefs="id2" ids="id1" level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Undefined substitution referenced: "unknown".
"""],
[u"""\
Substitutions support case differences:

.. |eacute| replace:: \u00E9
.. |Eacute| replace:: \u00C9

|Eacute|\\t\\ |eacute|, and even |EACUTE|.
""",
u"""\
<document source="test data">
    <paragraph>
        Substitutions support case differences:
    <substitution_definition names="eacute">
        \u00E9
    <substitution_definition names="Eacute">
        \u00C9
    <paragraph>
        \u00C9
        t
        \u00E9
        , and even \n\
        \u00C9
        .
"""],
[u"""\
Indirect substitution definitions with multiple references:

|substitute| my coke for gin
|substitute| you for my mum
at least I'll get my washing done

.. |substitute| replace:: |replace|
.. |replace| replace:: swap
""",
u"""\
<document source="test data">
    <paragraph>
        Indirect substitution definitions with multiple references:
    <paragraph>
        swap
         my coke for gin
        swap
         you for my mum
        at least I'll get my washing done
    <substitution_definition names="substitute">
        swap
    <substitution_definition names="replace">
        swap
"""],
])

totest['unicode'] = ((Substitutions,), [
["""\
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
        \u2014
        ), a copyright symbol (
        \u00a9
        ), a non-breaking
        space (
        \u00a0
        ), a backwards-not-equals (
        =
        \u20e5
        ), and a captial omega (
        \u03a9
        ).
    <substitution_definition names="mdash">
        \u2014
    <substitution_definition names="copy">
        \u00a9
    <substitution_definition names="nbsp">
        \u00a0
    <substitution_definition names="bne">
        =
        \u20e5
    <substitution_definition names="Omega">
        \u03a9
"""],
["""
Testing comments and extra text.

Copyright |copy| 2003, |BogusMegaCorp (TM)|.

.. |copy| unicode:: 0xA9 .. copyright sign
.. |BogusMegaCorp (TM)| unicode:: BogusMegaCorp U+2122
   .. with trademark sign
""",
u"""\
<document source="test data">
    <paragraph>
        Testing comments and extra text.
    <paragraph>
        Copyright \n\
        \u00a9
         2003, \n\
        BogusMegaCorp
        \u2122
        .
    <substitution_definition names="copy">
        \u00a9
    <substitution_definition names="BogusMegaCorp (TM)">
        BogusMegaCorp
        \u2122
"""],
["""\
Insert an em-dash |---| automatically trimming whitespace.
Some substitutions |TM| only need trimming on one side.

.. |---| unicode:: U+02014
   :trim:
.. |TM| unicode:: U+02122
   :ltrim:
""",
u"""\
<document source="test data">
    <paragraph>
        Insert an em-dash
        \u2014
        automatically trimming whitespace.
        Some substitutions
        \u2122
         only need trimming on one side.
    <substitution_definition ltrim="1" names="---" rtrim="1">
        \u2014
    <substitution_definition ltrim="1" names="TM">
        \u2122
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
