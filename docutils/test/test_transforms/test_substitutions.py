#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
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
    <substitution_definition name="biohazard">
        <image alt="biohazard" uri="biohazard.png">
"""],
["""\
Here's an |unknown| substitution.
""",
"""\
<document source="test data">
    <paragraph>
        Here's an \n\
        <problematic id="id2" refid="id1">
            |unknown|
         substitution.
    <system_message backrefs="id2" id="id1" level="3" line="1" source="test data" type="ERROR">
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
    <substitution_definition name="eacute">
        \u00E9
    <substitution_definition name="Eacute">
        \u00C9
    <paragraph>
        \u00C9
        t
        \u00E9
        , and even \n\
        \u00C9
        .
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
