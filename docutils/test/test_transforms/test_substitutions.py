#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

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
<document>
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
<document>
    <paragraph>
        Here's an \n\
        <problematic id="id2" refid="id1">
            |unknown|
         substitution.
    <system_message backrefs="id2" id="id1" level="3" type="ERROR">
        <paragraph>
            Undefined substitution referenced: "unknown".
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
