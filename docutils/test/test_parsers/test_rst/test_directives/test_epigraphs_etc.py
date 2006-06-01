#! /usr/bin/env python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the "epigraph", "highlights", and "pull_quote" directives.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['epigraphs'] = [
["""\
.. epigraph::
""",
"""\
<document source="test data">
"""],
["""\
.. epigraph:: arguments
""",
"""\
<document source="test data">
    <block_quote classes="epigraph">
        <paragraph>
            arguments
"""],
["""\
.. epigraph::

   Body text.
""",
"""\
<document source="test data">
    <block_quote classes="epigraph">
        <paragraph>
            Body text.
"""],
["""\
.. epigraph::

   A quote

   -- attribution

   Another quote

   -- attribution
""",
"""\
<document source="test data">
    <block_quote classes="epigraph">
        <paragraph>
            A quote
        <attribution>
            attribution
    <block_quote classes="epigraph">
        <paragraph>
            Another quote
        <attribution>
            attribution
"""],
]

totest['highlights'] = [
["""\
.. highlights::

   Some text.
""",
"""\
<document source="test data">
    <block_quote classes="highlights">
        <paragraph>
            Some text.
"""],
]

totest['pull-quote'] = [
["""\
.. pull-quote::

   Some text.
""",
"""\
<document source="test data">
    <block_quote classes="pull-quote">
        <paragraph>
            Some text.
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
