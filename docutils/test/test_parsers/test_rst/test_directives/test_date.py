#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the misc.py "date" directive.
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test import DocutilsTestSupport
import time

from docutils.io import _locale_encoding  # noqa


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['date'] = [
["""\
.. |date| date::

Today's date is |date|.
""",
"""\
<document source="test data">
    <substitution_definition names="date">
        %s
    <paragraph>
        Today's date is \n\
        <substitution_reference refname="date">
            date
        .
""" % time.strftime('%Y-%m-%d')],
["""\
.. |date| date:: %a, %d %b %Y
""",
"""\
<document source="test data">
    <substitution_definition names="date">
        %s
""" % time.strftime('%a, %d %b %Y')],
["""\
.. date::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Invalid context: the "date" directive can only be used within a substitution definition.
        <literal_block xml:space="preserve">
            .. date::
"""],
]

# some locales return non-ASCII characters for names of days or months
# ensure the directive handles them correctly
if _locale_encoding in ('utf-8', 'utf8', 'latin-1', 'iso-8859-1'):
    totest['decode date'] = [
    ["""\
.. |date| date:: täglich
""",
    """\
<document source="test data">
    <substitution_definition names="date">
        täglich
"""],
    ]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
