#! /usr/bin/env python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the misc.py "date" directive.
"""

from __init__ import DocutilsTestSupport
import time, os

from docutils.utils.error_reporting import locale_encoding

os.environ['SOURCE_DATE_EPOCH'] = '5000000'

def suite():
    s = DocutilsTestSupport.ParserTestSuite(suite_settings={'use_source_date_epoch':True})
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
        1970-02-27
    <paragraph>
        Today's date is \n\
        <substitution_reference refname="date">
            date
        .
"""],
["""\
.. |date| date:: %a, %d %b %Y %H:%M
""",
"""\
<document source="test data">
    <substitution_definition names="date">
        Fri, 27 Feb 1970 20:53
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
