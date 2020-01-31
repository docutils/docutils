#! /usr/bin/env python

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for admonition directives with local language module.
"""
from __future__ import absolute_import

if __name__ == '__main__':
    import __init__
from test_parsers import DocutilsTestSupport

def suite():
    settings = {'language_code': 'local-dummy-lang',
                'report_level': 2} # warning (has no effect on test output is run as __main__).
    s = DocutilsTestSupport.ParserTestSuite(suite_settings=settings)
    s.generateTests(totest)
    return s

totest = {}

totest['admonitions'] = [
["""\
.. Dummy-Attention:: directive with silly localised name.

.. Attention:: English fallback (an INFO is written).
""",
"""\
<document source="test data">
    <attention>
        <paragraph>
            directive with silly localised name.
    <attention>
        <paragraph>
            English fallback (an INFO is written).
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
