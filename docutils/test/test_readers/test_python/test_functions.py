#! /usr/bin/env python

# Author: David Goodger
# Contact: goodger@users.sourceforge.net
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

"""
Tests for PySource Reader functions.
"""

import unittest
from __init__ import DocutilsTestSupport
from docutils.readers.python.moduleparser import trim_docstring, TokenReader


class MiscTests(unittest.TestCase):

    docstrings = (
        ("""""", """"""), # empty
        ("""Begins on the first line.

        Middle line indented.

    Last line unindented.
    """,
         """\
Begins on the first line.

    Middle line indented.

Last line unindented."""),
        ("""
    Begins on the second line.

        Middle line indented.

    Last line unindented.""",
         """\
Begins on the second line.

    Middle line indented.

Last line unindented."""),
        ("""All on one line.""", """All on one line."""))

    def test_trim_docstring(self):
        for docstring, expected in self.docstrings:
            self.assertEquals(trim_docstring(docstring), expected)
            self.assertEquals(trim_docstring('\n    ' + docstring),
                              expected)

#     def test_token_reader(self):
#         tr = TokenReader('a = 1')
#         self.assertEquals(tr.rhs('a', 1), '1')


if __name__ == '__main__':
    unittest.main()
