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
from docutils.readers.python.moduleparser import trim_docstring


class MiscTests(unittest.TestCase):

    docstrings = ("""""", # empty
                  """Begins on the first line.

        Middle line indented.

    Last line unindented.
    """,
                  """
    Begins on the second line.

        Middle line indented.

    Last line unindented.""",
                  """All on one line.""")
    expected = ("""""", # empty
                """\
Begins on the first line.

    Middle line indented.

Last line unindented.""",
                  """\
Begins on the second line.

    Middle line indented.

Last line unindented.""",
                """All on one line.""")

    def test_trim_docstring(self):
        for i in range(len(self.docstrings)):
            self.assertEquals(trim_docstring(self.docstrings[i]),
                              self.expected[i])
            self.assertEquals(trim_docstring('\n    ' + self.docstrings[i]),
                              self.expected[i])


if __name__ == '__main__':
    unittest.main()
