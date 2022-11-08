#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

import unittest

from test import DocutilsTestSupport  # NoQA: F401

from docutils.parsers.rst import tableparser
from docutils.statemachine import StringList, string2lines


class SimpleTableParserTestCase(unittest.TestCase):
    def test_parse(self):
        parser = tableparser.SimpleTableParser()
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                lines_input = StringList(string2lines(case_input), 'test data')
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    try:
                        output = parser.parse(lines_input)
                    except Exception as details:
                        output = f'{details.__class__.__name__}: {details}'
                    self.assertEqual(output, case_expected)


totest = {}

totest['simple_tables'] = [
["""\
============  ============
A table with  two columns.
============  ============
""",
([12, 12],
 [],
 [[[0, 0, 1, ['A table with']],
   [0, 0, 1, ['two columns.']]]])],
["""\
============  ===============
A tāble w̅ith  comb̲ining chars
============  ===============
""",
([12, 15],
 [],
 [[[0, 0, 1, ['A ta\u0304ble w\u0305ith']],
   [0, 0, 1, ['comb\u0332ining chars']]]])],
["""\
============  ============
A table with  two columns
and           two rows.
============  ============
""",
([12, 12],
 [],
 [[[0, 0, 1, ['A table with']],
   [0, 0, 1, ['two columns']]],
  [[0, 0, 2, ['and']],
   [0, 0, 2, ['two rows.']]]])],
["""\
======================================
The last row might stick into the margin
second row.
======================================
""",
([40],
 [],
 [[[0, 0, 1, ['The last row might stick into the margin']]],
  [[0, 0, 2, ['second row.']]]])],
["""\
==========  ===========
A table with four rows,
-----------------------
and two     columns.
First and   last rows
contain column spans.
=======================
""",
([10, 11],
 [],
 [[[0, 1, 1, ['A table with four rows,']]],
  [[0, 0, 3, ['and two']],
   [0, 0, 3, ['columns.']]],
  [[0, 0, 4, ['First and']],
   [0, 0, 4, ['last rows']]],
  [[0, 1, 5, ['contain column spans.']]]])],
["""\
=======  =====  ======
A bad table     cell 2
cell 3          cell 4
============  ======
""",
'TableMarkupError: Text in column margin in table line 2.'],
["""\
======  =====  ======
row     one
Another bad    table
======  =====  ======
""",
'TableMarkupError: Text in column margin in table line 3.'],
["""\
===========  ================
A table with two header rows,
-----------------------------
the first    with a span.
===========  ================
Two body     rows,
the second with a span.
=============================
""",
([11, 16],
 [[[0, 1, 1, ['A table with two header rows,']]],
  [[0, 0, 3, ['the first']],
   [0, 0, 3, ['with a span.']]]],
 [[[0, 0, 5, ['Two body']],
   [0, 0, 5, ['rows,']]],
  [[0, 1, 6, ['the second with a span.']]]])],
["""\
============  =============
A table with  two head/body
============  =============
row           separators.
============  =============
That's bad.
============  =============
""",
'TableMarkupError: Multiple head/body row separators '
'(table lines 3 and 5); only one allowed.'],
["""\
============  ============
============  ============
""",
([12, 12],
 [],
 [[[0, 0, 1, []],
   [0, 0, 1, []]]])],
# ["""\
# ==============  ==========
# Table with row  separators
# ==============  ==========
#                 and blank
# --------------  ----------
#                 entries
# --------------  ----------
#                 in first
# --------------  ----------
#                 columns.
# ==============  ==========
# """,
# '']
]


if __name__ == '__main__':
    import unittest
    unittest.main()
