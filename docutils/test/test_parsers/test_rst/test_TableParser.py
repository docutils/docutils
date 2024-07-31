#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[3]))

from docutils.parsers.rst import tableparser
from docutils.statemachine import StringList, string2lines


class GridTableParserTestCase(unittest.TestCase):
    def test_parse_table(self):
        parser = tableparser.GridTableParser()
        for name, cases in totest.items():
            for casenum, case in enumerate(cases):
                case_input, case_table, _case_expected = case
                lines_input = StringList(string2lines(case_input), 'test data')
                parser.setup(lines_input)
                try:
                    parser.find_head_body_sep()
                    parser.parse_table()
                    output = parser.cells
                except Exception as details:
                    output = f'{details.__class__.__name__}: {details}'
                self.assertEqual(output, case_table)

    def test_parse(self):
        parser = tableparser.GridTableParser()
        for name, cases in totest.items():
            for casenum, case in enumerate(cases):
                case_input, _case_table, case_expected = case
                lines_input = StringList(string2lines(case_input), 'test data')
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    try:
                        output = parser.parse(lines_input)
                    except Exception as details:
                        output = f'{details.__class__.__name__}: {details}'
                    self.assertEqual(case_expected, output)


totest = {}

totest['grid_tables'] = [
["""\
+-------------------------------------+
| A table with one cell and one line. |
+-------------------------------------+
""",
[(0, 0, 2, 38, ['A table with one cell and one line.'])],
([37],
 [],
 [[(0, 0, 1, ['A table with one cell and one line.'])]])],
["""\
+--------------+--------------+
| A table with | two columns. |
+--------------+--------------+
""",
[(0, 0, 2, 15, ['A table with']),
 (0, 15, 2, 30, ['two columns.'])],
([14, 14],
 [],
 [[(0, 0, 1, ['A table with']),
   (0, 0, 1, ['two columns.'])]])],
# Combining chars in grid tables still fail
# ["""\
# +--------------+------------------+
# | A tāble w̅ith | comb̲ining chars. |
# +--------------+------------------+
# """,
# [(0, 0, 2, 15, ['A table with']),
#  (0, 15, 2, 30, ['combining chars.'])],
# ([14, 14],
#  [],
#  [[(0, 0, 1, ['A table with']),
#    (0, 0, 1, ['combining chars.'])]])],
["""\
+--------------+-------------+
| A table with | two columns |
+--------------+-------------+
| and          | two rows.   |
+--------------+-------------+
""",
[(0, 0, 2, 15, ['A table with']),
 (0, 15, 2, 29, ['two columns']),
 (2, 0, 4, 15, ['and']),
 (2, 15, 4, 29, ['two rows.'])],
([14, 13],
 [],
 [[(0, 0, 1, ['A table with']),
   (0, 0, 1, ['two columns'])],
  [(0, 0, 3, ['and']),
   (0, 0, 3, ['two rows.'])]])],
["""\
+--------------------------+
| A table with three rows, |
+------------+-------------+
| and two    | columns.    |
+------------+-------------+
| First and last rows      |
| contain column spans.    |
+--------------------------+
""",
[(0, 0, 2, 27, ['A table with three rows,']),
 (2, 0, 4, 13, ['and two']),
 (2, 13, 4, 27, ['columns.']),
 (4, 0, 7, 27, ['First and last rows', 'contain column spans.'])],
([12, 13],
 [],
 [[(0, 1, 1, ['A table with three rows,']),
   None],
  [(0, 0, 3, ['and two']),
   (0, 0, 3, ['columns.'])],
  [(0, 1, 5, ['First and last rows', 'contain column spans.']),
   None]])],
["""\
+------------+-------------+---------------+
| A table    | two rows in | and row spans |
| with three +-------------+ to left and   |
| columns,   | the middle, | right.        |
+------------+-------------+---------------+
""",
[(0, 0, 4, 13, ['A table', 'with three', 'columns,']),
 (0, 13, 2, 27, ['two rows in']),
 (0, 27, 4, 43, ['and row spans', 'to left and', 'right.']),
 (2, 13, 4, 27, ['the middle,'])],
([12, 13, 15],
 [],
 [[(1, 0, 1, ['A table', 'with three', 'columns,']),
   (0, 0, 1, ['two rows in']),
   (1, 0, 1, ['and row spans', 'to left and', 'right.'])],
  [None,
   (0, 0, 3, ['the middle,']),
   None]])],
["""\
+------------+-------------+---------------+
| A table |  | two rows in | and funny     |
| with 3  +--+-------------+-+ stuff.      |
| columns,   | the middle, | |             |
+------------+-------------+---------------+
""",
[(0, 0, 4, 13, ['A table |', 'with 3  +--', 'columns,']),
 (0, 13, 2, 27, ['two rows in']),
 (0, 27, 4, 43, [' and funny', '-+ stuff.', ' |']),
 (2, 13, 4, 27, ['the middle,'])],
([12, 13, 15],
 [],
 [[(1, 0, 1, ['A table |', 'with 3  +--', 'columns,']),
   (0, 0, 1, ['two rows in']),
   (1, 0, 1, [' and funny', '-+ stuff.', ' |'])],
  [None,
   (0, 0, 3, ['the middle,']),
   None]])],
["""\
+-----------+-------------------------+
| W/NW cell | N/NE cell               |
|           +-------------+-----------+
|           | Middle cell | E/SE cell |
+-----------+-------------+           |
| S/SE cell               |           |
+-------------------------+-----------+
""",
[(0, 0, 4, 12, ['W/NW cell', '', '']),
 (0, 12, 2, 38, ['N/NE cell']),
 (2, 12, 4, 26, ['Middle cell']),
 (2, 26, 6, 38, ['E/SE cell', '', '']),
 (4, 0, 6, 26, ['S/SE cell'])],
([11, 13, 11],
 [],
 [[(1, 0, 1, ['W/NW cell', '', '']),
   (0, 1, 1, ['N/NE cell']),
   None],
  [None,
   (0, 0, 3, ['Middle cell']),
   (1, 0, 3, ['E/SE cell', '', ''])],
  [(0, 1, 5, ['S/SE cell']),
   None,
   None]])],
["""\
+--------------+-------------+
| A bad table. |             |
+--------------+             |
| Cells must be rectangles.  |
+----------------------------+
""",
'TableMarkupError: Malformed table; parse incomplete.',
'TableMarkupError: Malformed table; parse incomplete.'],
["""\
+-------------------------------+
| A table with two header rows, |
+------------+------------------+
| the first  | with a span.     |
+============+==================+
| Two body   | rows,            |
+------------+------------------+
| the second with a span.       |
+-------------------------------+
""",
[(0, 0, 2, 32, ['A table with two header rows,']),
 (2, 0, 4, 13, ['the first']),
 (2, 13, 4, 32, ['with a span.']),
 (4, 0, 6, 13, ['Two body']),
 (4, 13, 6, 32, ['rows,']),
 (6, 0, 8, 32, ['the second with a span.'])],
([12, 18],
 [[(0, 1, 1, ['A table with two header rows,']),
   None],
  [(0, 0, 3, ['the first']),
   (0, 0, 3, ['with a span.'])]],
 [[(0, 0, 5, ['Two body']),
   (0, 0, 5, ['rows,'])],
  [(0, 1, 7, ['the second with a span.']),
   None]])],
["""\
+-------------------------------+
| A table with two head/body    |
+=============+=================+
| row         | separators.     |
+=============+=================+
| That's bad. |                 |
+-------------+-----------------+
""",
'TableMarkupError: Multiple head/body row separators '
'(table lines 3 and 5); only one allowed.',
'TableMarkupError: Multiple head/body row separators '
'(table lines 3 and 5); only one allowed.'],
["""\
+-------------------------------------+
|                                     |
+-------------------------------------+
""",
[(0, 0, 2, 38, [''])],
([37],
 [],
 [[(0, 0, 1, [''])]])],
]


if __name__ == '__main__':
    unittest.main()
