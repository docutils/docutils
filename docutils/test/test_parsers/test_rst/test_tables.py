#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for states.py.
"""

import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['tables'] = [
["""\
+-------------------------------------+
| A table with one cell and one line. |
+-------------------------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="1">
            <colspec colwidth="37">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with one cell and one line.
"""],
["""\
+-----------------------+
| A table with one cell |
| and two lines.        |
+-----------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="1">
            <colspec colwidth="23">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with one cell
                            and two lines.
"""],
["""\
+-----------------------+
| A malformed table. |
+-----------------------+
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Malformed table at line 1; formatting as a literal block.
    <literal_block>
        +-----------------------+
        | A malformed table. |
        +-----------------------+
"""],
["""\
+------------------------+
| A well-formed | table. |
+------------------------+

+------------------------+
| This +----------+ too! |
+------------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="1">
            <colspec colwidth="24">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A well-formed | table.
    <table>
        <tgroup cols="1">
            <colspec colwidth="24">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            This +----------+ too!
"""],
["""\
+--------------+--------------+
| A table with | two columns. |
+--------------+--------------+
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="14">
            <colspec colwidth="14">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with
                    <entry>
                        <paragraph>
                            two columns.
"""],
["""\
+--------------+
| A table with |
+--------------+
| two rows.    |
+--------------+
""",
"""\
<document>
    <table>
        <tgroup cols="1">
            <colspec colwidth="14">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with
                <row>
                    <entry>
                        <paragraph>
                            two rows.
"""],
["""\
+--------------+-------------+
| A table with | two columns |
+--------------+-------------+
| and          | two rows.   |
+--------------+-------------+
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="14">
            <colspec colwidth="13">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with
                    <entry>
                        <paragraph>
                            two columns
                <row>
                    <entry>
                        <paragraph>
                            and
                    <entry>
                        <paragraph>
                            two rows.
"""],
["""\
+--------------+---------------+
| A table with | two columns,  |
+--------------+---------------+
| two rows, and a column span. |
+------------------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="14">
            <colspec colwidth="15">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with
                    <entry>
                        <paragraph>
                            two columns,
                <row>
                    <entry morecols="1">
                        <paragraph>
                            two rows, and a column span.
"""],
["""\
+--------------------------+
| A table with three rows, |
+------------+-------------+
| and two    | columns.    |
+------------+-------------+
| First and last rows      |
| contains column spans.   |
+--------------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="12">
            <colspec colwidth="13">
            <tbody>
                <row>
                    <entry morecols="1">
                        <paragraph>
                            A table with three rows,
                <row>
                    <entry>
                        <paragraph>
                            and two
                    <entry>
                        <paragraph>
                            columns.
                <row>
                    <entry morecols="1">
                        <paragraph>
                            First and last rows
                            contains column spans.
"""],
["""\
+--------------+--------------+
| A table with | two columns, |
+--------------+ and a row    |
| two rows,    | span.        |
+--------------+--------------+
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="14">
            <colspec colwidth="14">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A table with
                    <entry morerows="1">
                        <paragraph>
                            two columns,
                            and a row
                            span.
                <row>
                    <entry>
                        <paragraph>
                            two rows,
"""],
["""\
+------------+-------------+---------------+
| A table    | two rows in | and row spans |
| with three +-------------+ to left and   |
| columns,   | the middle, | right.        |
+------------+-------------+---------------+
""",
"""\
<document>
    <table>
        <tgroup cols="3">
            <colspec colwidth="12">
            <colspec colwidth="13">
            <colspec colwidth="15">
            <tbody>
                <row>
                    <entry morerows="1">
                        <paragraph>
                            A table
                            with three
                            columns,
                    <entry>
                        <paragraph>
                            two rows in
                    <entry morerows="1">
                        <paragraph>
                            and row spans
                            to left and
                            right.
                <row>
                    <entry>
                        <paragraph>
                            the middle,
"""],
["""\
Complex spanning pattern (no edge knows all rows/cols):

+-----------+-------------------------+
| W/NW cell | N/NE cell               |
|           +-------------+-----------+
|           | Middle cell | E/SE cell |
+-----------+-------------+           |
| S/SE cell               |           |
+-------------------------+-----------+
""",
"""\
<document>
    <paragraph>
        Complex spanning pattern (no edge knows all rows/cols):
    <table>
        <tgroup cols="3">
            <colspec colwidth="11">
            <colspec colwidth="13">
            <colspec colwidth="11">
            <tbody>
                <row>
                    <entry morerows="1">
                        <paragraph>
                            W/NW cell
                    <entry morecols="1">
                        <paragraph>
                            N/NE cell
                <row>
                    <entry>
                        <paragraph>
                            Middle cell
                    <entry morerows="1">
                        <paragraph>
                            E/SE cell
                <row>
                    <entry morecols="1">
                        <paragraph>
                            S/SE cell
"""],
["""\
+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+------------------------+------------+----------+----------+
| body row 2             | Cells may span columns.          |
+------------------------+------------+---------------------+
| body row 3             | Cells may  | - Table cells       |
+------------------------+ span rows. | - contain           |
| body row 4             |            | - body elements.    |
+------------------------+------------+---------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="4">
            <colspec colwidth="24">
            <colspec colwidth="12">
            <colspec colwidth="10">
            <colspec colwidth="10">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Header row, column 1
                    <entry>
                        <paragraph>
                            Header 2
                    <entry>
                        <paragraph>
                            Header 3
                    <entry>
                        <paragraph>
                            Header 4
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            body row 1, column 1
                    <entry>
                        <paragraph>
                            column 2
                    <entry>
                        <paragraph>
                            column 3
                    <entry>
                        <paragraph>
                            column 4
                <row>
                    <entry>
                        <paragraph>
                            body row 2
                    <entry morecols="2">
                        <paragraph>
                            Cells may span columns.
                <row>
                    <entry>
                        <paragraph>
                            body row 3
                    <entry morerows="1">
                        <paragraph>
                            Cells may
                            span rows.
                    <entry morecols="1" morerows="1">
                        <bullet_list bullet="-">
                            <list_item>
                                <paragraph>
                                    Table cells
                            <list_item>
                                <paragraph>
                                    contain
                            <list_item>
                                <paragraph>
                                    body elements.
                <row>
                    <entry>
                        <paragraph>
                            body row 4
"""],
["""\
+-----------------+--------+
| A simple table  | cell 2 |
+-----------------+--------+
| cell 3          | cell 4 |
+-----------------+--------+
No blank line after table.
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="17">
            <colspec colwidth="8">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A simple table
                    <entry>
                        <paragraph>
                            cell 2
                <row>
                    <entry>
                        <paragraph>
                            cell 3
                    <entry>
                        <paragraph>
                            cell 4
    <system_message level="2" type="WARNING">
        <paragraph>
            Blank line required after table at line 6.
    <paragraph>
        No blank line after table.
"""],
["""\
+-----------------+--------+
| A simple table  | cell 2 |
+-----------------+--------+
| cell 3          | cell 4 |
+-----------------+--------+
    Unexpected indent and no blank line after table.
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="17">
            <colspec colwidth="8">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A simple table
                    <entry>
                        <paragraph>
                            cell 2
                <row>
                    <entry>
                        <paragraph>
                            cell 3
                    <entry>
                        <paragraph>
                            cell 4
    <system_message level="3" type="ERROR">
        <paragraph>
            Unexpected indentation at line 6.
    <system_message level="2" type="WARNING">
        <paragraph>
            Blank line required after table at line 6.
    <block_quote>
        <paragraph>
            Unexpected indent and no blank line after table.
"""],
["""\
+--------------+-------------+
| A bad table. |             |
+--------------+             |
| Cells must be rectangles.  |
+----------------------------+
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Malformed table at line 1; formatting as a literal block.
            Malformed table; parse incomplete.
    <literal_block>
        +--------------+-------------+
        | A bad table. |             |
        +--------------+             |
        | Cells must be rectangles.  |
        +----------------------------+
"""],
["""\
+------------------------------+
| This table contains another. |
|                              |
| +-------------------------+  |
| | A table within a table. |  |
| +-------------------------+  |
+------------------------------+
""",
"""\
<document>
    <table>
        <tgroup cols="1">
            <colspec colwidth="30">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            This table contains another.
                        <table>
                            <tgroup cols="1">
                                <colspec colwidth="25">
                                <tbody>
                                    <row>
                                        <entry>
                                            <paragraph>
                                                A table within a table.
"""],
["""\
+------------------+--------+
| A simple table   |        |
+------------------+--------+
| with empty cells |        |
+------------------+--------+
""",
"""\
<document>
    <table>
        <tgroup cols="2">
            <colspec colwidth="18">
            <colspec colwidth="8">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            A simple table
                    <entry>
                <row>
                    <entry>
                        <paragraph>
                            with empty cells
                    <entry>
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
