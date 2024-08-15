#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for tables.py directives.
"""

import os
import csv
import platform
from pathlib import Path
import sys
import unittest

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[4]))

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document

# TEST_ROOT is ./test/ from the docutils root
TEST_ROOT = os.path.abspath(os.path.join(__file__, '..', '..', '..', '..'))


class ParserTestCase(unittest.TestCase):

    maxDiff = None

    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        settings.halt_level = 5
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


mydir = os.path.join(TEST_ROOT, 'test_parsers/test_rst/test_directives')
utf_16_csv = os.path.join(mydir, 'utf-16.csv')
empty_txt = os.path.join(mydir, 'empty.rst')

try:
    chr(0x9999999999999)
except OverflowError as unichr_exception:
    unichr_exception_string = f'code too large ({unichr_exception})'
except Exception as unichr_exception:
    unichr_exception_string = str(unichr_exception)
else:
    unichr_exception_string = ''

csv_eod_error_str = 'unexpected end of data'
# pypy adds a line number
if platform.python_implementation() == 'PyPy':
    csv_eod_error_str = 'line 1: ' + csv_eod_error_str
csv_unknown_url = "'bogus.csv'"


try:
    csv_data = Path(utf_16_csv).read_bytes()
    csv_data = str(csv_data, 'latin1').splitlines()
    reader = csv.reader([line + '\n' for line in csv_data])
    next(reader)
except Exception as detail:
    null_bytes_exception = detail
else:
    null_bytes_exception = None

# Null bytes are valid in Python 3.11+:
if null_bytes_exception is None:
    bad_encoding_result = """\
<document source="test data">
    <table>
        <title>
            bad encoding
        <tgroup cols="4">
            <colspec colwidth="25">
            <colspec colwidth="25">
            <colspec colwidth="25">
            <colspec colwidth="25">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            \xfe\xff"Treat"
                    <entry>
                        <paragraph>
                            "Quantity"
                    <entry>
                        <paragraph>
                            "Description"
                    <entry>
                <row>
                    <entry>
                        <paragraph>
                            "Albatr\u00b0\u00df"
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            "\u00a1Ona\x03\xc3\x03\xc4\x03\xb9\x03\xba!"
                    <entry>
                <row>
                    <entry>
                        <paragraph>
                            "CrunchyFrog"
                    <entry>
                        <paragraph>
                            1.49
                    <entry>
                        <paragraph>
                            "Ifwetooktheb\u00f6nesout
                    <entry>
                        <paragraph>
                            itwouldn\x20\x19tbe
                <row>
                    <entry>
                        <paragraph>
                            crunchy
                    <entry>
                        <paragraph>
                            nowwouldit?"
                    <entry>
                    <entry>
                <row>
                    <entry>
                        <paragraph>
                            "GannetRipple"
                    <entry>
                        <paragraph>
                            1.99
                    <entry>
                        <paragraph>
                            "\xbfOna\x03\xc3\x03\xc4\x03\xb9\x03\xba?"
                    <entry>
    <paragraph>
        (7- and 8-bit text encoded as UTF-16 has lots of null/zero bytes.)
"""
else:
    bad_encoding_result = """\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error with CSV data in "csv-table" directive:
            %s
        <literal_block xml:space="preserve">
            .. csv-table:: bad encoding
               :file: %s
               :encoding: latin-1
    <paragraph>
        (7- and 8-bit text encoded as UTF-16 has lots of null/zero bytes.)
""" % (null_bytes_exception, utf_16_csv)

try:
    int('y')
except ValueError as detail:
    invalid_literal = detail.args[0]
else:
    invalid_literal = ''

totest = {}

totest['table'] = [
["""\
.. table:: Truth table for "not"
   :class: custom
   :name:  tab:truth.not

   =====  =====
     A    not A
   =====  =====
   False  True
   True   False
   =====  =====
""",
"""\
<document source="test data">
    <table classes="custom" ids="tab-truth-not" names="tab:truth.not">
        <title>
            Truth table for "not"
        <tgroup cols="2">
            <colspec colwidth="5">
            <colspec colwidth="5">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            A
                    <entry>
                        <paragraph>
                            not A
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            False
                    <entry>
                        <paragraph>
                            True
                <row>
                    <entry>
                        <paragraph>
                            True
                    <entry>
                        <paragraph>
                            False
"""],
["""\
.. table::

   ========== ==========
   Table      without
   a          title
   ========== ==========
""",
"""\
<document source="test data">
    <table>
        <tgroup cols="2">
            <colspec colwidth="10">
            <colspec colwidth="10">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Table
                    <entry>
                        <paragraph>
                            without
                <row>
                    <entry>
                        <paragraph>
                            a
                    <entry>
                        <paragraph>
                            title
"""],
["""\
.. table:: title with an *error

   ======  =====
   Simple  table
   ======  =====
""",
"""\
<document source="test data">
    <table>
        <title>
            title with an \n\
            <problematic ids="problematic-1" refid="system-message-1">
                *
            error
        <tgroup cols="2">
            <colspec colwidth="6">
            <colspec colwidth="5">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Simple
                    <entry>
                        <paragraph>
                            table
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
["""\
.. table:: Not a table.

   This is a paragraph.
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error parsing content block for the "table" directive: exactly one table expected.
        <literal_block xml:space="preserve">
            .. table:: Not a table.
            \n\
               This is a paragraph.
"""],
["""\
.. table:: empty
""",
"""\
<document source="test data">
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Content block expected for the "table" directive; none found.
        <literal_block xml:space="preserve">
            .. table:: empty
"""],
["""\
.. table::
    :width: 100 %

    ============ ==============
    col 1        col 2
    ============ ==============
""",
"""\
<document source="test data">
    <table width="100%">
        <tgroup cols="2">
            <colspec colwidth="12">
            <colspec colwidth="14">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            col 1
                    <entry>
                        <paragraph>
                            col 2
"""],
["""\
.. table::
    :width: 100px

    ============ ==============
    col 1        col 2
    ============ ==============
""",
"""\
<document source="test data">
    <table width="100px">
        <tgroup cols="2">
            <colspec colwidth="12">
            <colspec colwidth="14">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            col 1
                    <entry>
                        <paragraph>
                            col 2
"""],
["""\
.. table::
    :width: 321

    ============ ==============
    col 1        col 2
    ============ ==============
""",
"""\
<document source="test data">
    <table width="321">
        <tgroup cols="2">
            <colspec colwidth="12">
            <colspec colwidth="14">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            col 1
                    <entry>
                        <paragraph>
                            col 2
"""],
["""\
.. table::
    :widths: 15, 25

    ============ ==============
    Columns with custom widths.
    ============ ==============
""",
"""\
<document source="test data">
    <table classes="colwidths-given">
        <tgroup cols="2">
            <colspec colwidth="15">
            <colspec colwidth="25">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Columns with
                    <entry>
                        <paragraph>
                            custom widths.
"""],
["""\
.. table::
    :widths: 10, 20

    +--------------+----------------+
    | Columns with | custom widths. |
    +--------------+----------------+
""",
"""\
<document source="test data">
    <table classes="colwidths-given">
        <tgroup cols="2">
            <colspec colwidth="10">
            <colspec colwidth="20">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Columns with
                    <entry>
                        <paragraph>
                            custom widths.
"""],
["""\
.. table::
    :widths: auto

    +--------------+-------------------+
    | Columns with | automatic widths. |
    +--------------+-------------------+
""",
"""\
<document source="test data">
    <table classes="colwidths-auto">
        <tgroup cols="2">
            <colspec colwidth="14">
            <colspec colwidth="19">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Columns with
                    <entry>
                        <paragraph>
                            automatic widths.
"""],
["""\
.. table::
    :widths: grid

    +--------------+-------------------+
    | Columns with | automatic widths. |
    +--------------+-------------------+
""",
"""\
<document source="test data">
    <table classes="colwidths-given">
        <tgroup cols="2">
            <colspec colwidth="14">
            <colspec colwidth="19">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Columns with
                    <entry>
                        <paragraph>
                            automatic widths.
"""],
["""\
.. table::
    :align: center

    ======  =====
    Simple  table
    ======  =====
""",
"""\
<document source="test data">
    <table align="center">
        <tgroup cols="2">
            <colspec colwidth="6">
            <colspec colwidth="5">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Simple
                    <entry>
                        <paragraph>
                            table
"""],
]

totest['csv_table'] = [
["""\
.. csv-table:: inline with integral header
   :width: 80%
   :widths: 10, 20, 30
   :header-rows: 1
   :stub-columns: 1

   "Treat", "Quantity", "Description"
   "Albatross", 2.99, "On a stick!"
   "Crunchy Frog", 1.49, "If we took the bones out, it wouldn\'t be
   crunchy, now would it?"
   "Gannet Ripple", 1.99, "On a stick!"
""",
"""\
<document source="test data">
    <table classes="colwidths-given" width="80%">
        <title>
            inline with integral header
        <tgroup cols="3">
            <colspec colwidth="10" stub="1">
            <colspec colwidth="20">
            <colspec colwidth="30">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <paragraph>
                            Description
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Albatross
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            On a stick!
                <row>
                    <entry>
                        <paragraph>
                            Crunchy Frog
                    <entry>
                        <paragraph>
                            1.49
                    <entry>
                        <paragraph>
                            If we took the bones out, it wouldn't be
                            crunchy, now would it?
                <row>
                    <entry>
                        <paragraph>
                            Gannet Ripple
                    <entry>
                        <paragraph>
                            1.99
                    <entry>
                        <paragraph>
                            On a stick!
"""],
["""\
.. csv-table:: inline with separate header
   :delim: space
   :header: "Treat" Quantity "Description"
   :widths: 10,20,30

   "Albatross" 2.99 "On a stick!"
""",
"""\
<document source="test data">
    <table classes="colwidths-given">
        <title>
            inline with separate header
        <tgroup cols="3">
            <colspec colwidth="10">
            <colspec colwidth="20">
            <colspec colwidth="30">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <paragraph>
                            Description
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Albatross
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            On a stick!
"""],
["""\
.. csv-table:: complex internal structure
   :header: "Treat", Quantity, "
            * Description,
            * Definition, or
            * Narrative"

   "
   * Ice cream
   * Sorbet
   * Albatross", 2.99, "On a stick!"
""",
"""\
<document source="test data">
    <table>
        <title>
            complex internal structure
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <bullet_list bullet="*">
                            <list_item>
                                <paragraph>
                                    Description,
                            <list_item>
                                <paragraph>
                                    Definition, or
                            <list_item>
                                <paragraph>
                                    Narrative
            <tbody>
                <row>
                    <entry>
                        <bullet_list bullet="*">
                            <list_item>
                                <paragraph>
                                    Ice cream
                            <list_item>
                                <paragraph>
                                    Sorbet
                            <list_item>
                                <paragraph>
                                    Albatross
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            On a stick!
"""],
["""\
.. csv-table:: short rows

   one, 2, three
   4, five
""",
"""\
<document source="test data">
    <table>
        <title>
            short rows
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            one
                    <entry>
                        <paragraph>
                            2
                    <entry>
                        <paragraph>
                            three
                <row>
                    <entry>
                        <paragraph>
                            4
                    <entry>
                        <paragraph>
                            five
                    <entry>
"""],
["""\
.. csv-table:: short rows
   :header-rows: 1

   header col 1, header col 2
   one, 2, three
   4
""",
"""\
<document source="test data">
    <table>
        <title>
            short rows
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            header col 1
                    <entry>
                        <paragraph>
                            header col 2
                    <entry>
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            one
                    <entry>
                        <paragraph>
                            2
                    <entry>
                        <paragraph>
                            three
                <row>
                    <entry>
                        <paragraph>
                            4
                    <entry>
                    <entry>
"""],
["""\
.. csv-table:: non-ASCII characters

   Heiz\xf6lr\xfccksto\xdfabd\xe4mpfung
""",
"""\
<document source="test data">
    <table>
        <title>
            non-ASCII characters
        <tgroup cols="1">
            <colspec colwidth="100">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Heiz\xf6lr\xfccksto\xdfabd\xe4mpfung
"""],
["""\
.. csv-table:: center aligned
   :align: center

   11, 12
   21, 22
""",
"""\
<document source="test data">
    <table align="center">
        <title>
            center aligned
        <tgroup cols="2">
            <colspec colwidth="50">
            <colspec colwidth="50">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            11
                    <entry>
                        <paragraph>
                            12
                <row>
                    <entry>
                        <paragraph>
                            21
                    <entry>
                        <paragraph>
                            22
"""],
["""\
.. csv-table:: empty
""",
"""\
<document source="test data">
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            The "csv-table" directive requires content; none supplied.
        <literal_block xml:space="preserve">
            .. csv-table:: empty
"""],
["""\
.. csv-table:: insufficient header row data
   :header-rows: 2

   some, csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            2 header row(s) specified but only 1 row(s) of data supplied ("csv-table" directive).
        <literal_block xml:space="preserve">
            .. csv-table:: insufficient header row data
               :header-rows: 2
            \n\
               some, csv, data
"""],
["""\
.. csv-table:: insufficient body data
   :header-rows: 1

   some, csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Insufficient data supplied (1 row(s)); no data remaining for table body, required by "csv-table" directive.
        <literal_block xml:space="preserve">
            .. csv-table:: insufficient body data
               :header-rows: 1
            \n\
               some, csv, data
"""],
["""\
.. csv-table:: content and external
   :file: bogus.csv

   some, csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            "csv-table" directive may not both specify an external file and have content.
        <literal_block xml:space="preserve">
            .. csv-table:: content and external
               :file: bogus.csv
            \n\
               some, csv, data
"""],
["""\
.. csv-table:: external file and url
   :file: bogus.csv
   :url: http://example.org/bogus.csv
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            The "file" and "url" options may not be simultaneously specified for the "csv-table" directive.
        <literal_block xml:space="preserve">
            .. csv-table:: external file and url
               :file: bogus.csv
               :url: http://example.org/bogus.csv
"""],
["""\
.. csv-table:: error in the *title

   some, csv, data
""",
"""\
<document source="test data">
    <table>
        <title>
            error in the \n\
            <problematic ids="problematic-1" refid="system-message-1">
                *
            title
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            some
                    <entry>
                        <paragraph>
                            csv
                    <entry>
                        <paragraph>
                            data
    <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
["""\
.. csv-table:: no such file
   :file: bogus.csv
""",
"""\
<document source="test data">
    <system_message level="4" line="1" source="test data" type="SEVERE">
        <paragraph>
            Problems with "csv-table" directive path:
            [Errno 2] No such file or directory: 'bogus.csv'.
        <literal_block xml:space="preserve">
            .. csv-table:: no such file
               :file: bogus.csv
"""],
# note that this output is rewritten below for certain python versions
["""\
.. csv-table:: bad URL
   :url: bogus.csv
""",
"""\
<document source="test data">
    <system_message level="4" line="1" source="test data" type="SEVERE">
        <paragraph>
            Problems with "csv-table" directive URL "bogus.csv":
            unknown url type: 'bogus.csv'.
        <literal_block xml:space="preserve">
            .. csv-table:: bad URL
               :url: bogus.csv
"""],
["""\
.. csv-table:: column mismatch
   :widths: 10,20

   some, csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            "csv-table" widths do not match the number of columns in table (3).
        <literal_block xml:space="preserve">
            .. csv-table:: column mismatch
               :widths: 10,20
            \n\
               some, csv, data
"""],
["""\
.. csv-table:: bad column widths
   :widths: 10,y,z

   some, csv, data

.. csv-table:: bad column widths
   :widths: 0 0 0

   some, csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "csv-table" directive:
            invalid option value: (option: "widths"; value: '10,y,z')
            %s.
        <literal_block xml:space="preserve">
            .. csv-table:: bad column widths
               :widths: 10,y,z
            \n\
               some, csv, data
    <system_message level="3" line="6" source="test data" type="ERROR">
        <paragraph>
            Error in "csv-table" directive:
            invalid option value: (option: "widths"; value: '0 0 0')
            negative or zero value; must be positive.
        <literal_block xml:space="preserve">
            .. csv-table:: bad column widths
               :widths: 0 0 0
            \n\
               some, csv, data
""" % invalid_literal],
["""\
.. csv-table:: good delimiter
   :delim: /

   some/csv/data

.. csv-table:: good delimiter
   :delim: \\

   some\\csv\\data

.. csv-table:: good delimiter
   :delim: 0x5c

   some\\csv\\data

.. csv-table:: good delimiter
   :delim: space

   some csv data
""",
"""\
<document source="test data">
    <table>
        <title>
            good delimiter
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            some
                    <entry>
                        <paragraph>
                            csv
                    <entry>
                        <paragraph>
                            data
    <table>
        <title>
            good delimiter
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            some
                    <entry>
                        <paragraph>
                            csv
                    <entry>
                        <paragraph>
                            data
    <table>
        <title>
            good delimiter
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            some
                    <entry>
                        <paragraph>
                            csv
                    <entry>
                        <paragraph>
                            data
    <table>
        <title>
            good delimiter
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            some
                    <entry>
                        <paragraph>
                            csv
                    <entry>
                        <paragraph>
                            data
"""],
["""\
.. csv-table:: bad delimiter
   :delim: multiple

.. csv-table:: bad delimiter
   :delim: U+9999999999999
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "csv-table" directive:
            invalid option value: (option: "delim"; value: 'multiple')
            'multiple' invalid; must be a single character or a Unicode code.
        <literal_block xml:space="preserve">
            .. csv-table:: bad delimiter
               :delim: multiple
    <system_message level="3" line="4" source="test data" type="ERROR">
        <paragraph>
            Error in "csv-table" directive:
            invalid option value: (option: "delim"; value: 'U+9999999999999')
            %s.
        <literal_block xml:space="preserve">
            .. csv-table:: bad delimiter
               :delim: U+9999999999999
""" % unichr_exception_string],
["""\
.. csv-table:: bad CSV data

   "bad", "csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error with CSV data in "csv-table" directive:
            %s
        <literal_block xml:space="preserve">
            .. csv-table:: bad CSV data
            \n\
               "bad", \"csv, data
""" % csv_eod_error_str],
["""\
.. csv-table:: bad CSV header data
   :header: "bad", "csv, data

   good, csv, data
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error with CSV data in "csv-table" directive:
            %s
        <literal_block xml:space="preserve">
            .. csv-table:: bad CSV header data
               :header: "bad", \"csv, data
            \n\
               good, csv, data
""" % csv_eod_error_str],
["""\
.. csv-table:: bad encoding
   :file: %s
   :encoding: latin-1

(7- and 8-bit text encoded as UTF-16 has lots of null/zero bytes.)
""" % utf_16_csv,
bad_encoding_result
],
["""\
.. csv-table:: good encoding
   :file: %s
   :encoding: utf-16
   :header-rows: 1
""" % utf_16_csv,
"""\
<document source="test data">
    <table>
        <title>
            good encoding
        <tgroup cols="3">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <paragraph>
                            Description
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Albatr\u00b0\u00df
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            \u00a1On a \u03c3\u03c4\u03b9\u03ba!
                <row>
                    <entry>
                        <paragraph>
                            Crunchy Frog
                    <entry>
                        <paragraph>
                            1.49
                    <entry>
                        <paragraph>
                            If we took the b\u00f6nes out, it wouldn\u2019t be
                            crunchy, now would it?
                <row>
                    <entry>
                        <paragraph>
                            Gannet Ripple
                    <entry>
                        <paragraph>
                            1.99
                    <entry>
                        <paragraph>
                            \u00bfOn a \u03c3\u03c4\u03b9\u03ba?
"""],
["""\
.. csv-table:: no CSV data
   :file: %s
""" % empty_txt,
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            No table data detected in CSV file.
        <literal_block xml:space="preserve">
            .. csv-table:: no CSV data
               :file: %s
""" % empty_txt],
]

totest['list_table'] = [
["""\
.. list-table:: list table with integral header
   :widths: 10 20 30
   :header-rows: 1
   :stub-columns: 1

   * - Treat
     - Quantity
     - Description
   * - Albatross
     - 2.99
     - On a stick!
   * - Crunchy Frog
     - 1.49
     - If we took the bones out, it wouldn\'t be
       crunchy, now would it?
   * - Gannet Ripple
     - 1.99
     - On a stick!
""",
"""\
<document source="test data">
    <table classes="colwidths-given">
        <title>
            list table with integral header
        <tgroup cols="3">
            <colspec colwidth="10" stub="1">
            <colspec colwidth="20">
            <colspec colwidth="30">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <paragraph>
                            Description
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Albatross
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            On a stick!
                <row>
                    <entry>
                        <paragraph>
                            Crunchy Frog
                    <entry>
                        <paragraph>
                            1.49
                    <entry>
                        <paragraph>
                            If we took the bones out, it wouldn\'t be
                            crunchy, now would it?
                <row>
                    <entry>
                        <paragraph>
                            Gannet Ripple
                    <entry>
                        <paragraph>
                            1.99
                    <entry>
                        <paragraph>
                            On a stick!
"""],
["""\
.. list-table:: list table with integral header
   :widths: auto
   :header-rows: 1
   :stub-columns: 1

   * - Treat
     - Quantity
     - Description
   * - Albatross
     - 2.99
     - On a stick!
""",
"""\
<document source="test data">
    <table classes="colwidths-auto">
        <title>
            list table with integral header
        <tgroup cols="3">
            <colspec colwidth="33" stub="1">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <paragraph>
                            Description
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Albatross
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            On a stick!
"""],
["""\
.. list-table:: list table with integral header
   :header-rows: 1
   :stub-columns: 1

   * - Treat
     - Quantity
     - Description
   * - Albatross
     - 2.99
     - On a stick!
   * - Crunchy Frog
     - 1.49
     - If we took the bones out, it wouldn\'t be
       crunchy, now would it?
   * - Gannet Ripple
     - 1.99
     - On a stick!
""",
"""\
<document source="test data">
    <table>
        <title>
            list table with integral header
        <tgroup cols="3">
            <colspec colwidth="33" stub="1">
            <colspec colwidth="33">
            <colspec colwidth="33">
            <thead>
                <row>
                    <entry>
                        <paragraph>
                            Treat
                    <entry>
                        <paragraph>
                            Quantity
                    <entry>
                        <paragraph>
                            Description
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Albatross
                    <entry>
                        <paragraph>
                            2.99
                    <entry>
                        <paragraph>
                            On a stick!
                <row>
                    <entry>
                        <paragraph>
                            Crunchy Frog
                    <entry>
                        <paragraph>
                            1.49
                    <entry>
                        <paragraph>
                            If we took the bones out, it wouldn\'t be
                            crunchy, now would it?
                <row>
                    <entry>
                        <paragraph>
                            Gannet Ripple
                    <entry>
                        <paragraph>
                            1.99
                    <entry>
                        <paragraph>
                            On a stick!
"""],
["""\
.. list-table:: center aligned
   :align: center

   * - 11
     - 12
   * - 21
     - 22
""",
"""\
<document source="test data">
    <table align="center">
        <title>
            center aligned
        <tgroup cols="2">
            <colspec colwidth="50">
            <colspec colwidth="50">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            11
                    <entry>
                        <paragraph>
                            12
                <row>
                    <entry>
                        <paragraph>
                            21
                    <entry>
                        <paragraph>
                            22
"""],
["""\
.. list-table::

   not a bullet list
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error parsing content block for the "list-table" directive: exactly one bullet list expected.
        <literal_block xml:space="preserve">
            .. list-table::
            \n\
               not a bullet list
"""],
["""\
.. list-table::

   * not a second-level bullet list
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error parsing content block for the "list-table" directive: two-level bullet list expected, but row 1 does not contain a second-level bullet list.
        <literal_block xml:space="preserve">
            .. list-table::
            \n\
               * not a second-level bullet list
"""],
["""\
.. list-table::

   * - columns not uniform
   * - first row has one,
     - second row has two
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error parsing content block for the "list-table" directive: uniform two-level bullet list expected, but row 2 does not contain the same number of items as row 1 (2 vs 1).
        <literal_block xml:space="preserve">
            .. list-table::
            \n\
               * - columns not uniform
               * - first row has one,
                 - second row has two
"""],
["""\
.. list-table::
   :widths: 10 20

   * - ":widths:" option doesn't match columns
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            "list-table" widths do not match the number of columns in table (1).
        <literal_block xml:space="preserve">
            .. list-table::
               :widths: 10 20
            \n\
               * - ":widths:" option doesn\'t match columns
"""],
["""\
.. list-table::
   :stub-columns: 3

   * - column 1
     - column 2
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            3 stub column(s) specified but only 2 columns(s) of data supplied ("list-table" directive).
        <literal_block xml:space="preserve">
            .. list-table::
               :stub-columns: 3
            \n\
               * - column 1
                 - column 2
"""],
["""\
.. list-table::
   :stub-columns: 2

   * - column 1
     - column 2
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Insufficient data supplied (2 columns(s)); no data remaining for table body, required by "list-table" directive.
        <literal_block xml:space="preserve">
            .. list-table::
               :stub-columns: 2
            \n\
               * - column 1
                 - column 2
"""],
["""\
.. list-table:: empty
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            The "list-table" directive is empty; content required.
        <literal_block xml:space="preserve">
            .. list-table:: empty
"""],
]


if __name__ == '__main__':
    unittest.main()
