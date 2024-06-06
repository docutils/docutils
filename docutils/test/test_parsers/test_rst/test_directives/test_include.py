#! /usr/bin/env python3
# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "include" directive.
"""

from pathlib import Path
import os.path
import unittest
import sys

if __name__ == '__main__':
    # prepend the "docutils root" to the Python library path
    # so we import the local `docutils` package.
    sys.path.insert(0, str(Path(__file__).resolve().parents[4]))

from docutils import parsers, utils
from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document
from docutils.utils.code_analyzer import with_pygments
from test.test_parsers.test_rst.test_directives.test_code \
    import PYGMENTS_2_14_PLUS


TEST_ROOT = Path(__file__).resolve().parents[3]


# optional 3rd-party markdown parser
md_parser_name = 'recommonmark'
try:  # check availability
    md_parser_class = parsers.get_parser_class(md_parser_name)
except ImportError:
    md_parser_class = None


class ParserTestCase(unittest.TestCase):
    maxDiff = None

    def test_parser(self):
        # eventually skip optional parts:
        if not with_pygments:
            del totest['include_parsed_code']
        if not md_parser_class:
            del totest['include_markdown']

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


try:
    chr(0x11111111)
except ValueError as detail:
    unichr_exception = f'{detail.__class__.__name__}: {detail}'
else:
    unichr_exception = ''


# prepend this directory (relative to the test root):
def mydir(path):
    return os.path.relpath(
        os.path.join(TEST_ROOT, 'test_parsers/test_rst/test_directives', path),
        os.getcwd()).replace('\\', '/')


include1 = mydir('include1.txt')
include2 = mydir('include2.txt')
include3 = mydir('include3.txt')
include5 = mydir('includes/include5.txt')
include6 = mydir('includes/more/include6.txt')
include8 = mydir('include8.txt')
include10 = mydir('include10.txt')
include11 = mydir('include 11.txt')
include12 = mydir('include12.txt')
include13 = mydir('include13.txt')
include14 = mydir('includes/include14.txt')
include15 = mydir('includes/include15.txt')
include16 = mydir('includes/include16.txt')
include_literal = mydir('include_literal.txt')
include_md = mydir('include.md')
include_xml = TEST_ROOT/'data/duplicate-id.xml'
include = TEST_ROOT/'data/include.txt'
latin2 = TEST_ROOT/'data/latin2.txt'
utf_16_file = TEST_ROOT/'data/utf-16-le-sig.txt'
utf_16_error_str = ("UnicodeDecodeError: 'ascii' codec can't decode byte 0xff "
                    "in position 0: ordinal not in range(128)")
rst_states_dir = os.path.dirname(parsers.rst.states.__file__)
# TODO fix case normalisation bug on Windows in utils.relative_path()
# nonexistent = os.path.relpath(
#     os.path.join(rst_states_dir, 'include', 'nonexistent'),
#     os.getcwd()).replace('\\', '/')
nonexistent = utils.relative_path(
    os.path.join(os.getcwd(), 'dummy'),
    os.path.join(rst_states_dir, 'include', 'nonexistent')
)

# Different error for path with 8bit chars with locale == C or None:
try:
    open('\u043c\u0438\u0440.txt')
except UnicodeEncodeError:
    errstr_8bit_path = """\
Cannot encode input file path "\u043c\u0438\u0440.txt" (wrong locale?).\
"""
except FileNotFoundError:
    errstr_8bit_path = """\
InputError: [Errno 2] No such file or directory: '\u043c\u0438\u0440.txt'.\
"""

totest = {}

totest['include'] = [
[f"""\
Include Test
============

.. include:: {include1}

A paragraph.
""",
"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <section ids="inclusion-1" names="inclusion\\ 1">
            <title>
                Inclusion 1
            <paragraph>
                This file is used by \n\
                <literal>
                    test_include.py
                .
            <paragraph>
                A paragraph.
"""],
[f"""\
Include Test
============

.. include:: {include1}
   :literal:
   :class: test
   :name: my name

A paragraph.
""",
f"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <literal_block classes="test" ids="my-name" names="my\\ name" source="{include1}" xml:space="preserve">
            Inclusion 1
            -----------
            \n\
            This file is used by ``test_include.py``.
        <paragraph>
            A paragraph.
"""],
[f"""\
Literal include, add line numbers

.. include:: {include1}
   :literal:
   :number-lines:
""",
f"""\
<document source="test data">
    <paragraph>
        Literal include, add line numbers
    <literal_block source="{include1}" xml:space="preserve">
        <inline classes="ln">
            1 \n\
        Inclusion 1
        <inline classes="ln">
            2 \n\
        -----------
        <inline classes="ln">
            3 \n\
        \n\
        <inline classes="ln">
            4 \n\
        This file is used by ``test_include.py``.
"""],
[f"""\
Include code

.. include:: {include1}
   :code:
   :class: test
   :name: my name
""",
f"""\
<document source="test data">
    <paragraph>
        Include code
    <literal_block classes="code test" ids="my-name" names="my\\ name" source="{include1}" xml:space="preserve">
        Inclusion 1
        -----------
        \n\
        This file is used by ``test_include.py``.
"""],
[f"""\
Include code, add line numbers

.. include:: {include1}
   :code:
   :number-lines:
""",
f"""\
<document source="test data">
    <paragraph>
        Include code, add line numbers
    <literal_block classes="code" source="{include1}" xml:space="preserve">
        <inline classes="ln">
            1 \n\
        Inclusion 1
        <inline classes="ln">
            2 \n\
        -----------
        <inline classes="ln">
            3 \n\
        \n\
        <inline classes="ln">
            4 \n\
        This file is used by ``test_include.py``.
"""],
[f"""\
Include with unknown parser.

.. include:: {include1}
   :parser: sillyformat

A paragraph.
""",
f"""\
<document source="test data">
    <paragraph>
        Include with unknown parser.
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Error in "include" directive:
            invalid option value: (option: "parser"; value: \'sillyformat\')
            Parser "sillyformat" not found. No module named 'sillyformat'.
        <literal_block xml:space="preserve">
            .. include:: {include1}
               :parser: sillyformat
    <paragraph>
        A paragraph.
"""],
[f"""\
Let's test the parse context.

    This paragraph is in a block quote.

    .. include:: {include2}

The included paragraphs should also be in the block quote.
""",
"""\
<document source="test data">
    <paragraph>
        Let's test the parse context.
    <block_quote>
        <paragraph>
            This paragraph is in a block quote.
        <paragraph>
            Here are some paragraphs
            that can appear at any level.
        <paragraph>
            This file (include2.txt) is used by
            <literal>
                test_include.py
            .
    <paragraph>
        The included paragraphs should also be in the block quote.
"""],
["""\
Include Test
============

.. include:: nonexistent.txt

A paragraph.
""",
"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <system_message level="4" line="4" source="test data" type="SEVERE">
            <paragraph>
                Problems with "include" directive path:
                InputError: [Errno 2] No such file or directory: 'nonexistent.txt'.
            <literal_block xml:space="preserve">
                .. include:: nonexistent.txt
        <paragraph>
            A paragraph.
"""],
[f"""\
Include Test
============

.. include:: {include1}

.. include:: {include1}

A paragraph.
""",
f"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <section dupnames="inclusion\\ 1" ids="inclusion-1">
            <title>
                Inclusion 1
            <paragraph>
                This file is used by \n\
                <literal>
                    test_include.py
                .
        <section dupnames="inclusion\\ 1" ids="inclusion-1-1">
            <title>
                Inclusion 1
            <system_message backrefs="inclusion-1-1" level="1" line="2" source="{include1}" type="INFO">
                <paragraph>
                    Duplicate implicit target name: "inclusion 1".
            <paragraph>
                This file is used by \n\
                <literal>
                    test_include.py
                .
            <paragraph>
                A paragraph.
"""],
[f"""\
Include Test
============

.. include:: {include1}

----------

.. include:: {include1}

A paragraph.
""",
f"""\
<document source="test data">
    <section ids="include-test" names="include\\ test">
        <title>
            Include Test
        <section dupnames="inclusion\\ 1" ids="inclusion-1">
            <title>
                Inclusion 1
            <paragraph>
                This file is used by \n\
                <literal>
                    test_include.py
                .
            <transition>
        <section dupnames="inclusion\\ 1" ids="inclusion-1-1">
            <title>
                Inclusion 1
            <system_message backrefs="inclusion-1-1" level="1" line="2" source="{include1}" type="INFO">
                <paragraph>
                    Duplicate implicit target name: "inclusion 1".
            <paragraph>
                This file is used by \n\
                <literal>
                    test_include.py
                .
            <paragraph>
                A paragraph.
"""],
[f"""\
Recursive inclusions: adapt paths.

In test data

.. include:: {include3}
""",
f"""\
<document source="test data">
    <paragraph>
        Recursive inclusions: adapt paths.
    <paragraph>
        In test data
    <paragraph>
        In include3.txt
    <paragraph>
        In includes/include4.txt
    <paragraph>
        In includes/include5.txt
    <paragraph>
        In includes/more/include6.txt
    <paragraph>
        In includes/sibling/include7.txt
    <literal_block source="{include5}" xml:space="preserve">
        In includes/include5.txt
        \n\
        .. include:: more/include6.txt
    <table>
        <tgroup cols="2">
            <colspec colwidth="50">
            <colspec colwidth="50">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            In
                    <entry>
                        <paragraph>
                            includes/sibling/include7.txt
"""],
[f"""\
Recursive inclusion with specified parser.

In test data

.. include:: {include3}
   :parser: rst
""",
f"""\
<document source="test data">
    <paragraph>
        Recursive inclusion with specified parser.
    <paragraph>
        In test data
    <paragraph>
        In include3.txt
    <paragraph>
        In includes/include4.txt
    <paragraph>
        In includes/include5.txt
    <paragraph>
        In includes/more/include6.txt
    <paragraph>
        In includes/sibling/include7.txt
    <literal_block source="{include5}" xml:space="preserve">
        In includes/include5.txt
        \n\
        .. include:: more/include6.txt
    <table>
        <tgroup cols="2">
            <colspec colwidth="50">
            <colspec colwidth="50">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            In
                    <entry>
                        <paragraph>
                            includes/sibling/include7.txt
"""],
[f"""\
In test data

Section
=======

(Section contents in nested parse; slice of input_lines ViewList.)

.. include:: {include3}
""",
f"""\
<document source="test data">
    <paragraph>
        In test data
    <section ids="section" names="section">
        <title>
            Section
        <paragraph>
            (Section contents in nested parse; slice of input_lines ViewList.)
        <paragraph>
            In include3.txt
        <paragraph>
            In includes/include4.txt
        <paragraph>
            In includes/include5.txt
        <paragraph>
            In includes/more/include6.txt
        <paragraph>
            In includes/sibling/include7.txt
        <literal_block source="{include5}" xml:space="preserve">
            In includes/include5.txt
            \n\
            .. include:: more/include6.txt
        <table>
            <tgroup cols="2">
                <colspec colwidth="50">
                <colspec colwidth="50">
                <tbody>
                    <row>
                        <entry>
                            <paragraph>
                                In
                        <entry>
                            <paragraph>
                                includes/sibling/include7.txt
"""],
[f"""\
Testing relative includes:

.. include:: {include8}
""",
"""\
<document source="test data">
    <paragraph>
        Testing relative includes:
    <paragraph>
        In include8.txt
    <paragraph>
        In ../includes/include9.txt.
    <paragraph>
        Here are some paragraphs
        that can appear at any level.
    <paragraph>
        This file (include2.txt) is used by
        <literal>
            test_include.py
        .
"""],
[f"""\
Encoding:

.. include:: {utf_16_file}
   :encoding: utf-16
""",
"""\
<document source="test data">
    <paragraph>
        Encoding:
    <paragraph>
        Grüße
"""],
[f"""\
Default encoding: auto-determine (here via BOM).

.. include:: {utf_16_file}
""",
"""\
<document source="test data">
    <paragraph>
        Default encoding: auto-determine (here via BOM).
    <paragraph>
        Grüße
"""],
[f"""\
Default encoding: auto-determine (via encoding declaration).

.. include:: {latin2}
""",
"""\
<document source="test data">
    <paragraph>
        Default encoding: auto-determine (via encoding declaration).
    <comment xml:space="preserve">
        -*- encoding: latin2 -*-
    <paragraph>
        škoda
"""],
[f"""\
Include file is UTF-16-encoded, and is not valid ASCII.

.. include:: {utf_16_file}
   :encoding: ascii
""",
f"""\
<document source="test data">
    <paragraph>
        Include file is UTF-16-encoded, and is not valid ASCII.
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problem with "include" directive:
            {utf_16_error_str}
        <literal_block xml:space="preserve">
            .. include:: {utf_16_file}
               :encoding: ascii
"""],
["""\
cyrillic filename:

.. include:: \u043c\u0438\u0440.txt
""",
f"""\
<document source="test data">
    <paragraph>
        cyrillic filename:
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problems with "include" directive path:
            {errstr_8bit_path}
        <literal_block xml:space="preserve">
            .. include:: \u043c\u0438\u0440.txt
"""],
[f"""\
Testing errors in included file:

.. include:: {include10}
""",
f"""\
<document source="test data">
    <paragraph>
        Testing errors in included file:
    <system_message level="3" line="1" source="{include10}" type="ERROR">
        <paragraph>
            Invalid character code: 0x11111111
            {unichr_exception}
        <literal_block xml:space="preserve">
            unicode:: 0x11111111
    <system_message level="2" line="1" source="{include10}" type="WARNING">
        <paragraph>
            Substitution definition "bad" empty or invalid.
        <literal_block xml:space="preserve">
            .. |bad| unicode:: 0x11111111
    <section dupnames="hi" ids="hi">
        <title>
            hi
        <block_quote>
            <paragraph>
                indent
        <system_message level="2" line="7" source="{include10}" type="WARNING">
            <paragraph>
                Block quote ends without a blank line; unexpected unindent.
        <paragraph>
            error
    <section dupnames="hi" ids="hi-1">
        <title>
            hi
        <system_message backrefs="hi-1" level="1" line="10" source="{include10}" type="INFO">
            <paragraph>
                Duplicate implicit target name: "hi".
        <system_message level="4" line="12" source="{include10}" type="SEVERE">
            <paragraph>
                Problems with "include" directive path:
                InputError: [Errno 2] No such file or directory: '{nonexistent}'.
            <literal_block xml:space="preserve">
                .. include:: <nonexistent>
        <system_message level="3" line="14" source="{include10}" type="ERROR">
            <paragraph>
                Content block expected for the "note" directive; none found.
            <literal_block xml:space="preserve">
                .. note::
        <system_message level="3" line="16" source="{include10}" type="ERROR">
            <paragraph>
                Content block expected for the "admonition" directive; none found.
            <literal_block xml:space="preserve">
                .. admonition::
                   without title
        <system_message level="3" line="19" source="{include10}" type="ERROR">
            <paragraph>
                Content block expected for the "epigraph" directive; none found.
            <literal_block xml:space="preserve">
                .. epigraph::
        <system_message level="3" line="21" source="{include10}" type="ERROR">
            <paragraph>
                Content block expected for the "highlights" directive; none found.
            <literal_block xml:space="preserve">
                .. highlights::
        <system_message level="3" line="23" source="{include10}" type="ERROR">
            <paragraph>
                Content block expected for the "pull-quote" directive; none found.
            <literal_block xml:space="preserve">
                .. pull-quote::
        <system_message level="3" line="25" source="{include10}" type="ERROR">
            <paragraph>
                Invalid context: the "date" directive can only be used within a substitution definition.
            <literal_block xml:space="preserve">
                .. date::
        <paragraph>
            not a
            definition list:
        <system_message level="3" line="29" source="{include10}" type="ERROR">
            <paragraph>
                Unexpected indentation.
        <block_quote>
            <paragraph>
                as a term may only be one line long.
        <system_message level="3" line="31" source="{include10}" type="ERROR">
            <paragraph>
                Error in "admonition" directive:
                1 argument(s) required, 0 supplied.
            <literal_block xml:space="preserve">
                .. admonition::
                \n\
                   without title and content following a blank line
    <section ids="section-underline-too-short" names="section\\ underline\\ too\\ short">
        <title>
            section underline too short
        <system_message level="2" line="36" source="{include10}" type="WARNING">
            <paragraph>
                Title underline too short.
            <literal_block xml:space="preserve">
                section underline too short
                -----
        <table>
            <tgroup cols="2">
                <colspec colwidth="14">
                <colspec colwidth="6">
                <thead>
                    <row>
                        <entry>
                            <paragraph>
                                A simple table
                        <entry>
                            <paragraph>
                                cell 2
                <tbody>
                    <row>
                        <entry>
                            <paragraph>
                                cell 3
                        <entry>
                            <paragraph>
                                cell 4
        <system_message level="2" line="43" source="{include10}" type="WARNING">
            <paragraph>
                Blank line required after table.
        <paragraph>
            No blank line after table.
        <system_message level="3" line="45" source="{include10}" type="ERROR">
            <paragraph>
                Error in "unicode" directive:
                1 argument(s) required, 0 supplied.
            <literal_block xml:space="preserve">
                unicode::
        <system_message level="2" line="45" source="{include10}" type="WARNING">
            <paragraph>
                Substitution definition "empty" empty or invalid.
            <literal_block xml:space="preserve">
                .. |empty| unicode::
        <system_message level="3" line="47" source="{include10}" type="ERROR">
            <paragraph>
                Error in "topic" directive:
                1 argument(s) required, 0 supplied.
            <literal_block xml:space="preserve">
                .. topic::
        <system_message level="3" line="49" source="{include10}" type="ERROR">
            <paragraph>
                Error in "rubric" directive:
                1 argument(s) required, 0 supplied.
            <literal_block xml:space="preserve">
                .. rubric::
        <rubric>
            A rubric has no content
        <comment xml:space="preserve">
            _`target: No matching backquote.
        <system_message level="2" line="52" source="{include10}" type="WARNING">
            <paragraph>
                malformed hyperlink target.
        <comment xml:space="preserve">
            __malformed: no good
        <system_message level="2" line="53" source="{include10}" type="WARNING">
            <paragraph>
                malformed hyperlink target.
        <definition_list>
            <definition_list_item>
                <term>
                    A literal block::
                <definition>
                    <system_message level="1" line="56" source="{include10}" type="INFO">
                        <paragraph>
                            Blank line missing before literal block (after the "::")? Interpreted as a definition list item.
                    <paragraph>
                        with no blank line above.
        <literal_block xml:space="preserve">
            > A literal block.
        <system_message level="3" line="61" source="{include10}" type="ERROR">
            <paragraph>
                Inconsistent literal block quoting.
        <paragraph>
            $ with inconsistent quoting.
        <paragraph>
            <problematic ids="problematic-1" refid="system-message-1">
                :unknown-role:`role`
            \n\
            and \n\
            <problematic ids="problematic-2" refid="system-message-2">
                *
            unbalanced
            <problematic ids="problematic-3" refid="system-message-3">
                `
            inline
            <problematic ids="problematic-4" refid="system-message-4">
                **
            markup
        <system_message level="1" line="63" source="{include10}" type="INFO">
            <paragraph>
                No role entry for "unknown-role" in module "docutils.parsers.rst.languages.en".
                Trying "unknown-role" as canonical role name.
        <system_message backrefs="problematic-1" ids="system-message-1" level="3" line="63" source="{include10}" type="ERROR">
            <paragraph>
                Unknown interpreted text role "unknown-role".
        <system_message backrefs="problematic-2" ids="system-message-2" level="2" line="63" source="{include10}" type="WARNING">
            <paragraph>
                Inline emphasis start-string without end-string.
        <system_message backrefs="problematic-3" ids="system-message-3" level="2" line="63" source="{include10}" type="WARNING">
            <paragraph>
                Inline interpreted text or phrase reference start-string without end-string.
        <system_message backrefs="problematic-4" ids="system-message-4" level="2" line="63" source="{include10}" type="WARNING">
            <paragraph>
                Inline strong start-string without end-string.
        <block_quote>
            <paragraph>
                A block quote with \n\
                <problematic ids="problematic-5" refid="system-message-5">
                    *
                inline warning
            <system_message backrefs="problematic-5" ids="system-message-5" level="2" line="68" source="{include10}" type="WARNING">
                <paragraph>
                    Inline emphasis start-string without end-string.
            <attribution>
                attribution with \n\
                <problematic ids="problematic-6" refid="system-message-6">
                    *
                inline warning
        <system_message backrefs="problematic-6" ids="system-message-6" level="2" line="70" source="{include10}" type="WARNING">
            <paragraph>
                Inline emphasis start-string without end-string.
        <paragraph>
            <problematic ids="problematic-7" refid="system-message-7">
                :PEP:`-1`
        <system_message backrefs="problematic-7" ids="system-message-7" level="3" line="72" source="{include10}" type="ERROR">
            <paragraph>
                PEP number must be a number from 0 to 9999; "-1" is invalid.
        <system_message level="1" line="70" source="{include10}" type="INFO">
            <paragraph>
                No directive entry for "unknown" in module "docutils.parsers.rst.languages.en".
                Trying "unknown" as canonical directive name.
        <system_message level="3" line="74" source="{include10}" type="ERROR">
            <paragraph>
                Unknown directive type "unknown".
            <literal_block xml:space="preserve">
                .. unknown:: directive (TODO: info still reported with wrong line)
        <system_message level="3" line="76" source="{include10}" type="ERROR">
            <paragraph>
                Malformed table.
                No bottom table border found.
            <literal_block xml:space="preserve">
                ==============  ======
                A simple table  with
                no bottom       border
                \n\
                .. end of inclusion from "{include10}"
"""],
[f"""\
Include file with whitespace in the path:

.. include:: {include11}
""",
"""\
<document source="test data">
    <paragraph>
        Include file with whitespace in the path:
    <paragraph>
        some text
"""],
["""\
Standard include data file:

.. include:: <isogrk4.txt>
""",
"""\
<document source="test data">
    <paragraph>
        Standard include data file:
    <comment xml:space="preserve">
        This data file has been placed in the public domain.
    <comment xml:space="preserve">
        Derived from the Unicode character mappings available from
        <http://www.w3.org/2003/entities/xml/>.
        Processed by unicode2rstsubs.py, part of Docutils:
        <https://docutils.sourceforge.io>.
    <substitution_definition names="b.Gammad">
        \u03dc
    <substitution_definition names="b.gammad">
        \u03dd
"""],
["""\
Nonexistent standard include data file:

.. include:: <nonexistent>
""",
f"""\
<document source="test data">
    <paragraph>
        Nonexistent standard include data file:
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problems with "include" directive path:
            InputError: [Errno 2] No such file or directory: '{nonexistent}'.
        <literal_block xml:space="preserve">
            .. include:: <nonexistent>
"""],
[f"""\
Include start-line/end-line Test

.. include:: {include2}
   :start-line: 3
   :end-line: 4
""",
"""\
<document source="test data">
    <paragraph>
        Include start-line/end-line Test
    <paragraph>
        This file (include2.txt) is used by
"""],
[f"""\
Include start-line/end-line + start-after Test

.. include:: {include12}
   :start-line: 2
   :end-line: 5
   :start-after: here

Text search is limited to the specified lines.
""",
"""\
<document source="test data">
    <paragraph>
        Include start-line/end-line + start-after Test
    <paragraph>
        In include12.txt (after "start here", before "stop here")
    <paragraph>
        Text search is limited to the specified lines.
"""],
[f"""\
Include start-after/end-before Test

.. include:: {include12}
   :start-after: .. start here
   :end-before: .. stop here

A paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Include start-after/end-before Test
    <paragraph>
        In include12.txt (after "start here", before "stop here")
    <paragraph>
        A paragraph.
"""],
[f"""\
Include start-after/end-before Test, single option variant

.. include:: {include12}
   :end-before: .. start here

.. include:: {include12}
   :start-after: .. stop here

A paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Include start-after/end-before Test, single option variant
    <paragraph>
        In include12.txt (but before "start here")
    <paragraph>
        In include12.txt (after "stop here")
    <paragraph>
        A paragraph.
"""],
[f"""\
Include start-after/end-before multi-line test.

.. include:: {include13}
   :start-after: From: me
                 To: you
   :end-before: -------
                -- mork of ork

.. include:: {include13}
   :start-after: From: me
                 To: you
   :end-before:
       -------
         -- mork of ork

A paragraph.
""",
f"""\
<document source="test data">
    <paragraph>
        Include start-after/end-before multi-line test.
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problem with "end-before" option of "include" directive:
            Text not found.
        <literal_block xml:space="preserve">
            .. include:: {include13}
               :start-after: From: me
                             To: you
               :end-before: -------
                            -- mork of ork
    <paragraph>
        In include13.txt (between header and signature)
    <paragraph>
        A paragraph.
"""],
[f"""\
Error handling test; "end-before" error handling tested in previous test.

.. include:: {include13}
   :start-after: bad string
   :end-before: mork of ork
""",
f"""\
<document source="test data">
    <paragraph>
        Error handling test; "end-before" error handling tested in previous test.
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problem with "start-after" option of "include" directive:
            Text not found.
        <literal_block xml:space="preserve">
            .. include:: {include13}
               :start-after: bad string
               :end-before: mork of ork
"""],
[f"""\
Default TAB expansion with literal include:

.. include:: {include_literal}
   :literal:
""",
f"""\
<document source="test data">
    <paragraph>
        Default TAB expansion with literal include:
    <literal_block source="{include_literal}" xml:space="preserve">
        Literal included this should **not** be *marked* `up`.
                <- leading raw tab.
        \n\
        Newlines
        are
        normalized.
"""],
[f"""\
Custom TAB expansion with literal include:

.. include:: {include_literal}
   :literal:
   :tab-width: 2
""",
f"""\
<document source="test data">
    <paragraph>
        Custom TAB expansion with literal include:
    <literal_block source="{include_literal}" xml:space="preserve">
        Literal included this should **not** be *marked* `up`.
          <- leading raw tab.
        \n\
        Newlines
        are
        normalized.
"""],
[f"""\
No TAB expansion with literal include:

.. include:: {include_literal}
   :literal:
   :tab-width: -1
""",
f"""\
<document source="test data">
    <paragraph>
        No TAB expansion with literal include:
    <literal_block source="{include_literal}" xml:space="preserve">
        Literal included this should **not** be *marked* `up`.
        \t<- leading raw tab.
        \n\
        Newlines
        are
        normalized.
"""],
]

totest['include_parsed_code'] = [
[f"""\
Included code

.. include:: {include1}
   :code: rst
""",
f"""\
<document source="test data">
    <paragraph>
        Included code
    <literal_block classes="code rst" source="{include1}" xml:space="preserve">
        <inline classes="generic heading">
            Inclusion 1
        \n\
        <inline classes="generic heading">
            -----------
        \n\
        <inline classes="whitespace">
            \n\
        This file is used by \n\
        <inline classes="literal string">
            ``test_include.py``
        .
""" if PYGMENTS_2_14_PLUS else f"""\
<document source="test data">
    <paragraph>
        Included code
    <literal_block classes="code rst" source="{include1}" xml:space="preserve">
        <inline classes="generic heading">
            Inclusion 1
        \n\
        <inline classes="generic heading">
            -----------
        \n\
        \n\
        This file is used by \n\
        <inline classes="literal string">
            ``test_include.py``
        .
"""],
[f"""\
Included code

.. include:: {include1}
   :code: rst
   :number-lines:
""",
f"""\
<document source="test data">
    <paragraph>
        Included code
    <literal_block classes="code rst" source="{include1}" xml:space="preserve">
        <inline classes="ln">
            1 \n\
        <inline classes="generic heading">
            Inclusion 1
        \n\
        <inline classes="ln">
            2 \n\
        <inline classes="generic heading">
            -----------
        \n\
        <inline classes="ln">
            3 \n\
        <inline classes="whitespace">
            \n\
        <inline classes="ln">
            4 \n\
        <inline classes="whitespace">
        This file is used by \n\
        <inline classes="literal string">
            ``test_include.py``
        .
""" if PYGMENTS_2_14_PLUS else f"""\
<document source="test data">
    <paragraph>
        Included code
    <literal_block classes="code rst" source="{include1}" xml:space="preserve">
        <inline classes="ln">
            1 \n\
        <inline classes="generic heading">
            Inclusion 1
        \n\
        <inline classes="ln">
            2 \n\
        <inline classes="generic heading">
            -----------
        \n\
        <inline classes="ln">
            3 \n\
        \n\
        <inline classes="ln">
            4 \n\
        This file is used by \n\
        <inline classes="literal string">
            ``test_include.py``
        .
"""],
[f"""\
Default TAB expansion with included code:

.. include:: {include_literal}
   :code: rst
""",
f"""\
<document source="test data">
    <paragraph>
        Default TAB expansion with included code:
    <literal_block classes="code rst" source="{include_literal}" xml:space="preserve">
        Literal included this should \n\
        <inline classes="generic strong">
            **not**
         be \n\
        <inline classes="generic emph">
            *marked*
         \n\
        <inline classes="name variable">
            `up`
        .
        <inline classes="whitespace">
            \n\
                <- leading raw tab.
        <inline classes="whitespace">
            \n\
            \n\
        Newlines
        <inline classes="whitespace">
            \n\
        are
        <inline classes="whitespace">
            \n\
        normalized.
""" if PYGMENTS_2_14_PLUS else f"""\
<document source="test data">
    <paragraph>
        Default TAB expansion with included code:
    <literal_block classes="code rst" source="{include_literal}" xml:space="preserve">
        Literal included this should \n\
        <inline classes="generic strong">
            **not**
         be \n\
        <inline classes="generic emph">
            *marked*
         \n\
        <inline classes="name variable">
            `up`
        .
                <- leading raw tab.
        \n\
        Newlines
        are
        normalized.
"""],
[f"""\
Custom TAB expansion with included code:

.. include:: {include_literal}
   :code: rst
   :tab-width: 2
""",
f"""\
<document source="test data">
    <paragraph>
        Custom TAB expansion with included code:
    <literal_block classes="code rst" source="{include_literal}" xml:space="preserve">
        Literal included this should \n\
        <inline classes="generic strong">
            **not**
         be \n\
        <inline classes="generic emph">
            *marked*
         \n\
        <inline classes="name variable">
            `up`
        .
        <inline classes="whitespace">
            \n\
          <- leading raw tab.
        <inline classes="whitespace">
            \n\
            \n\
        Newlines
        <inline classes="whitespace">
            \n\
        are
        <inline classes="whitespace">
            \n\
        normalized.
""" if PYGMENTS_2_14_PLUS else f"""\
<document source="test data">
    <paragraph>
        Custom TAB expansion with included code:
    <literal_block classes="code rst" source="{include_literal}" xml:space="preserve">
        Literal included this should \n\
        <inline classes="generic strong">
            **not**
         be \n\
        <inline classes="generic emph">
            *marked*
         \n\
        <inline classes="name variable">
            `up`
        .
          <- leading raw tab.
        \n\
        Newlines
        are
        normalized.
"""],
[f"""\
No TAB expansion with included code:

.. include:: {include_literal}
   :code: rst
   :tab-width: -1
""",
f"""\
<document source="test data">
    <paragraph>
        No TAB expansion with included code:
    <literal_block classes="code rst" source="{include_literal}" xml:space="preserve">
        Literal included this should \n\
        <inline classes="generic strong">
            **not**
         be \n\
        <inline classes="generic emph">
            *marked*
         \n\
        <inline classes="name variable">
            `up`
        .
        <inline classes="whitespace">
            \n\
        \t<- leading raw tab.
        <inline classes="whitespace">
            \n\
            \n\
        Newlines
        <inline classes="whitespace">
            \n\
        are
        <inline classes="whitespace">
            \n\
        normalized.
""" if PYGMENTS_2_14_PLUS else f"""\
<document source="test data">
    <paragraph>
        No TAB expansion with included code:
    <literal_block classes="code rst" source="{include_literal}" xml:space="preserve">
        Literal included this should \n\
        <inline classes="generic strong">
            **not**
         be \n\
        <inline classes="generic emph">
            *marked*
         \n\
        <inline classes="name variable">
            `up`
        .
        \t<- leading raw tab.
        \n\
        Newlines
        are
        normalized.
"""],
[f"""\
Including includes/include14.txt

.. include:: {include14}
""",
f"""\
<document source="test data">
    <paragraph>
        Including includes/include14.txt
    <paragraph>
        Including more/include6.txt as rst-code from includes/include14.txt:
    <literal_block classes="code rst" source="{include6}" xml:space="preserve">
        In includes/more/include6.txt
        <inline classes="whitespace">
            \n\
            \n\
        <inline classes="punctuation">
            ..
         \n\
        <inline classes="operator word">
            include
        <inline classes="punctuation">
            ::
         ../sibling/include7.txt
""" if PYGMENTS_2_14_PLUS else f"""\
<document source="test data">
    <paragraph>
        Including includes/include14.txt
    <paragraph>
        Including more/include6.txt as rst-code from includes/include14.txt:
    <literal_block classes="code rst" source="{include6}" xml:space="preserve">
        In includes/more/include6.txt
        \n\
        <inline classes="punctuation">
            ..
         \n\
        <inline classes="operator word">
            include
        <inline classes="punctuation">
            ::
         ../sibling/include7.txt
"""],
[f"""\
Circular inclusion

.. include:: {include15}
""",
f"""\
<document source="test data">
    <paragraph>
        Circular inclusion
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
    <paragraph>
        File "include16.txt": example of rekursive inclusion.
    <system_message level="2" line="3" source="{include16}" type="WARNING">
        <paragraph>
            circular inclusion in "include" directive:
            {include15}
            > {include16}
            > {include15}
            > test data
        <literal_block xml:space="preserve">
            .. include:: include15.txt
    <paragraph>
        No loop when clipping before the "include" directive:
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
"""],
[f"""\
Circular inclusion with clipping.

.. include:: {include16}
   :start-line: 2
""",
f"""\
<document source="test data">
    <paragraph>
        Circular inclusion with clipping.
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
    <paragraph>
        File "include16.txt": example of rekursive inclusion.
    <system_message level="2" line="3" source="{include16}" type="WARNING">
        <paragraph>
            circular inclusion in "include" directive:
            {include15}
            > {include16}
            > {include15}
            > {include16}
            > test data
        <literal_block xml:space="preserve">
            .. include:: include15.txt
    <paragraph>
        No loop when clipping before the "include" directive:
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
    <paragraph>
        No loop when clipping before the "include" directive:
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
"""],
[f"""\
Circular inclusion with specified parser.

.. include:: {include15}
   :parser: rst
""",
f"""\
<document source="test data">
    <paragraph>
        Circular inclusion with specified parser.
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
    <paragraph>
        File "include16.txt": example of rekursive inclusion.
    <system_message level="2" line="3" source="{include16}" type="WARNING">
        <paragraph>
            circular inclusion in "include" directive:
            {include15}
            > {include16}
            > {include15}
            > test data
        <literal_block xml:space="preserve">
            .. include:: include15.txt
    <paragraph>
        No loop when clipping before the "include" directive:
    <paragraph>
        File "include15.txt": example of rekursive inclusion.
"""],
[f"""\
Include Docutils XML file:

.. include:: {include_xml}
   :parser: xml

The duplicate id is reported and would be appended
by the "universal.Messages" transform.
""",
"""\
<document source="test data">
    <paragraph>
        Include Docutils XML file:
    <section>
        <title ids="s4">
            nice heading
        <paragraph>
            Text with \n\
            <strong ids="s4">
                strong
                statement
             and more text.
    <paragraph>
        The duplicate id is reported and would be appended
        by the "universal.Messages" transform.
"""],
[f"""\
No circular inclusion.

.. list-table::

   * - .. include:: {include}
     - .. include:: {include}
""",
"""\
<document source="test data">
    <paragraph>
        No circular inclusion.
    <table>
        <tgroup cols="2">
            <colspec colwidth="50">
            <colspec colwidth="50">
            <tbody>
                <row>
                    <entry>
                        <paragraph>
                            Some include text.
                    <entry>
                        <paragraph>
                            Some include text.
"""],
]

# Parsing with Markdown is an optional feature depending on 3rd-party modules:
totest['include_markdown'] = [
[f"""\
Include Markdown source.

.. include:: {include_md}
   :parser: {md_parser_name}

A paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Include Markdown source.
    <section ids="title-1" names="title\\ 1">
        <title>
            Title 1
        <paragraph>
            <emphasis>
                emphasis
             and \n\
            <emphasis>
                also emphasis
        <paragraph>
            No whitespace required around a
            <reference name="phrase reference" refuri="/uri">
                phrase reference
            .
    <paragraph>
        A paragraph.
"""],
]


if __name__ == '__main__':
    unittest.main()
