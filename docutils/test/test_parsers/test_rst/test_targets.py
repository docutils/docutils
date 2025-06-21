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

from docutils.frontend import get_default_settings
from docutils.parsers.rst import Parser
from docutils.utils import new_document


class ParserTestCase(unittest.TestCase):

    maxDiff = None

    def test_parser(self):
        parser = Parser()
        settings = get_default_settings(Parser)
        settings.warning_stream = ''
        for name, cases in totest.items():
            for casenum, (case_input, case_expected) in enumerate(cases):
                with self.subTest(id=f'totest[{name!r}][{casenum}]'):
                    document = new_document('test data', settings.copy())
                    parser.parse(case_input, document)
                    output = document.pformat()
                    self.assertEqual(case_expected, output)


totest = {}

totest['targets'] = [
["""\
.. _target:

(Internal hyperlink target.)
""",
"""\
<document source="test data">
    <target ids="target" names="target">
    <paragraph>
        (Internal hyperlink target.)
"""],
["""\
.. _optional space before colon :
""",
"""\
<document source="test data">
    <target ids="optional-space-before-colon" names="optional\\ space\\ before\\ colon">
"""],
[r"""
External hyperlink targets:

.. _one-liner: http://structuredtext.sourceforge.net

.. _starts-on-this-line: http://
                         structuredtext.
                         sourceforge.net

.. _entirely-below:
   http://structuredtext.
   sourceforge.net

.. _escaped-whitespace: http://example.org/a\ path\ with\
   spaces.html

.. _not-indirect: uri\_
""",
"""\
<document source="test data">
    <paragraph>
        External hyperlink targets:
    <target ids="one-liner" names="one-liner" refuri="http://structuredtext.sourceforge.net">
    <target ids="starts-on-this-line" names="starts-on-this-line" refuri="http://structuredtext.sourceforge.net">
    <target ids="entirely-below" names="entirely-below" refuri="http://structuredtext.sourceforge.net">
    <target ids="escaped-whitespace" names="escaped-whitespace" refuri="http://example.org/a path with spaces.html">
    <target ids="not-indirect" names="not-indirect" refuri="uri_">
"""],
["""\
Indirect hyperlink targets:

.. _target1: reference_

.. _target2: `phrase-link reference`_
""",
"""\
<document source="test data">
    <paragraph>
        Indirect hyperlink targets:
    <target ids="target1" names="target1" refname="reference">
    <target ids="target2" names="target2" refname="phrase-link reference">
"""],
["""\
.. _a long target name:

.. _`a target name: including a colon (quoted)`:

.. _a target name\\: including a colon (escaped):
""",
"""\
<document source="test data">
    <target ids="a-long-target-name" names="a\\ long\\ target\\ name">
    <target ids="a-target-name-including-a-colon-quoted" names="a\\ target\\ name:\\ including\\ a\\ colon\\ (quoted)">
    <target ids="a-target-name-including-a-colon-escaped" names="a\\ target\\ name:\\ including\\ a\\ colon\\ (escaped)">
"""],
["""\
.. _`target: No matching backquote.
.. _`: No matching backquote either.
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
        _`target: No matching backquote.
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            malformed hyperlink target.
    <comment xml:space="preserve">
        _`: No matching backquote either.
    <system_message level="2" line="2" source="test data" type="WARNING">
        <paragraph>
            malformed hyperlink target.
"""],
["""\
.. _a very long target name,
   split across lines:
.. _`and another,
   with backquotes`:
""",
"""\
<document source="test data">
    <target ids="a-very-long-target-name-split-across-lines" names="a\\ very\\ long\\ target\\ name,\\ split\\ across\\ lines">
    <target ids="and-another-with-backquotes" names="and\\ another,\\ with\\ backquotes">
"""],
["""\
External hyperlink:

.. _target: http://www.python.org/
""",
"""\
<document source="test data">
    <paragraph>
        External hyperlink:
    <target ids="target" names="target" refuri="http://www.python.org/">
"""],
["""\
.. _email: jdoe@example.com

.. _multi-line email: jdoe
   @example.com
""",
"""\
<document source="test data">
    <target ids="email" names="email" refuri="mailto:jdoe@example.com">
    <target ids="multi-line-email" names="multi-line\\ email" refuri="mailto:jdoe@example.com">
"""],
["""\
Malformed target:

.. __malformed: no good

Target beginning with an underscore:

.. _`_target`: OK
""",
"""\
<document source="test data">
    <paragraph>
        Malformed target:
    <comment xml:space="preserve">
        __malformed: no good
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            malformed hyperlink target.
    <paragraph>
        Target beginning with an underscore:
    <target ids="target" names="_target" refuri="OK">
"""],
["""\
Duplicate external targets (different URIs):

.. _target: first

.. _target: second
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external targets (different URIs):
    <target dupnames="target" ids="target" refuri="first">
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "target".
    <target dupnames="target" ids="target-1" refuri="second">
"""],
["""\
Duplicate external targets (same URIs):

.. _target: first

.. _target: first
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external targets (same URIs):
    <target ids="target" names="target" refuri="first">
    <system_message level="1" line="5" source="test data" type="INFO">
        <paragraph>
            Duplicate name "target" for external target "first".
    <target dupnames="target" ids="target-1" refuri="first">
"""],
["""\
Duplicate external targets (embedded/explicit, same URIs):

See the `example <example.rst>`_

See the example_

.. _example: example.rst
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate external targets (embedded/explicit, same URIs):
    <paragraph>
        See the \n\
        <reference name="example" refuri="example.rst">
            example
        <target ids="example" names="example" refuri="example.rst">
    <paragraph>
        See the \n\
        <reference name="example" refname="example">
            example
    <system_message level="1" line="7" source="test data" type="INFO">
        <paragraph>
            Duplicate name "example" for external target "example.rst".
    <target dupnames="example" ids="example-1" refuri="example.rst">
"""],
["""\
Duplicate indirect _`targets` (same refname):

.. _link: targets_

.. _link: targets_

do not conflict. The reference name can be used in a link_.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate indirect \n\
        <target ids="targets" names="targets">
            targets
         (same refname):
    <target ids="link" names="link" refname="targets">
    <system_message backrefs="link-1" level="1" line="5" source="test data" type="INFO">
        <paragraph>
            Duplicate name "link" for external target "targets".
    <target dupnames="link" ids="link-1" refname="targets">
    <paragraph>
        do not conflict. The reference name can be used in a \n\
        <reference name="link" refname="link">
            link
        .
"""],
["""\
Duplicate implicit targets.

Title
=====

Paragraph.

Title
=====

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit targets.
    <section dupnames="title" ids="title">
        <title>
            Title
        <paragraph>
            Paragraph.
    <section dupnames="title" ids="title-1">
        <title>
            Title
        <system_message backrefs="title-1" level="1" line="9" source="test data" type="INFO">
            <paragraph>
                Duplicate implicit target name: "title".
        <paragraph>
            Paragraph.
"""],
["""\
Duplicate implicit/explicit targets.

Title
=====

.. _title:

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit/explicit targets.
    <section dupnames="title" ids="title">
        <title>
            Title
        <system_message backrefs="title-1" level="1" line="6" source="test data" type="INFO">
            <paragraph>
                Target name overrides implicit target name "title".
        <target ids="title-1" names="title">
        <paragraph>
            Paragraph.
"""],
["""\
Duplicate implicit/directive targets.

Title
=====

.. note:: remember remember
   :name: title
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate implicit/directive targets.
    <section dupnames="title" ids="title">
        <title>
            Title
        <note ids="title-1" names="title">
            <system_message backrefs="title-1" level="1" line="7" source="test data" type="INFO">
                <paragraph>
                    Target name overrides implicit target name "title".
            <paragraph>
                remember remember
"""],
["""\
Duplicate explicit targets.

.. _title:

First.

.. _title:

Second.

.. _title:

Third.
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate explicit targets.
    <target dupnames="title" ids="title">
    <paragraph>
        First.
    <system_message backrefs="title-1" level="2" line="7" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "title".
    <target dupnames="title" ids="title-1">
    <paragraph>
        Second.
    <system_message backrefs="title-2" level="2" line="11" source="test data" type="WARNING">
        <paragraph>
            Duplicate explicit target name: "title".
    <target dupnames="title" ids="title-2">
    <paragraph>
        Third.
"""],
["""\
Duplicate explicit/directive targets.

.. _title:

First.

.. rubric:: this is a title too
   :name: title

The system message is left dangling
(to be handled by the "universal.Messages" transform).
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate explicit/directive targets.
    <target dupnames="title" ids="title">
    <paragraph>
        First.
    <rubric dupnames="title" ids="title-1">
        this is a title too
    <paragraph>
        The system message is left dangling
        (to be handled by the "universal.Messages" transform).
"""],
["""\
Duplicate targets:

Target
======

Implicit section header target.

.. [TARGET] Citation target.

.. [#target] Autonumber-labeled footnote target.

.. _target:

Explicit internal target.

.. _target: Explicit_external_target

| Do not insert <system_message> element for duplicate
| _`target`, if this results in an invalid doctree.

.. rubric:: directive with target
   :name: Target

:field list: with
:_`target`: in a field name
""",
"""\
<document source="test data">
    <paragraph>
        Duplicate targets:
    <section dupnames="target" ids="target">
        <title>
            Target
        <paragraph>
            Implicit section header target.
        <citation dupnames="target" ids="target-1">
            <label>
                TARGET
            <system_message backrefs="target-1" level="1" line="8" source="test data" type="INFO">
                <paragraph>
                    Target name overrides implicit target name "target".
            <paragraph>
                Citation target.
        <footnote auto="1" dupnames="target" ids="target-2">
            <system_message backrefs="target-2" level="2" line="10" source="test data" type="WARNING">
                <paragraph>
                    Duplicate explicit target name: "target".
            <paragraph>
                Autonumber-labeled footnote target.
        <system_message backrefs="target-3" level="2" line="12" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "target".
        <target dupnames="target" ids="target-3">
        <paragraph>
            Explicit internal target.
        <system_message level="2" line="16" source="test data" type="WARNING">
            <paragraph>
                Duplicate explicit target name: "target".
        <target dupnames="target" ids="target-4" refuri="Explicit_external_target">
        <line_block>
            <line>
                Do not insert <system_message> element for duplicate
            <line>
                <target dupnames="target" ids="target-5">
                    target
                , if this results in an invalid doctree.
        <rubric dupnames="target" ids="target-6">
            directive with target
        <field_list>
            <field>
                <field_name>
                    field list
                <field_body>
                    <paragraph>
                        with
            <field>
                <field_name>
                    <target dupnames="target" ids="target-7">
                        target
                <field_body>
                    <paragraph>
                        in a field name
"""],
["""\
.. _unescaped colon at end:: no good

.. _:: no good either

.. _escaped colon\\:: OK

.. _`unescaped colon, quoted:`: OK
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
        _unescaped colon at end:: no good
    <system_message level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            malformed hyperlink target.
    <comment xml:space="preserve">
        _:: no good either
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            malformed hyperlink target.
    <target ids="escaped-colon" names="escaped\\ colon:" refuri="OK">
    <target ids="unescaped-colon-quoted" names="unescaped\\ colon,\\ quoted:" refuri="OK">
"""],
]

totest['anonymous_targets'] = [
["""\
Anonymous external hyperlink target:

.. __: http://w3c.org/
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous external hyperlink target:
    <target anonymous="1" ids="target-1" refuri="http://w3c.org/">
"""],
["""\
Anonymous external hyperlink target:

__ http://w3c.org/
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous external hyperlink target:
    <target anonymous="1" ids="target-1" refuri="http://w3c.org/">
"""],
["""\
Anonymous indirect hyperlink target:

.. __: reference_
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous indirect hyperlink target:
    <target anonymous="1" ids="target-1" refname="reference">
"""],
["""\
Anonymous external hyperlink target, not indirect:

__ uri\\_

__ this URI ends with an underscore_
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous external hyperlink target, not indirect:
    <target anonymous="1" ids="target-1" refuri="uri_">
    <target anonymous="1" ids="target-2" refuri="thisURIendswithanunderscore_">
"""],
["""\
Anonymous indirect hyperlink targets:

__ reference_
__ `a very long
   reference`_
""",
"""\
<document source="test data">
    <paragraph>
        Anonymous indirect hyperlink targets:
    <target anonymous="1" ids="target-1" refname="reference">
    <target anonymous="1" ids="target-2" refname="a very long reference">
"""],
["""\
Mixed anonymous & named indirect hyperlink targets:

__ reference_
.. __: reference_
__ reference_
.. _target1: reference_
no blank line

.. _target2: reference_
__ reference_
.. __: reference_
__ reference_
no blank line
""",
"""\
<document source="test data">
    <paragraph>
        Mixed anonymous & named indirect hyperlink targets:
    <target anonymous="1" ids="target-1" refname="reference">
    <target anonymous="1" ids="target-2" refname="reference">
    <target anonymous="1" ids="target-3" refname="reference">
    <target ids="target1" names="target1" refname="reference">
    <system_message level="2" line="7" source="test data" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
    <target ids="target2" names="target2" refname="reference">
    <target anonymous="1" ids="target-4" refname="reference">
    <target anonymous="1" ids="target-5" refname="reference">
    <target anonymous="1" ids="target-6" refname="reference">
    <system_message level="2" line="13" source="test data" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
"""],
["""\
.. _
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
        _
"""],
]


if __name__ == '__main__':
    unittest.main()
