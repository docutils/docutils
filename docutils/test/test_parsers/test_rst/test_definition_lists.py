#! /usr/bin/env python3

# $Id$
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

if __name__ == '__main__':
    import __init__  # noqa: F401
from test import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s


totest = {}

totest['definition_lists'] = [
["""\
term
  definition
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term
            <definition>
                <paragraph>
                    definition
"""],
["""\
term
  definition

paragraph
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term
            <definition>
                <paragraph>
                    definition
    <paragraph>
        paragraph
"""],
["""\
term
  definition
no blank line
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term
            <definition>
                <paragraph>
                    definition
    <system_message level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Definition list ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
"""],
["""\
A paragraph::
    A literal block without a blank line first?
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                A paragraph::
            <definition>
                <system_message level="1" line="2" source="test data" type="INFO">
                    <paragraph>
                        Blank line missing before literal block (after the "::")? Interpreted as a definition list item.
                <paragraph>
                    A literal block without a blank line first?
"""],
["""\
this is not a term;
a term may only be one line long
  this is not a definition
""",
"""\
<document source="test data">
    <paragraph>
        this is not a term;
        a term may only be one line long
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Unexpected indentation.
    <block_quote>
        <paragraph>
            this is not a definition
"""],
["""\
term 1
  definition 1

term 2
  definition 2
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term 1
            <definition>
                <paragraph>
                    definition 1
        <definition_list_item>
            <term>
                term 2
            <definition>
                <paragraph>
                    definition 2
"""],
["""\
term 1
  definition 1 (no blank line below)
term 2
  definition 2
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term 1
            <definition>
                <paragraph>
                    definition 1 (no blank line below)
        <definition_list_item>
            <term>
                term 2
            <definition>
                <paragraph>
                    definition 2
"""],
["""\
term 1
  definition 1 (no blank line below)
term 2
  definition 2
No blank line after the definition list.
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term 1
            <definition>
                <paragraph>
                    definition 1 (no blank line below)
        <definition_list_item>
            <term>
                term 2
            <definition>
                <paragraph>
                    definition 2
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Definition list ends without a blank line; unexpected unindent.
    <paragraph>
        No blank line after the definition list.
"""],
["""\
term 1
  definition 1

  term 1a
    definition 1a

  term 1b
    definition 1b

term 2
  definition 2

paragraph
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                term 1
            <definition>
                <paragraph>
                    definition 1
                <definition_list>
                    <definition_list_item>
                        <term>
                            term 1a
                        <definition>
                            <paragraph>
                                definition 1a
                    <definition_list_item>
                        <term>
                            term 1b
                        <definition>
                            <paragraph>
                                definition 1b
        <definition_list_item>
            <term>
                term 2
            <definition>
                <paragraph>
                    definition 2
    <paragraph>
        paragraph
"""],
["""\
Term : classifier
    The ' : ' indicates a classifier in
    definition list item terms only.
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                Term
            <classifier>
                classifier
            <definition>
                <paragraph>
                    The ' : ' indicates a classifier in
                    definition list item terms only.
"""],
["""\
Term: not a classifier
    Because there's no space before the colon.
Term :not a classifier
    Because there's no space after the colon.
Term \\: not a classifier
    Because the colon is escaped.
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                Term: not a classifier
            <definition>
                <paragraph>
                    Because there's no space before the colon.
        <definition_list_item>
            <term>
                Term :not a classifier
            <definition>
                <paragraph>
                    Because there's no space after the colon.
        <definition_list_item>
            <term>
                Term : not a classifier
            <definition>
                <paragraph>
                    Because the colon is escaped.
"""],
["""\
``Term : not a classifier``
    Because the ' : ' is inside an inline literal.
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                <literal>
                    Term : not a classifier
            <definition>
                <paragraph>
                    Because the ' : ' is inside an inline literal.
"""],
["""\
Term `with *inline ``text **errors : classifier `with *errors ``too
    Definition `with *inline ``text **markup errors.
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                Term \n\
                <problematic ids="problematic-1" refid="system-message-1">
                    `
                with \n\
                <problematic ids="problematic-2" refid="system-message-2">
                    *
                inline \n\
                <problematic ids="problematic-3" refid="system-message-3">
                    ``
                text \n\
                <problematic ids="problematic-4" refid="system-message-4">
                    **
                errors
            <classifier>
                classifier \n\
                <problematic ids="problematic-5" refid="system-message-5">
                    `
                with \n\
                <problematic ids="problematic-6" refid="system-message-6">
                    *
                errors \n\
                <problematic ids="problematic-7" refid="system-message-7">
                    ``
                too
            <definition>
                <system_message backrefs="problematic-1" ids="system-message-1" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline interpreted text or phrase reference start-string without end-string.
                <system_message backrefs="problematic-2" ids="system-message-2" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline emphasis start-string without end-string.
                <system_message backrefs="problematic-3" ids="system-message-3" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline literal start-string without end-string.
                <system_message backrefs="problematic-4" ids="system-message-4" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline strong start-string without end-string.
                <system_message backrefs="problematic-5" ids="system-message-5" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline interpreted text or phrase reference start-string without end-string.
                <system_message backrefs="problematic-6" ids="system-message-6" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline emphasis start-string without end-string.
                <system_message backrefs="problematic-7" ids="system-message-7" level="2" line="1" source="test data" type="WARNING">
                    <paragraph>
                        Inline literal start-string without end-string.
                <paragraph>
                    Definition \n\
                    <problematic ids="problematic-8" refid="system-message-8">
                        `
                    with \n\
                    <problematic ids="problematic-9" refid="system-message-9">
                        *
                    inline \n\
                    <problematic ids="problematic-10" refid="system-message-10">
                        ``
                    text \n\
                    <problematic ids="problematic-11" refid="system-message-11">
                        **
                    markup errors.
                <system_message backrefs="problematic-8" ids="system-message-8" level="2" line="2" source="test data" type="WARNING">
                    <paragraph>
                        Inline interpreted text or phrase reference start-string without end-string.
                <system_message backrefs="problematic-9" ids="system-message-9" level="2" line="2" source="test data" type="WARNING">
                    <paragraph>
                        Inline emphasis start-string without end-string.
                <system_message backrefs="problematic-10" ids="system-message-10" level="2" line="2" source="test data" type="WARNING">
                    <paragraph>
                        Inline literal start-string without end-string.
                <system_message backrefs="problematic-11" ids="system-message-11" level="2" line="2" source="test data" type="WARNING">
                    <paragraph>
                        Inline strong start-string without end-string.
"""],
["""\
Term : `reference`_
    classifier starting with a reference crashes from release 8197 to ...
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                Term
            <classifier>
                <reference name="reference" refname="reference">
                    reference
            <definition>
                <paragraph>
                    classifier starting with a reference crashes from release 8197 to ...
"""],
["""\
Term : a `reference`_ in text : second
    classifier with reference crashes from release 8197 to ...
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                Term
            <classifier>
                a \n\
                <reference name="reference" refname="reference">
                    reference
                 in text
            <classifier>
                second
            <definition>
                <paragraph>
                    classifier with reference crashes from release 8197 to ...
"""],
["""\
Term : classifier one  :  classifier two
    Definition
""",
"""\
<document source="test data">
    <definition_list>
        <definition_list_item>
            <term>
                Term
            <classifier>
                classifier one
            <classifier>
                classifier two
            <definition>
                <paragraph>
                    Definition
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
