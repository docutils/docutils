#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for parts.py contents directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['contents'] = [
["""\
.. contents::
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               title: None
"""],
["""\
.. contents:: Table of Contents
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               title:
                 <title>
                     Table of Contents
"""],
["""\
.. contents::
   Table of Contents
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               title:
                 <title>
                     Table of Contents
"""],
["""\
.. contents:: Table
   of
   Contents
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               title:
                 <title>
                     Table of Contents
"""],
["""\
.. contents:: *Table* of ``Contents``
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               title:
                 <title>
                     <emphasis>
                         Table
                      of \n\
                     <literal>
                         Contents
"""],
["""\
.. contents::
   :depth: 2
   :local:
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               depth: 2
               local: None
               title: None
"""],
["""\
.. contents:: Table of Contents
   :local:
   :depth: 2
   :backlinks: none
""",
"""\
<document source="test data">
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.parts.Contents
             .stage: 'first writer'
             .details:
               backlinks: None
               depth: 2
               local: None
               title:
                 <title>
                     Table of Contents
"""],
["""\
.. contents::
   :depth: two
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive options:
            invalid option value: (option: "depth"; value: 'two')
            invalid literal for int(): two.
        <literal_block xml:space="1">
            .. contents::
               :depth: two
"""],
["""\
.. contents::
   :width: 2
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive options:
            unknown option: "width".
        <literal_block xml:space="1">
            .. contents::
               :width: 2
"""],
["""\
.. contents::
   :backlinks: no way!
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive options:
            invalid option value: (option: "backlinks"; value: 'no way!')
            "no way!" unknown; choose from "top", "entry", or "none".
        <literal_block xml:space="1">
            .. contents::
               :backlinks: no way!
"""],
["""\
.. contents::
   :backlinks:
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "contents" directive options:
            invalid option value: (option: "backlinks"; value: None)
            must supply an argument; choose from "top", "entry", or "none".
        <literal_block xml:space="1">
            .. contents::
               :backlinks:
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
