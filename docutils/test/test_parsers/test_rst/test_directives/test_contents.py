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
<document>
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
<document>
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
<document>
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
<document>
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
<document>
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
<document>
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
<document>
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
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Error in "contents" directive attributes at line 1:
            invalid attribute value: (attribute: "depth"; value: 'two')
            invalid literal for int(): two.
        <literal_block>
            .. contents::
               :depth: two
"""],
["""\
.. contents::
   :width: 2
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Error in "contents" directive attributes at line 1:
            unknown attribute: "width".
        <literal_block>
            .. contents::
               :width: 2
"""],
["""\
.. contents::
   :backlinks: no way!
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Error in "contents" directive attributes at line 1:
            invalid attribute value: (attribute: "backlinks"; value: 'no way!')
            "no way!" unknown; choose from "top", "entry", or "none".
        <literal_block>
            .. contents::
               :backlinks: no way!
"""],
["""\
.. contents::
   :backlinks:
""",
"""\
<document>
    <system_message level="3" type="ERROR">
        <paragraph>
            Error in "contents" directive attributes at line 1:
            invalid attribute value: (attribute: "backlinks"; value: None)
            must supply an argument; choose from "top", "entry", or "none".
        <literal_block>
            .. contents::
               :backlinks:
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
