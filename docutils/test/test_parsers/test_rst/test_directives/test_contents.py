#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

Tests for components.py contents directives.
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
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
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
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
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
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
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
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
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
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
             .details:
               title:
                 <title>
                     <emphasis>
                         Table
                      of 
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
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
             .details:
               depth: 2
               local: None
               title: None
"""],
["""\
.. contents:: Table of Contents
   :local:
   :depth: 2
""",
"""\
<document>
    <pending>
        .. internal attributes:
             .transform: docutils.transforms.components.Contents
             .stage: 'last_reader'
             .details:
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
            invalid attribute value:
            (attribute "depth", value "'two'") invalid literal for int(): two.
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
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
